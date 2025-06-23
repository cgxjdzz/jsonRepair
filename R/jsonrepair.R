# jsonrepair.R

# Ensure R6 package is available
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}

# Define JsonContext R6 Class
# This class manages the parsing context (e.g., inside an object key, value, or array)
JsonContext <- R6::R6Class("JsonContext",
                           public = list(
                             context = list(), # Stack to store context values

                             #' @description
                             #' Initializes the JsonContext.
                             initialize = function() {
                               self$context <- list()
                             },

                             #' @description
                             #' Sets a new context value by pushing it onto the stack.
                             #' @param value The context value to set (e.g., "OBJECT_KEY", "OBJECT_VALUE", "ARRAY").
                             set = function(value) {
                               self$context <- c(self$context, list(value))
                             },

                             #' @description
                             #' Resets the context by popping the last value from the stack.
                             reset = function() {
                               if (length(self$context) > 0) {
                                 self$context <- self$context[-length(self$context)]
                               }
                             },

                             #' @description
                             #' Prints the current context for debugging.
                             #' @param ... Additional arguments for print.
                             print = function(...) {
                               cat("JsonContext: ", paste(self$context, collapse = " -> "), "\n")
                             }
                           ),
                           active = list(
                             #' @field current
                             #' Gets the current (topmost) context value. Returns NULL if stack is empty.
                             current = function() {
                               if (length(self$context) > 0) {
                                 return(self$context[[length(self$context)]])
                               }
                               NULL
                             },

                             #' @field empty
                             #' Checks if the context stack is empty.
                             empty = function() {
                               length(self$context) == 0
                             }
                           )
)

# Define ContextValues for better readability
ContextValues <- list(
  OBJECT_KEY = "OBJECT_KEY",
  OBJECT_VALUE = "OBJECT_VALUE",
  ARRAY = "ARRAY"
)

# Define JSONParser R6 Class
# This class implements the core JSON parsing and repair logic
JSONParser <- R6::R6Class("JSONParser",
                          public = list(
                            json_str = NULL,
                            index = 0,
                            context = NULL,
                            logging = FALSE,
                            logger = list(),
                            stream_stable = FALSE,

                            # Constants
                            STRING_DELIMITERS = c('"', "'", "“", "”"),
                            NUMBER_CHARS = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", ".", "e", "E", "/", ","),

                            #' @description
                            #' Initializes the JSONParser.
                            #' @param json_str The input JSON string to parse and repair.
                            #' @param logging Logical, whether to enable logging of repair activities.
                            #' @param stream_stable Logical, whether to keep repair results stable for streaming JSON.
                            initialize = function(json_str, logging = FALSE, stream_stable = FALSE) {
                              self$json_str <- json_str
                              self$index <- 0
                              self$context <- JsonContext$new()
                              self$logging <- logging
                              self$stream_stable <- stream_stable
                              if (logging) {
                                self$logger <- list()
                                self$log <- self$`_log` # Assign the internal log function
                              } else {
                                self$log <- function(...) {} # No-op logging function
                              }
                            },

                            #' @description
                            #' Main parsing function.
                            #' @return The repaired JSON object (list, vector, string, number, logical, or NULL),
                            #'   or a list containing the repaired JSON and the log if logging is enabled.
                            parse = function() {
                              json_result <- self$parse_json()

                              # Handle multiple JSON elements in the string
                              if (self$index < nchar(self$json_str)) {
                                self$log("The parser returned early, checking if there's more json elements")
                                json_list <- list(json_result)
                                while (self$index < nchar(self$json_str)) {
                                  j <- self$parse_json()
                                  if (nchar(j) > 0) { # Check if something meaningful was parsed
                                    # If the new object seems to be an update of the last one, replace
                                    # This is a simplified comparison, can be expanded if needed.
                                    if (length(json_list) > 0 && private$is_same_object(json_list[[length(json_list)]], j)) {
                                      json_list[[length(json_list)]] <- j # Replace last entry
                                    } else {
                                      json_list <- c(json_list, list(j)) # Append new entry
                                    }
                                  } else {
                                    # This was a bust, move the index to avoid infinite loops on invalid chars
                                    self$index <- self$index + 1
                                  }
                                }
                                # If nothing extra was found, don't return a list
                                if (length(json_list) == 1) {
                                  self$log("There were no more elements, returning the element without the array")
                                  json_result <- json_list[[1]]
                                } else {
                                  json_result <- json_list
                                }
                              }

                              if (self$logging) {
                                return(list(json = json_result, log = self$logger))
                              } else {
                                return(json_result)
                              }
                            },

                            #' @description
                            #' Parses any JSON element (object, array, string, number, boolean, null, comment).
                            #' @return The parsed JSON element.
                            parse_json = function() {
                              while (TRUE) {
                                char <- self$get_char_at()
                                if (isFALSE(char)) { # False means end of string
                                  return("")
                                } else if (char == "{") {
                                  self$index <- self$index + 1
                                  return(self$parse_object())
                                } else if (char == "[") {
                                  self$index <- self$index + 1
                                  return(self$parse_array())
                                } else if (self$context$current == ContextValues$OBJECT_VALUE && char == "}") {
                                  # Edge case: key with missing value at object end
                                  self$log("At the end of an object we found a key with missing value, skipping")
                                  return("")
                                } else if (!self$context$empty && (char %in% self$STRING_DELIMITERS || grepl("[[:alpha:]]", char))) {
                                  return(self$parse_string())
                                } else if (!self$context$empty && (grepl("[[:digit:]]", char) || char == "-" || char == ".")) {
                                  return(self$parse_number())
                                } else if (char %in% c("#", "/")) {
                                  return(self$parse_comment())
                                } else {
                                  # Ignore and move on for unhandled characters
                                  self$index <- self$index + 1
                                }
                              }
                            },

                            #' @description
                            #' Parses a JSON object.
                            #' @return A named list (R's equivalent of a dictionary).
                            parse_object = function() {
                              obj <- list()
                              # Stop when closing brace '}' is found or end of string
                              while (isFALSE(self$get_char_at()) || self$get_char_at() != "}") {
                                self$skip_whitespaces_at()

                                # Handle stray ':' before a key
                                if (self$get_char_at() == ":") {
                                  self$log("While parsing an object we found a : before a key, ignoring")
                                  self$index <- self$index + 1
                                }

                                self$context$set(ContextValues$OBJECT_KEY)
                                rollback_index <- self$index
                                key <- ""

                                while (isTRUE(self$get_char_at())) {
                                  rollback_index <- self$index
                                  if (self$get_char_at() == "[" && nchar(key) == 0) {
                                    # Check if this is an array merge (e.g., {"key": ["val1", "val2"]} ["val3"])
                                    prev_key <- if (length(obj) > 0) tail(names(obj), 1)[[1]] else NULL
                                    if (!is.null(prev_key) && is.list(obj[[prev_key]])) { # Check if prev value is a list (array)
                                      self$index <- self$index + 1
                                      new_array <- self$parse_array()
                                      if (is.list(new_array)) {
                                        # Merge and flatten if new_array is a single nested list
                                        if (length(new_array) == 1 && is.list(new_array[[1]])) {
                                          obj[[prev_key]] <- c(obj[[prev_key]], new_array[[1]])
                                        } else {
                                          obj[[prev_key]] <- c(obj[[prev_key]], new_array)
                                        }
                                      }
                                      self$skip_whitespaces_at()
                                      if (self$get_char_at() == ",") {
                                        self$index <- self$index + 1
                                      }
                                      self$skip_whitespaces_at()
                                      next # Continue parsing the object
                                    }
                                  }

                                  key <- as.character(self$parse_string())
                                  if (nchar(key) == 0) {
                                    self$skip_whitespaces_at()
                                  }
                                  # If the string is empty but there's a colon or closing brace, we're done with the key
                                  if (nchar(key) > 0 || (nchar(key) == 0 && (self$get_char_at() == ":" || self$get_char_at() == "}"))) {
                                    break
                                  }
                                }

                                # Handle duplicate keys, especially in array contexts
                                if (ContextValues$ARRAY %in% self$context$context && key %in% names(obj)) {
                                  self$log("While parsing an object we found a duplicate key, closing the object here and rolling back the index")
                                  self$index <- rollback_index - 1
                                  # Insert a '{' to correct the structure for the next parse_json call (if any)
                                  self$json_str <- paste0(substr(self$json_str, 1, self$index), "{", substr(self$json_str, self$index + 1, nchar(self$json_str)))
                                  break
                                }

                                self$skip_whitespaces_at()

                                if (isFALSE(self$get_char_at()) || self$get_char_at() == "}") {
                                  next # No value after key, just close object or continue
                                }

                                self$skip_whitespaces_at()

                                # Handle missing ':' after a key
                                if (self$get_char_at() != ":") {
                                  self$log("While parsing an object we missed a : after a key")
                                }

                                self$index <- self$index + 1
                                self$context$reset() # Pop OBJECT_KEY
                                self$context$set(ContextValues$OBJECT_VALUE)

                                value <- self$parse_json()

                                self$context$reset() # Pop OBJECT_VALUE

                                obj[[key]] <- value

                                if (self$get_char_at() %in% c(",", "'", '"')) {
                                  self$index <- self$index + 1
                                }
                                self$skip_whitespaces_at()
                              }

                              self$index <- self$index + 1 # Move past '}'
                              return(obj)
                            },

                            #' @description
                            #' Parses a JSON array.
                            #' @return A list (R's equivalent of an array).
                            parse_array = function() {
                              arr <- list()
                              self$context$set(ContextValues$ARRAY)

                              char <- self$get_char_at()
                              while (isTRUE(char) && !(char %in% c("]", "}"))) {
                                self$skip_whitespaces_at()
                                value <- ""

                                # Special handling for strings that might be unquoted objects
                                if (char %in% self$STRING_DELIMITERS) {
                                  # Look ahead to see if ':' follows, suggesting it's actually an object
                                  i <- 1
                                  i <- self$skip_to_character(char, i) # Skip to closing quote of potential key
                                  i <- self$skip_whitespaces_at(idx = i + 1, move_main_index = FALSE) # Skip spaces after potential key
                                  value <- if (self$get_char_at(i) == ":") self$parse_object() else self$parse_string()
                                } else {
                                  value <- self$parse_json()
                                }

                                if (nchar(value) == 0) { # If parse_json returned nothing valid
                                  self$index <- self$index + 1
                                } else if (value == "..." && self$get_char_at(-1) == ".") { # Handle stray "..."
                                  self$log("While parsing an array, found a stray '...'; ignoring it")
                                } else {
                                  arr <- c(arr, list(value)) # Append to array
                                }

                                # Skip whitespace and commas after a value but before closing ']'
                                char <- self$get_char_at()
                                while (isTRUE(char) && char != "]" && (grepl("[[:space:]]", char) || char == ",")) {
                                  self$index <- self$index + 1
                                  char <- self$get_char_at()
                                }
                              }

                              # Handle missing ']' at the end of an array
                              if (isTRUE(char) && char != "]") {
                                self$log("While parsing an array we missed the closing ], ignoring it")
                              }

                              self$index <- self$index + 1 # Move past ']'
                              self$context$reset() # Pop ARRAY
                              return(arr)
                            },

                            #' @description
                            #' Parses a JSON string, handling quotes, escape sequences, and unquoted literals.
                            #' @return The parsed string.
                            parse_string = function() {
                              missing_quotes <- FALSE
                              doubled_quotes <- FALSE
                              lstring_delimiter <- '"'
                              rstring_delimiter <- '"'

                              char <- self$get_char_at()

                              # Handle leading comments before a string
                              if (char %in% c("#", "/")) {
                                return(self$parse_comment())
                              }

                              # Skip non-alphanumeric/non-delimiter leading characters
                              while (isTRUE(char) && !(char %in% self$STRING_DELIMITERS) && !grepl("[[:alnum:]]", char)) {
                                self$index <- self$index + 1
                                char <- self$get_char_at()
                              }

                              if (isFALSE(char)) {
                                return("") # Empty string if nothing found
                              }

                              # Determine delimiter and check for missing quotes
                              if (char == "'") {
                                lstring_delimiter <- rstring_delimiter <- "'"
                              } else if (char == "“") {
                                lstring_delimiter <- "“"
                                rstring_delimiter <- "”"
                              } else if (grepl("[[:alnum:]]", char)) {
                                # Could be a boolean/null or unquoted string
                                if (tolower(char) %in% c("t", "f", "n") && self$context$current != ContextValues$OBJECT_KEY) {
                                  value <- self$parse_boolean_or_null()
                                  if (nchar(value) > 0 || is.logical(value) || is.null(value)) { # Check for non-empty/non-falsey return
                                    return(value)
                                  }
                                }
                                self$log("While parsing a string, we found a literal instead of a quote")
                                missing_quotes <- TRUE
                              }

                              if (!missing_quotes) {
                                self$index <- self$index + 1 # Move past opening quote
                              }

                              # Handle doubled quotes (e.g., """string""")
                              if (self$get_char_at() %in% self$STRING_DELIMITERS && self$get_char_at() == lstring_delimiter) {
                                if (self$context$current == ContextValues$OBJECT_KEY && self$get_char_at(1) == ":") {
                                  self$index <- self$index + 1
                                  return("") # Empty key
                                }
                                if (self$get_char_at(1) == lstring_delimiter) { # Found "" at start
                                  self$log("While parsing a string, we found a doubled quote and then a quote again, ignoring it")
                                  return("")
                                }
                                i <- self$skip_to_character(character = rstring_delimiter, idx = 1)
                                next_c <- self$get_char_at(i)
                                if (isTRUE(next_c) && (self$get_char_at(i + 1) == rstring_delimiter || isFALSE(self$get_char_at(i+1)))) { # Check for closing "" or end of string after first "
                                  self$log("While parsing a string, we found a valid starting doubled quote")
                                  doubled_quotes <- TRUE
                                  self$index <- self$index + 1 # Skip first char of doubled quote
                                } else {
                                  i <- self$skip_whitespaces_at(idx = 1, move_main_index = FALSE)
                                  next_c <- self$get_char_at(i)
                                  if (isTRUE(next_c) && next_c %in% c(self$STRING_DELIMITERS, "{", "[")) {
                                    self$log("While parsing a string, we found a doubled quote but also another quote afterwards, ignoring it")
                                    self$index <- self$index + 1
                                    return("")
                                  } else if (isTRUE(next_c) && !(next_c %in% c(",", "]", "}"))) {
                                    self$log("While parsing a string, we found a doubled quote but it was a mistake, removing one quote")
                                    self$index <- self$index + 1
                                  }
                                }
                              }

                              string_acc <- ""
                              char <- self$get_char_at()
                              unmatched_delimiter <- FALSE

                              while (isTRUE(char) && char != rstring_delimiter) {
                                # Break conditions for missing quotes
                                if (missing_quotes && self$context$current == ContextValues$OBJECT_KEY && (char == ":" || grepl("[[:space:]]", char))) {
                                  self$log("While parsing a string missing the left delimiter in object key context, we found a :, stopping here")
                                  break
                                }
                                if (!self$stream_stable && self$context$current == ContextValues$OBJECT_VALUE && char %in% c(",", "}") && nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) != rstring_delimiter) {
                                  rstring_delimiter_missing <- TRUE
                                  i <- self$skip_to_character(character = rstring_delimiter, idx = 1)
                                  next_c <- self$get_char_at(i)

                                  if (isTRUE(next_c)) {
                                    i <- i + 1
                                    i <- self$skip_whitespaces_at(idx = i, move_main_index = FALSE)
                                    next_c <- self$get_char_at(i)
                                    if (isFALSE(next_c) || next_c %in% c(",", "}")) {
                                      rstring_delimiter_missing <- FALSE
                                    } else {
                                      # Check for a new lstring_delimiter
                                      i <- self$skip_to_character(character = lstring_delimiter, idx = i)
                                      next_c <- self$get_char_at(i)
                                      if (isTRUE(next_c)) {
                                        # Check for ':' after (skip spaces)
                                        i <- self$skip_whitespaces_at(idx = i + 1, move_main_index = FALSE)
                                        next_c <- self$get_char_at(i)
                                        if (isTRUE(next_c) && next_c == ":") {
                                          rstring_delimiter_missing <- FALSE
                                        }
                                      }
                                    }
                                  } else {
                                    # Could be a missing key:value pair
                                    i <- self$skip_to_character(character = ":", idx = 1)
                                    next_c <- self$get_char_at(i)
                                    if (isTRUE(next_c)) {
                                      break # Systemic issue, stop here
                                    } else {
                                      i <- self$skip_whitespaces_at(idx = 1, move_main_index = FALSE)
                                      j <- self$skip_to_character(character = "}", idx = i)
                                      if (isTRUE(self$get_char_at(j)) && (j - i <= 1 || grepl("\\{", string_acc))) {
                                        rstring_delimiter_missing <- FALSE # Is last char before closing brace or has unmatched brace
                                      }
                                    }
                                  }
                                  if (rstring_delimiter_missing) {
                                    self$log("While parsing a string missing the left delimiter in object value context, we found a , or } and we couldn't determine that a right delimiter was present. Stopping here")
                                    break
                                  }
                                }
                                if (!self$stream_stable && char == "]" && ContextValues$ARRAY %in% self$context$context && nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) != rstring_delimiter) {
                                  i <- self$skip_to_character(rstring_delimiter)
                                  if (isFALSE(self$get_char_at(i))) {
                                    break # No delimiter found, end early
                                  }
                                }

                                string_acc <- paste0(string_acc, char)
                                self$index <- self$index + 1
                                char <- self$get_char_at()

                                # Handle escape sequences
                                if (isTRUE(char) && nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\\") {
                                  self$log("Found a stray escape sequence, normalizing it")
                                  if (char %in% c(rstring_delimiter, "t", "n", "r", "b", "\\")) {
                                    string_acc <- substr(string_acc, 1, nchar(string_acc) - 1) # Remove the backslash
                                    escape_map <- list(t = "\t", n = "\n", r = "\r", b = "\b")
                                    string_acc <- paste0(string_acc, escape_map[[char]] %||% char)
                                    self$index <- self$index + 1
                                    char <- self$get_char_at()
                                    # Handle multiple backslashes
                                    while (isTRUE(char) && nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\\" && char %in% c(rstring_delimiter, "\\")) {
                                      string_acc <- substr(string_acc, 1, nchar(string_acc) - 1)
                                      string_acc <- paste0(string_acc, char)
                                      self$index <- self$index + 1
                                      char <- self$get_char_at()
                                    }
                                    next
                                  } else if (char %in% c("u", "x")) {
                                    num_chars <- if (char == "u") 4 else 2
                                    next_chars_end_idx <- self$index + num_chars
                                    next_chars <- substr(self$json_str, self$index + 1, next_chars_end_idx)
                                    if (nchar(next_chars) == num_chars && grepl("^[0-9a-fA-F]+$", next_chars)) {
                                      self$log("Found a unicode escape sequence, normalizing it")
                                      string_acc <- substr(string_acc, 1, nchar(string_acc) - 1) # Remove backslash
                                      string_acc <- paste0(string_acc, intToUtf8(strtoi(next_chars, 16L)))
                                      self$index <- self$index + num_chars
                                      char <- self$get_char_at()
                                      next
                                    }
                                  }
                                }

                                # If in object key context and ':' is found, it might be a missing right quote
                                if (isTRUE(char) && char == ":" && !missing_quotes && self$context$current == ContextValues$OBJECT_KEY) {
                                  # Look ahead for a quoted value
                                  i <- self$skip_to_character(character = lstring_delimiter, idx = 1)
                                  next_c <- self$get_char_at(i)
                                  if (isTRUE(next_c)) {
                                    i <- i + 1 # Skip opening quote
                                    i <- self$skip_to_character(character = rstring_delimiter, idx = i)
                                    next_c <- self$get_char_at(i)
                                    if (isTRUE(next_c)) {
                                      i <- i + 1 # Skip closing quote
                                      i <- self$skip_whitespaces_at(idx = i, move_main_index = FALSE)
                                      next_c <- self$get_char_at(i)
                                      if (isTRUE(next_c) && next_c %in% c(",", "}")) {
                                        self$log("While parsing a string missing the right delimiter in object key context, we found a :, stopping here")
                                        break
                                      }
                                    }
                                  } else {
                                    self$log("While parsing a string missing the right delimiter in object key context, we found a :, stopping here")
                                    break
                                  }
                                }

                                # Handling doubled quotes or misplaced quotes
                                if (isTRUE(char) && char == rstring_delimiter && nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) != "\\") {
                                  if (doubled_quotes && self$get_char_at(1) == rstring_delimiter) {
                                    self$log("While parsing a string, we found a doubled quote, ignoring it")
                                    self$index <- self$index + 1
                                  } else if (missing_quotes && self$context$current == ContextValues$OBJECT_VALUE) {
                                    i <- 1
                                    next_c <- self$get_char_at(i)
                                    while (isTRUE(next_c) && !(next_c %in% c(rstring_delimiter, lstring_delimiter))) {
                                      i <- i + 1
                                      next_c <- self$get_char_at(i)
                                    }
                                    if (isTRUE(next_c)) {
                                      i <- i + 1
                                      i <- self$skip_whitespaces_at(idx = i, move_main_index = FALSE)
                                      next_c <- self$get_char_at(i)
                                      if (isTRUE(next_c) && next_c == ":") {
                                        self$index <- self$index - 1
                                        char <- self$get_char_at()
                                        self$log("In a string with missing quotes and object value context, I found a delimeter but it turns out it was the beginning on the next key. Stopping here.")
                                        break
                                      }
                                    }
                                  } else if (unmatched_delimiter) {
                                    unmatched_delimiter <- FALSE
                                    string_acc <- paste0(string_acc, char)
                                    self$index <- self$index + 1
                                    char <- self$get_char_at()
                                  } else {
                                    i <- 1
                                    next_c <- self$get_char_at(i)
                                    check_comma_in_object_value <- TRUE
                                    while (isTRUE(next_c) && !(next_c %in% c(rstring_delimiter, lstring_delimiter))) {
                                      if (check_comma_in_object_value && grepl("[[:alpha:]]", next_c)) {
                                        check_comma_in_object_value <- FALSE
                                      }
                                      if ((ContextValues$OBJECT_KEY %in% self$context$context && next_c %in% c(":", "}")) ||
                                          (ContextValues$OBJECT_VALUE %in% self$context$context && next_c == "}") ||
                                          (ContextValues$ARRAY %in% self$context$context && next_c %in% c("]", ",")) ||
                                          (check_comma_in_object_value && self$context$current == ContextValues$OBJECT_VALUE && next_c == ",")) {
                                        break
                                      }
                                      i <- i + 1
                                      next_c <- self$get_char_at(i)
                                    }

                                    if (isTRUE(next_c) && next_c == "," && self$context$current == ContextValues$OBJECT_VALUE) {
                                      i <- i + 1
                                      i <- self$skip_to_character(character = rstring_delimiter, idx = i)
                                      next_c <- self$get_char_at(i)
                                      i <- i + 1
                                      i <- self$skip_whitespaces_at(idx = i, move_main_index = FALSE)
                                      next_c <- self$get_char_at(i)
                                    } else if (isTRUE(next_c) && next_c == rstring_delimiter && nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) != "\\") {
                                      if (all(sapply(1:(i-1), function(j) grepl("[[:space:]]", self$get_char_at(j))))) { # All spaces before
                                        break
                                      }
                                      if (self$context$current == ContextValues$OBJECT_VALUE) {
                                        i_temp <- self$skip_to_character(character = rstring_delimiter, idx = i + 1)
                                        i_temp <- i_temp + 1
                                        next_c_temp <- self$get_char_at(i_temp)
                                        while (isTRUE(next_c_temp) && next_c_temp != ":") {
                                          if (next_c_temp %in% c(",", "]", "}") || (next_c_temp == rstring_delimiter && self$get_char_at(i_temp - 1) != "\\")) {
                                            break
                                          }
                                          i_temp <- i_temp + 1
                                          next_c_temp <- self$get_char_at(i_temp)
                                        }
                                        if (isTRUE(next_c_temp) && next_c_temp != ":") {
                                          self$log("While parsing a string, we a misplaced quote that would have closed the string but has a different meaning here, ignoring it")
                                          unmatched_delimiter <- !unmatched_delimiter
                                          string_acc <- paste0(string_acc, char)
                                          self$index <- self$index + 1
                                          char <- self$get_char_at()
                                        }
                                      } else if (self$context$current == ContextValues$ARRAY) {
                                        self$log("While parsing a string in Array context, we detected a quoted section that would have closed the string but has a different meaning here, ignoring it")
                                        unmatched_delimiter <- !unmatched_delimiter
                                        string_acc <- paste0(string_acc, char)
                                        self$index <- self$index + 1
                                        char <- self$get_char_at()
                                      } else if (self$context$current == ContextValues$OBJECT_KEY) {
                                        self$log("While parsing a string in Object Key context, we detected a quoted section that would have closed the string but has a different meaning here, ignoring it")
                                        string_acc <- paste0(string_acc, char)
                                        self$index <- self$index + 1
                                        char <- self$get_char_at()
                                      }
                                    }
                                  }
                                }
                              }

                              # Special handling for comments instead of valid strings
                              if (isTRUE(char) && missing_quotes && self$context$current == ContextValues$OBJECT_KEY && grepl("[[:space:]]", char)) {
                                self$log("While parsing a string, handling an extreme corner case in which the LLM added a comment instead of valid string, invalidate the string and return an empty value")
                                self$skip_whitespaces_at()
                                if (!(self$get_char_at() %in% c(":", ","))) {
                                  return("")
                                }
                              }

                              # Handle missing closing quote
                              if (isFALSE(char) || char != rstring_delimiter) {
                                if (!self$stream_stable) {
                                  self$log("While parsing a string, we missed the closing quote, ignoring")
                                  string_acc <- trimws(string_acc, "right")
                                }
                              } else {
                                self$index <- self$index + 1 # Move past closing quote
                              }

                              if (!self$stream_stable && (missing_quotes || (nchar(string_acc) > 0 && substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\n"))) {
                                string_acc <- trimws(string_acc, "right")
                              }

                              return(string_acc)
                            },

                            #' @description
                            #' Parses a JSON number.
                            #' @return The parsed number (numeric, integer), or character if invalid.
                            parse_number = function() {
                              number_str <- ""
                              char <- self$get_char_at()
                              is_array <- self$context$current == ContextValues$ARRAY

                              while (isTRUE(char) && char %in% self$NUMBER_CHARS && (!is_array || char != ",")) {
                                number_str <- paste0(number_str, char)
                                self$index <- self$index + 1
                                char <- self$get_char_at()
                              }

                              # Rollback if number ends with non-valid char
                              if (nchar(number_str) > 0 && substr(number_str, nchar(number_str), nchar(number_str)) %in% c("-", "e", "E", "/", ",")) {
                                number_str <- substr(number_str, 1, nchar(number_str) - 1)
                                self$index <- self$index - 1
                              } else if (isTRUE(self$get_char_at()) && grepl("[[:alpha:]]", self$get_char_at())) {
                                # This was a string, not a number
                                self$index <- self$index - nchar(number_str)
                                return(self$parse_string())
                              }

                              # Attempt to convert to number
                              if (grepl(",", number_str, fixed = TRUE)) { # Contains comma, treat as string
                                return(as.character(number_str))
                              }
                              if (grepl(".", number_str, fixed = TRUE) || grepl("e", tolower(number_str), fixed = TRUE)) {
                                num_val <- suppressWarnings(as.numeric(number_str))
                                if (!is.na(num_val)) return(num_val) else return(as.character(number_str))
                              } else {
                                num_val <- suppressWarnings(as.integer(number_str))
                                if (!is.na(num_val)) return(num_val) else return(as.character(number_str))
                              }
                            },

                            #' @description
                            #' Parses JSON boolean or null literals.
                            #' @return TRUE, FALSE, NULL, or empty string if not a valid literal.
                            parse_boolean_or_null = function() {
                              starting_index <- self$index
                              char <- tolower(self$get_char_at() %||% "")
                              value_map <- list(t = c("true", TRUE), f = c("false", FALSE), n = c("null", NULL))

                              if (char %in% names(value_map)) {
                                expected_str <- value_map[[char]][[1]]
                                expected_val <- value_map[[char]][[2]]

                                i <- 1
                                current_char <- char
                                while (isTRUE(current_char) && i <= nchar(expected_str) && current_char == substr(expected_str, i, i)) {
                                  i <- i + 1
                                  self$index <- self$index + 1
                                  current_char <- tolower(self$get_char_at() %||% "")
                                }

                                if (i - 1 == nchar(expected_str)) { # Successfully matched full literal
                                  return(expected_val)
                                }
                              }

                              self$index <- starting_index # Reset index if no match
                              return("")
                            },

                            #' @description
                            #' Parses and skips over code-like comments.
                            #' @return An empty string as comments are ignored in the output JSON.
                            parse_comment = function() {
                              char <- self$get_char_at()
                              termination_characters <- c("\n", "\r")
                              if (ContextValues$ARRAY %in% self$context$context) {
                                termination_characters <- c(termination_characters, "]")
                              }
                              if (ContextValues$OBJECT_VALUE %in% self$context$context) {
                                termination_characters <- c(termination_characters, "}")
                              }
                              if (ContextValues$OBJECT_KEY %in% self$context$context) {
                                termination_characters <- c(termination_characters, ":")
                              }

                              # Line comment starting with #
                              if (char == "#") {
                                comment <- ""
                                while (isTRUE(char) && !(char %in% termination_characters)) {
                                  comment <- paste0(comment, char)
                                  self$index <- self$index + 1
                                  char <- self$get_char_at()
                                }
                                self$log(paste0("Found line comment: ", comment))
                                return("")
                              } else if (char == "/") {
                                next_char <- self$get_char_at(1)
                                # Line comment starting with //
                                if (next_char == "/") {
                                  comment <- "//"
                                  self$index <- self$index + 2 # Skip both slashes
                                  char <- self$get_char_at()
                                  while (isTRUE(char) && !(char %in% termination_characters)) {
                                    comment <- paste0(comment, char)
                                    self$index <- self$index + 1
                                    char <- self$get_char_at()
                                  }
                                  self$log(paste0("Found line comment: ", comment))
                                  return("")
                                } else if (next_char == "*") {
                                  # Block comment starting with /*
                                  comment <- "/*"
                                  self$index <- self$index + 2 # Skip '/*'
                                  while (TRUE) {
                                    char <- self$get_char_at()
                                    if (isFALSE(char)) {
                                      self$log("Reached end-of-string while parsing block comment; unclosed block comment.")
                                      break
                                    }
                                    comment <- paste0(comment, char)
                                    self$index <- self$index + 1
                                    if (nchar(comment) >= 2 && substr(comment, nchar(comment) - 1, nchar(comment)) == "*/") {
                                      break
                                    }
                                  }
                                  self$log(paste0("Found block comment: ", comment))
                                  return("")
                                } else {
                                  # Skip standalone '/'
                                  self$index <- self$index + 1
                                  return("")
                                }
                              }
                              return("") # Should not be reached
                            },

                            #' @description
                            #' Gets the character at the current index plus an offset.
                            #' @param count The offset from the current index.
                            #' @return The character as a string, or FALSE if out of bounds.
                            get_char_at = function(count = 0) {
                              pos <- self$index + count + 1 # R uses 1-based indexing
                              if (pos > 0 && pos <= nchar(self$json_str)) {
                                return(substr(self$json_str, pos, pos))
                              }
                              return(FALSE) # Using FALSE to mimic Python's end-of-string check
                            },

                            #' @description
                            #' Skips whitespace characters from a given index.
                            #' @param idx The starting offset from the main index.
                            #' @param move_main_index Logical, if TRUE, the main parser index is moved.
                            #' @return The new offset (idx) after skipping whitespaces.
                            skip_whitespaces_at = function(idx = 0, move_main_index = TRUE) {
                              current_idx <- self$index + idx + 1 # 1-based R index
                              char <- self$get_char_at(idx)
                              while (isTRUE(char) && grepl("[[:space:]]", char)) {
                                if (move_main_index) {
                                  self$index <- self$index + 1
                                } else {
                                  idx <- idx + 1
                                }
                                char <- self$get_char_at(idx)
                              }
                              return(idx)
                            },

                            #' @description
                            #' Skips characters until a specific target character is found.
                            #' @param character The target character to find.
                            #' @param idx The starting offset from the main index.
                            #' @return The new offset (idx) after skipping to the character.
                            skip_to_character = function(character, idx = 0) {
                              char <- self$get_char_at(idx)
                              while (isTRUE(char) && char != character) {
                                idx <- idx + 1
                                char <- self$get_char_at(idx)
                              }
                              return(idx)
                            },

                            #' @description
                            #' Internal logging function. Appends log messages with context.
                            #' @param text The log message.
                            `_log` = function(text) {
                              window_size <- 10
                              start <- max(self$index - window_size, 0) + 1 # R 1-based index
                              end <- min(self$index + window_size, nchar(self$json_str))
                              context_str <- substr(self$json_str, start, end)
                              self$logger <- c(self$logger, list(list(text = text, context = context_str)))
                            }
                          ),
                          private = list(
                            # Helper for comparing objects (used in parse to detect updates)
                            is_same_object = function(obj1, obj2) {
                              if (typeof(obj1) != typeof(obj2)) return(FALSE)

                              # Basic recursive comparison for lists (R's equivalent of Python dict/list)
                              if (is.list(obj1) && is.list(obj2)) {
                                if (length(obj1) != length(obj2)) return(FALSE)
                                # Check if names are identical and content for each named element
                                if (!identical(names(obj1), names(obj2))) return(FALSE)
                                for (i in seq_along(obj1)) {
                                  if (!private$is_same_object(obj1[[i]], obj2[[i]])) {
                                    return(FALSE)
                                  }
                                }
                                return(TRUE)
                              }
                              # For atomic vectors (numeric, character, logical)
                              if (is.atomic(obj1) && is.atomic(obj2)) {
                                return(identical(obj1, obj2))
                              }
                              # Default for other types, or if they are environments (like R6 objects)
                              # For simplicity, if they are not lists/atomic, just use identical
                              return(identical(obj1, obj2))
                            }
                          )
)

# Define a simple operator for NULL-coalescing (similar to Python's `or` for default values)
# Usage: `value %||% default_value`
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || (is.atomic(lhs) && is.na(lhs))) {
    rhs
  } else {
    lhs
  }
}

# Source this file for the classes to be available.
