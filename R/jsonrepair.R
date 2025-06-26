# jsonrepair.R

ContextValues <- c("OBJECT_KEY", "OBJECT_VALUE", "ARRAY")

#' @title JsonContext
#' @description Internal JSON parser class for managing parsing context stack
#' @details This R6 class maintains a stack-based context system for JSON parsing operations.
#'   It tracks the current parsing state and allows for pushing/popping context values
#'   during the parsing process. The context helps determine how to interpret different
#'   parts of the JSON structure.
#' @field context A list storing the context stack
#' @field current The current active context value
#' @field empty Logical indicating if the context stack is empty
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{Constructor method to create a new JsonContext instance}
#'   \item{\code{set(value)}}{Push a new context value onto the stack. The value must be one of the valid ContextValues}
#'   \item{\code{reset()}}{Pop the most recent context value from the stack and update current context}
#'   \item{\code{get_current()}}{Return the current active context value}
#'   \item{\code{is_empty()}}{Check if the context stack is empty}
#' }
#' @import R6
#' @examples
#' \dontrun{
#' ctx <- JsonContext$new()
#' ctx$set("object")
#' ctx$get_current()  # Returns "object"
#' ctx$reset()
#' ctx$is_empty()     # Returns TRUE
#' }
#' @keywords internal
JsonContext <- R6Class(
  "JsonContext",
  public = list(
    context = list(),
    current = NULL,
    empty = TRUE,
    #' @description Initialize a new JSONParser instance
    initialize = function() {
    },
    #' @description Push a new context value onto the stack
    #' @param value Character string representing the context value. Must be one of the valid ContextValues
    #' @details This method adds a new context value to the top of the context stack,
    #'   updates the current context, and marks the context as non-empty.
    #' @return No return value, called for side effects
    set = function(value) {
      if (!value %in% ContextValues) {
        stop("Invalid ContextValue. Must be one of: ", paste(ContextValues, collapse = ", "))
      }
      self$context <- c(self$context, value)
      self$current <- value
      self$empty <- FALSE
    },
    #' @description Pop the most recent context value from the stack
    #' @details This method removes the top context value from the stack and updates
    #'   the current context to the previous value. If the stack becomes empty,
    #'   current is set to NULL and empty flag is set to TRUE.
    #' @return No return value, called for side effects
    reset = function() {
      if (length(self$context) > 0) {
        self$context <- self$context[-length(self$context)]
        if (length(self$context) > 0) {
          self$current <- self$context[[length(self$context)]]
        } else {
          self$current <- NULL
          self$empty <- TRUE
        }
      } else {
        self$current <- NULL
        self$empty <- TRUE
      }
    },

    #' @description Get the current active context value
    #' @return Character string representing the current context value, or NULL if context is empty
    get_current = function() {
      return(self$current)
    },
    #' @description Check if the context stack is empty
    #' @return Logical value: TRUE if the context stack is empty, FALSE otherwise
    is_empty = function() {
      return(self$empty)
    }
  )
)


#' ObjectComparer
#'
#' @title ObjectComparer
#' @description Internal utility class for deep comparison of R objects in JSON parsing context
#' @details This R6 class provides functionality to recursively compare R objects for structural
#'   and content equality. It handles nested lists (both named and unnamed), which correspond
#'   to JSON objects and arrays respectively. The comparison is designed to work with objects
#'   that have been parsed from JSON format.
#' @section Class Methods:
#' \describe{
#'   \item{\code{is_same_object(obj1, obj2)}}{
#'     Recursively compares two R objects for deep equality.
#'     Returns FALSE immediately if object types don't match.
#'     For named lists (JSON objects): compares keys and recursively compares values.
#'     For unnamed lists (JSON arrays): compares length and recursively compares elements.
#'     For primitive types: assumes equality if types match.
#'   }
#' }
#' @keywords internal
#' @export
ObjectComparer <- R6::R6Class("ObjectComparer",
                              public = list(
                                #' @description Initialize a new ObjectComparer instance
                                #' @details No-operation constructor as this class is primarily used for its static methods
                                #' @return A new ObjectComparer instance
                                initialize = function() {}  # No-op constructor
                              ),
                              class = TRUE,  # Enable class-level methods
                              active = list(),
                              private = list(),
                              portable = TRUE
)
#' Recursively compare two R objects for deep structural equality
#' @name  ObjectComparer$is_same_object
#' @description Recursively compare two R objects for deep structural equality
#' @param obj1 First object to compare - can be any R object (list, vector, primitive)
#' @param obj2 Second object to compare - can be any R object (list, vector, primitive)
#' @details This static method performs deep comparison of R objects with special handling for:
#'   \itemize{
#'     \item Named lists (JSON objects): Compares all keys and recursively compares corresponding values
#'     \item Unnamed lists (JSON arrays): Compares length and recursively compares elements by position
#'     \item Primitive types: Returns TRUE if object classes match (assumes content equality for performance)
#'   }
#'   The comparison returns FALSE immediately if object classes don't match, enabling
#'   fast rejection of obviously different objects.
#' @return Logical value: TRUE if objects are structurally and content-wise identical, FALSE otherwise
#' @keywords internal
ObjectComparer$is_same_object <- function(obj1, obj2) {
  # Return FALSE immediately if types don't match
  if (class(obj1) != class(obj2)) {
    return(FALSE)
  }

  # If both are named lists (equivalent to Python dict)
  if (is.list(obj1) && !is.null(names(obj1))) {
    if (length(obj1) != length(obj2)) {
      return(FALSE)
    }
    for (key in names(obj1)) {
      if (!(key %in% names(obj2))) {
        return(FALSE)
      }
      if (!ObjectComparer$is_same_object(obj1[[key]], obj2[[key]])) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  # If both are unnamed lists (equivalent to Python list)
  if (is.list(obj1) && is.null(names(obj1))) {
    if (length(obj1) != length(obj2)) {
      return(FALSE)
    }
    for (i in seq_along(obj1)) {
      if (!ObjectComparer$is_same_object(obj1[[i]], obj2[[i]])) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  # For primitive types (numeric, character, logical, etc.), assume equality if types match
  return(TRUE)
}


#' @title JSONParser
#' @description Internal JSON parser class for incremental parsing of JSON strings
#' @details This R6 class implements a streaming JSON parser that can handle incomplete
#'   or streaming JSON data. It maintains parsing state through an index position and
#'   context stack, allowing for incremental parsing and handling of multiple JSON
#'   objects in a single string. The parser supports logging for debugging purposes
#'   and can detect duplicate objects during streaming operations.
#' @field json_str Character string containing the JSON data to parse
#' @field index Integer position of the current parsing location in the JSON string
#' @field context JsonContext object managing the parsing context stack
#' @field logging Logical flag indicating whether to enable parsing logs
#' @field logger List storing log messages when logging is enabled
#' @field stream_stable Logical flag for streaming stability mode
#' @field log Function for logging (no-op when logging disabled)
#' @field NUMBER_CHARS Character vector of valid numeric characters including operators
#' @field STRING_DELIMITERS Character vector of valid string delimiter characters
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(json_str, logging = FALSE, stream_stable = FALSE)}}{
#'     Constructor to create a new JSONParser instance.
#'     \code{json_str}: JSON string to parse
#'     \code{logging}: Enable logging for debugging
#'     \code{stream_stable}: Enable stream stability mode
#'   }
#'   \item{\code{parse()}}{
#'     Main parsing function that processes the JSON string from the current index position.
#'     Handles multiple JSON objects in a single string and removes duplicates when detected.
#'     Returns the parsed JSON object(s), or a list containing both the result and logs if logging is enabled.
#'   }
#' }
#' @examples
#' \dontrun{
#' # Basic parsing
#' parser <- JSONParser$new('{"key": "value"}')
#' result <- parser$parse()
#'
#' # Parsing with logging
#' parser <- JSONParser$new('{"key": "value"}', logging = TRUE)
#' result_with_logs <- parser$parse()
#'
#' # Streaming mode
#' parser <- JSONParser$new('{"a": 1}{"b": 2}', stream_stable = TRUE)
#' result <- parser$parse()  # Returns list of objects
#' }
#' @keywords internal
JSONParser <- R6Class("JSONParser",
                      public = list(

                        json_str = NULL,
                        index = 1L,
                        context = NULL,
                        logging = FALSE,
                        logger = NULL,
                        stream_stable = FALSE,
                        log = NULL,
                        NUMBER_CHARS =  c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", ".", "e", "E", "/", ","),
                        STRING_DELIMITERS =  c('"', "'", "“", "”"),

                        #' @description Initialize a new JSONParser instance
                        #' @param json_str Character string containing JSON data to parse
                        #' @param logging Logical flag to enable debug logging (default: FALSE)
                        #' @param stream_stable Logical flag for streaming stability mode (default: FALSE)
                        #' @return A new JSONParser object
                        initialize = function(json_str,
                                              logging = FALSE,
                                              stream_stable = FALSE) {
                          self$json_str <- json_str
                          self$index <- 1L
                          self$context <- JsonContext$new()
                          self$logging <- logging
                          self$stream_stable <- stream_stable
                          if (logging) {
                            self$logger <- list()
                            self$log <- self$._log
                          } else {
                            self$log <- function(...) {}  # no-op
                          }
                        },
    #' @description Main parse function that processes JSON string and handles multiple objects
    #' @return Parsed JSON object(s). If logging enabled, returns list with result and logs.
    #'   For multiple objects, returns a list unless they are duplicates.
    parse = function() {

      self$log("Start Parsing")
      json <- self$parse_json()


      if (self$index < nchar(self$json_str)) {
        self$log("The parser returned early, checking if there's more json elements")
        json_list <- list(json)

        while (self$index < nchar(self$json_str)) {
          j <- self$parse_json()

          if (!identical(j, "")) {
            if (ObjectComparer$is_same_object(json_list[[length(json_list)]], j)) {
              json_list <- json_list[-length(json_list)]
            }
            json_list <- append(json_list, list(j))
          } else {
            self$index <- self$index + 1
          }
        }

        if (length(json_list) == 1) {
          self$log("There were no more elements, returning the element without the array")
          json <- json_list[[1]]
        } else {
          json <- json_list
        }
      }

      if (self$logging) {
        return(list(json, self$logger))
      } else {
        return(json)
      }
    },


    #' @description Main JSON parsing dispatcher method
    #' @details This is the core parsing method that determines the type of JSON element
    #'   at the current parsing position and delegates to the appropriate specialized
    #'   parsing method. It handles the main JSON data types: objects, arrays, and
    #'   special edge cases for malformed JSON.
    #'
    #'   The method operates in a loop, examining each character to determine the
    #'   JSON structure type. It maintains parsing state through the context stack
    #'   and handles various edge cases that may occur in streaming or malformed JSON.
    #'
    #' @return The parsed JSON element:
    #'   \itemize{
    #'     \item For objects: Result from \code{parse_object()}
    #'     \item For arrays: Result from \code{parse_array()}
    #'     \item For end-of-input: Empty string \code{""}
    #'     \item For missing values in objects: Empty string \code{""} with warning log
    #'   }
    #'
    #' @section Parsing Logic:
    #' The method follows a specific decision tree:
    #' \enumerate{
    #'   \item \strong{End of Input}: When no more characters are available, returns empty string
    #'   \item \strong{Object Detection}: When encountering '{', delegates to \code{parse_object()}
    #'   \item \strong{Array Detection}: When encountering '[', delegates to \code{parse_array()}
    #'   \item \strong{Edge Case Handling}: Manages malformed JSON scenarios like missing values
    #' }
    #'
    #' @section Context Awareness:
    #' The method is context-aware and behaves differently based on the current parsing state:
    #' \itemize{
    #'   \item \strong{OBJECT_VALUE context}: Special handling for missing values when '}' is encountered
    #'   \item \strong{General parsing}: Standard object and array detection
    #'   \item \strong{Stream stability}: Maintains state for continuous parsing operations
    #' }
    #'
    #' @section Error Recovery:
    #' Implements graceful error recovery for common JSON malformation issues:
    #' \itemize{
    #'   \item Missing values in object properties
    #'   \item Incomplete JSON structures in streaming scenarios
    #'   \item Unexpected end-of-input conditions
    #' }
    #' @keywords internal
    parse_json = function() {

      repeat {
        # Current character (or FALSE when we are past the end)
        char <- self$get_char_at()

        # 1) Reached the end of the input → return empty string
        if (isFALSE(char)) {
          return("")

          # 2) An object starts with '{'
        } else if (char == "{") {
          self$index <- self$index + 1
          return(self$parse_object())

          # 3) An array starts with '['
        } else if (char == "[") {
          self$index <- self$index + 1
          return(self$parse_array())

          # 4) Edge case: inside an object value, '}' means a missing value
        } else if (!is.null(self$context$current) &&
                   self$context$current == "OBJECT_VALUE" &&
                   char == "}") {
          self$log(
            "At the end of an object we found a key with missing value, skipping"
          )
          return("")

          # 5) String: when in context, starts with a quote or an alpha character
        } else if (!self$context$empty &&
                   (char %in% self$STRING_DELIMITERS ||
                    grepl("^[[:alpha:]]$", char))) {
          return(self$parse_string())

          # 6) Number: when in context, starts with digit / minus / dot
        } else if (!self$context$empty &&
                   (grepl("^[0-9]$", char) || char %in% c("-", "."))) {
          return(self$parse_number())

          # 7) Comment: starts with '#' or '/'
        } else if (char %in% c("#", "/")) {
          return(self$parse_comment())

          # 8) Anything else → skip one character and continue
        } else {
          self$index <- self$index + 1
        }
      }
    },
    #' @description Parse a JSON object into an R named list
    #' @details This method handles the parsing of JSON objects (denoted by curly braces {})
    #'   into R named lists. It processes key-value pairs sequentially, handling various
    #'   edge cases and malformed JSON scenarios that may occur in streaming or real-world
    #'   data. The method maintains robust error recovery and context awareness throughout
    #'   the parsing process.
    #'
    #'   The parser assumes the opening '{' has already been consumed and focuses on
    #'   parsing the object contents until the closing '}' is encountered or end-of-input
    #'   is reached. It handles whitespace normalization, key parsing, value parsing,
    #'   and various JSON formatting irregularities.
    #'
    #'
    #' @keywords internal
    parse_object = function() {
      obj <- list()                    # resulting R named list
      # Walk forward until we hit '}' or the end of the input
      while ( (ch <- self$get_char_at()) != "}" && !isFALSE(ch) ) {

        # 1) Skip leading whitespace
        self$skip_whitespaces_at()

        # 2) If an unexpected ':' turns up before the key   {... : "oops"}
        if (identical(self$get_char_at(), ":")) {
          self$log("While parsing an object we found ':' before a key, ignoring")
          self$index <- self$index + 1
        }

        # 3) We are about to parse the key string
        self$context$set("OBJECT_KEY")
        rollback_index <- self$index      # in case we must roll back
        key <- ""

        while (!isFALSE(self$get_char_at())) {
          rollback_index <- self$index    # track for duplicate-key rollback

          # --- Special edge case ----------------------------------------
          # If we meet a '[' immediately (no key) and the previous value
          # was an array, we treat this as "concatenate two arrays".
          if (self$get_char_at() == "[" && key == "") {
            prev_key <- if (length(obj)) tail(names(obj), 1) else NULL
            if (!is.null(prev_key) && is.list(obj[[prev_key]])) {
              self$index <- self$index + 1          # consume '['
              new_array <- self$parse_array()       # parse the next array

              if (is.list(new_array)) {
                prev_val <- obj[[prev_key]]
                # flatten if the new array is wrapped one level deeper
                merged <- if (length(new_array) == 1 &&
                              is.list(new_array[[1]])) {
                  c(prev_val, new_array[[1]])
                } else {
                  c(prev_val, new_array)
                }
                obj[[prev_key]] <- merged
              }

              self$skip_whitespaces_at()
              if (self$get_char_at() == ",") self$index <- self$index + 1
              self$skip_whitespaces_at()
              next   # continue searching for the real key
            }
          }
          # ----------------------------------------------------------------

          # Try reading a string key (quoted or unquoted)
          key <- as.character(self$parse_string())
          if (key == "") self$skip_whitespaces_at()

          # Exit the repeat-loop when we *have* a key, OR we see : / }
          if (key != "" || (key == "" && self$get_char_at() %in% c(":", "}")))
            break
        }

        # 4) Duplicate key inside an array context → close object early
        if ("ARRAY" %in% self$context$context &&
            key %in% names(obj)) {
          self$log("Duplicate key detected, closing object early")
          self$index <- rollback_index - 1
          # Insert a '{' so the rest of the parser can keep going safely
          self$json_str <- paste0(
            substr(self$json_str, 1, self$index),
            "{",
            substr(self$json_str, self$index + 1, nchar(self$json_str))
          )
          break
        }

        # 5) Skip whitespace before the ':' (or directly '}' for empty value)
        self$skip_whitespaces_at()

        if (identical(self$get_char_at(), "}")) next   # empty member
        if(isFALSE(self$get_char_at())) next
        self$skip_whitespaces_at()

        # 6) If ':' missing after key, log but keep going
        if (!identical(self$get_char_at(), ":")) {
          self$log("Missing ':' after key in object, attempting recovery")
        }

        # 7) Consume ':' then parse the value
        self$index <- self$index + 1
        self$context$reset()
        self$context$set("OBJECT_VALUE")
        value <- self$parse_json()          # value can be any JSON type
        self$context$reset()

        # 8) Store key → value
        obj[[key]] <- value

        # 9) Consume separator characters , ' or "
        if (self$get_char_at() %in% c(",", "'", '"'))
          self$index <- self$index + 1

        # Remove trailing spaces before next member / closing brace
        self$skip_whitespaces_at()
      }

      # 10) Finally consume the closing '}'
      self$index <- self$index + 1
      return(obj)
    },
    #' @description Parse a JSON array into an R list
    #' @details This method handles the parsing of JSON arrays (denoted by square brackets [])
    #'   into R lists. It processes array elements sequentially, maintaining proper context
    #'   and handling various edge cases including nested structures, mixed data types,
    #'   and malformed array syntax. The parser supports both complete and streaming
    #'   array parsing scenarios.
    #'
    #'   The parser assumes the opening '[' has already been consumed and focuses on
    #'   parsing array elements until the closing ']' is encountered or end-of-input
    #'   is reached. It handles element separation, whitespace normalization, and
    #'   graceful error recovery for malformed arrays.
    #' @keywords internal
    parse_array = function() {
      arr <- list()                              # [] → list()
      self$context$set("ARRAY")      # push ARRAY context

      char <- self$get_char_at()                 # first char

      # while char and char not in ["]", "}"]
      while (!isFALSE(char) && !(char %in% c("]", "}"))) {

        self$skip_whitespaces_at()
        value <- ""                              # default “empty”

        # if char in STRING_DELIMITERS
        if (!isFALSE(char) && char %in% self$STRING_DELIMITERS) {
          i <- 1
          i <- self$skip_to_character(char, i)
          i <- self$skip_whitespaces_at(idx = i + 1, move_main_index = FALSE)

          value <- if (identical(self$get_char_at(i), ":")) {
            self$parse_object()
          } else {
            self$parse_string()
          }

        } else {
          value <- self$parse_json()
        }

        # if value == ""
        if (identical(value, "")) {
          self$index <- self$index + 1
        } else if (identical(value, "...") &&
                   identical(self$get_char_at(-1), ".")) {
          self$log("While parsing an array, found a stray '...'; ignoring it")
        } else {
          arr <- append(arr, list(value))
        }

        # skip over whitespace / commas until next token or ']'
        char <- self$get_char_at()
        while (!isFALSE(char) &&
               char != "]" &&
               (grepl("^\\s$", char, perl = TRUE) || char == ",")) {
          self$index <- self$index + 1
          char <- self$get_char_at()
        }
      }

      if (!isFALSE(char) && char != "]") {
        self$log("While parsing an array we missed the closing ']', ignoring it")
      }

      self$index <- self$index + 1               # consume ']'
      self$context$reset()                       # pop context

      return(arr)
    },

    #' @description Parse a JSON string value with flexible quote handling
    #' @details This method handles the parsing of JSON string values, including various
    #'   quote styles and edge cases. It manages missing quotes, doubled quotes, comments,
    #'   and literal values. This function resolves many weird cases in invalid JSON.
    #'
    #' @return A character string containing the parsed value. Returns empty string
    #'   for malformed or empty strings.
    #' @keywords internal
    parse_string = function() {
      # <string> is a string of valid characters enclosed in quotes
      # i.e. { name: "John" }
      # Somehow all weird cases in an invalid JSON happen to be resolved in this function, so be careful here

      # Flag to manage corner cases related to missing starting quote

      missing_quotes <- FALSE
      doubled_quotes <- FALSE
      lstring_delimiter <- rstring_delimiter <- '"'
      char <- self$get_char_at()
      if (char %in% c("#", "/")) {
        return(self$parse_comment())
      }

      # A valid string can only start with a valid quote or, in our case, with a literal
      while (!isFALSE(char) && !(char %in% self$STRING_DELIMITERS) && !grepl("^[[:alnum:]]$", char)) {
        self$index <- self$index + 1
        char <- self$get_char_at()
      }

      if (isFALSE(char) || is.null(char) || char == "" || length(char) == 0) {
        # This is an empty string
        return("")
      }

      # Ensuring we use the right delimiter
      if (char == "'") {
        lstring_delimiter <- rstring_delimiter <- "'"
      } else if (char == "“") {
    lstring_delimiter <- "“"
                 rstring_delimiter <- "”"
  } else if (grepl("^[[:alnum:]]$", char)) {
    # This could be a <boolean> and not a string. Because (T)rue or (F)alse or (N)ull are valid
    # But remember, object keys are only of type string
    if (tolower(char) %in% c("t", "f", "n") && self$context$current != "OBJECT_KEY") {
      value <- self$parse_boolean_or_null()
      if (value != "") {
        return(value)
      }
    }
    self$log( "While parsing a string, we found a literal instead of a quote")
    missing_quotes <- TRUE
  }

  if (!missing_quotes) {
    self$index <- self$index + 1
  }

  # There is sometimes a weird case of doubled quotes, we manage this also later in the while loop
  if (self$get_char_at() %in% self$STRING_DELIMITERS && self$get_char_at() == lstring_delimiter) {
    # If it's an empty key, this was easy
    if (self$context$current == "OBJECT_KEY" && self$get_char_at( 1) == ":") {
      self$index <- self$index + 1
      return("")
    }
    if (self$get_char_at( 1) == lstring_delimiter) {
      # There's something fishy about this, we found doubled quotes and then again quotes
      self$log( "While parsing a string, we found a doubled quote and then a quote again, ignoring it")
      return("")
    }
    # Find the next delimiter
    i <- self$skip_to_character( character = rstring_delimiter, idx = 1)
    next_c <- self$get_char_at( i)
    # Now check that the next character is also a delimiter to ensure that we have "".....""
    # In that case we ignore this rstring delimiter
    if (!isFALSE(next_c) && (self$get_char_at( i + 1) %||% "") == rstring_delimiter) {
      self$log( "While parsing a string, we found a valid starting doubled quote")
      doubled_quotes <- TRUE
      self$index <- self$index + 1
    } else {
      # Ok this is not a doubled quote, check if this is an empty string or not
      i <- self$skip_whitespaces_at( idx = 1, move_main_index = FALSE)
      next_c <- self$get_char_at( i)
      if (next_c %in% c(self$STRING_DELIMITERS, "{", "[")) {
        # something fishy is going on here
        self$log( "While parsing a string, we found a doubled quote but also another quote afterwards, ignoring it")
        self$index <- self$index + 1
        return("")
      } else if (!(next_c %in% c(",", "]", "}"))) {
        self$log( "While parsing a string, we found a doubled quote but it was a mistake, removing one quote")
        self$index <- self$index + 1
      }
    }
  }

  # Initialize our return value
  string_acc <- ""

  # Here things get a bit hairy because a string missing the final quote can also be a key or a value in an object
  # In that case we need to use the ":|,|}" characters as terminators of the string
  # So this will stop if:
  # * It finds a closing quote
  # * It iterated over the entire sequence
  # * If we are fixing missing quotes in an object, when it finds the special terminators
  char <- self$get_char_at()
  unmatched_delimiter <- FALSE

  while (!isFALSE(char) && char != rstring_delimiter) {
    if (missing_quotes && self$context$current == "OBJECT_KEY" && (char == ":" || grepl("^\\s$", char))) {
      self$log( "While parsing a string missing the left delimiter in object key context, we found a :, stopping here")
      break
    }

    if (!self$stream_stable &&
        self$context$current == "OBJECT_VALUE" &&
        char %in% c(",", "}") &&
        nchar(string_acc) > 0 &&
        substr(string_acc, nchar(string_acc), nchar(string_acc)) != rstring_delimiter) {

      rstring_delimiter_missing <- TRUE
      # check if this is a case in which the closing comma is NOT missing instead
      i <- self$skip_to_character( character = rstring_delimiter, idx = 1)
      next_c <- self$get_char_at( i)
      if (!isFALSE(next_c)) {
        i <- i + 1
        # found a delimiter, now we need to check that is followed strictly by a comma or brace
        # or the string ended
        i <- self$skip_whitespaces_at( idx = i, move_main_index = FALSE)
        next_c <- self$get_char_at( i)
        if (isFALSE(next_c) || next_c %in% c(",", "}")) {
          rstring_delimiter_missing <- FALSE
        } else {
          # OK but this could still be some garbage at the end of the string
          # So we need to check if we find a new lstring_delimiter afterwards
          # If we do, maybe this is a missing delimiter
          i <- self$skip_to_character( character = lstring_delimiter, idx = i)
          next_c <- self$get_char_at( i)
          if (isFALSE(next_c)) {
            rstring_delimiter_missing <- FALSE
          } else {
            # But again, this could just be something a bit stupid like "lorem, "ipsum" sic"
            # Check if we find a : afterwards (skipping space)
            i <- self$skip_whitespaces_at( idx = i + 1, move_main_index = FALSE)
            next_c <- self$get_char_at( i)
            if (!isFALSE(next_c) && next_c != ":") {
              rstring_delimiter_missing <- FALSE
            }
          }
        }
      } else {
        # There could be a case in which even the next key:value is missing delimeters
        # because it might be a systemic issue with the output
        # So let's check if we can find a : in the string instead
        i <- self$skip_to_character( character = ":", idx = 1)
        next_c <- self$get_char_at( i)
        if (!isFALSE(next_c)) {
          # OK then this is a systemic issue with the output
          break
        } else {
          # skip any whitespace first
          i <- self$skip_whitespaces_at( idx = 1, move_main_index = FALSE)
          # We couldn't find any rstring_delimeter before the end of the string
          # check if this is the last string of an object and therefore we can keep going
          # make an exception if this is the last char before the closing brace
          j <- self$skip_to_character( character = "}", idx = i)
          if (j - i > 1) {
            # Ok it's not right after the comma
            # Let's ignore
            rstring_delimiter_missing <- FALSE
          } else if (!isFALSE(self$get_char_at( j))) {
            # Check for an unmatched opening brace in string_acc
            for (k in nchar(string_acc):1) {
              c <- substr(string_acc, k, k)
              if (c == "{") {
                # Ok then this is part of the string
                rstring_delimiter_missing <- FALSE
                break
              }
            }
          }
        }
      }
      if (rstring_delimiter_missing) {
        self$log( "While parsing a string missing the left delimiter in object value context, we found a , or } and we couldn't determine that a right delimiter was present. Stopping here")
        break
      }
    }

    if (!self$stream_stable &&
        char == "]" &&
        "ARRAY" %in% self$context$context &&
        nchar(string_acc) > 0 &&
        substr(string_acc, nchar(string_acc), nchar(string_acc)) != rstring_delimiter) {
      # We found the end of an array and we are in array context
      # So let's check if we find a rstring_delimiter forward otherwise end early
i <- self$skip_to_character( rstring_delimiter)
if (isFALSE(self$get_char_at( i))) {
  # No delimiter found
  break
}
}

string_acc <- paste0(string_acc, char)
self$index <- self$index + 1
char <- self$get_char_at()

# Unclosed string ends with a \ character. This character is ignored if stream_stable = TRUE.
if (self$stream_stable && isFALSE(char) && nchar(string_acc) > 0 &&
    substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\\") {
  string_acc <- substr(string_acc, 1, nchar(string_acc) - 1)
}

if (!isFALSE(char) && nchar(string_acc) > 0 &&
    substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\\") {
  # This is a special case, if people use real strings this might happen
  self$log( "Found a stray escape sequence, normalizing it")
  if (char %in% c(rstring_delimiter, "t", "n", "r", "b", "\\")) {
    string_acc <- substr(string_acc, 1, nchar(string_acc) - 1)
    escape_seqs <- list("t" = "\t", "n" = "\n", "r" = "\r", "b" = "\b")
    string_acc <- paste0(string_acc, escape_seqs[[char]] %||% char)
    self$index <- self$index + 1
    char <- self$get_char_at()
    while (!isFALSE(char) && nchar(string_acc) > 0 &&
           substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\\" &&
           char %in% c(rstring_delimiter, "\\")) {
      # this is a bit of a special case, if I don't do this it will close the loop or create a train of \\
      # I don't love it though
      string_acc <- substr(string_acc, 1, nchar(string_acc) - 1)
      string_acc <- paste0(string_acc, char)
      self$index <- self$index + 1
      char <- self$get_char_at()
    }
    next
  } else if (char %in% c("u", "x")) {
    # If we find a unicode escape sequence, normalize it
    num_chars <- if (char == "u") 4 else 2
    next_chars <- substr(self$json_str, self$index + 2, self$index + 1 + num_chars)
    if (nchar(next_chars) == num_chars &&
        all(strsplit(next_chars, "")[[1]] %in% c(0:9, letters[1:6], LETTERS[1:6]))) {
      self$log( "Found a unicode escape sequence, normalizing it")
      string_acc <- substr(string_acc, 1, nchar(string_acc) - 1)
      string_acc <- paste0(string_acc, intToUtf8(strtoi(next_chars, 16)))
      self$index <- self$index + 1 + num_chars
      char <- self$get_char_at()
      next
    }
  }
}

# If we are in object key context and we find a colon, it could be a missing right quote
if (char == ":" && !missing_quotes && self$context$current == "OBJECT_KEY") {
  # Ok now we need to check if this is followed by a value like "..."
  i <- self$skip_to_character( character = lstring_delimiter, idx = 1)
  next_c <- self$get_char_at( i)
  if (!isFALSE(next_c)) {
    i <- i + 1
    # found the first delimiter
    i <- self$skip_to_character( character = rstring_delimiter, idx = i)
    next_c <- self$get_char_at( i)
    if (!isFALSE(next_c)) {
      # found a second delimiter
      i <- i + 1
      # Skip spaces
      i <- self$skip_whitespaces_at( idx = i, move_main_index = FALSE)
      next_c <- self$get_char_at( i)
      if (!isFALSE(next_c) && next_c %in% c(",", "}")) {
        # Ok then this is a missing right quote
        self$log( "While parsing a string missing the right delimiter in object key context, we found a :, stopping here")
        break
      }
    }
  } else {
    # The string ended without finding a lstring_delimiter, I will assume this is a missing right quote
    self$log( "While parsing a string missing the right delimiter in object key context, we found a :, stopping here")
    break
  }
}

# ChatGPT sometimes forget to quote stuff in html tags or markdown, so we do this whole thing here
if (char == rstring_delimiter && (nchar(string_acc) == 0 ||
                                  substr(string_acc, nchar(string_acc), nchar(string_acc)) != "\\")) {
  # Special case here, in case of double quotes one after another
  if (doubled_quotes && self$get_char_at( 1) == rstring_delimiter) {
    self$log( "While parsing a string, we found a doubled quote, ignoring it")
    self$index <- self$index + 1
  } else if (missing_quotes && self$context$current == "OBJECT_VALUE") {
    # In case of missing starting quote I need to check if the delimeter is the end or the beginning of a key
    i <- 1
    next_c <- self$get_char_at( i)
    while (!isFALSE(next_c) && !(next_c %in% c(rstring_delimiter, lstring_delimiter))) {
      i <- i + 1
      next_c <- self$get_char_at( i)
    }
    if (!isFALSE(next_c)) {
      # We found a quote, now let's make sure there's a ":" following
      i <- i + 1
      # found a delimiter, now we need to check that is followed strictly by a comma or brace
      i <- self$skip_whitespaces_at( idx = i, move_main_index = FALSE)
      next_c <- self$get_char_at( i)
      if (!isFALSE(next_c) && next_c == ":") {
        # Reset the cursor
        self$index <- self$index - 1
        char <- self$get_char_at()
        self$log( "In a string with missing quotes and object value context, I found a delimeter but it turns out it was the beginning on the next key. Stopping here.")
        break
      }
    }
  } else if (unmatched_delimiter) {
    unmatched_delimiter <- FALSE
    string_acc <- paste0(string_acc, char)
    self$index <- self$index + 1
    char <- self$get_char_at()
  } else {
    # Check if eventually there is a rstring delimiter, otherwise we bail
    i <- 1
    next_c <- self$get_char_at( i)
    check_comma_in_object_value <- TRUE

    while (!isFALSE(next_c) && !(next_c %in% c(rstring_delimiter, lstring_delimiter))) {
      # This is a bit of a weird workaround, essentially in object_value context we don't always break on commas
      # This is because the routine after will make sure to correct any bad guess and this solves a corner case
      if (check_comma_in_object_value && grepl("^[[:alpha:]]$", next_c)) {
        check_comma_in_object_value <- FALSE
      }
      # If we are in an object context, let's check for the right delimiters
      if (("OBJECT_KEY" %in% self$context$context && next_c %in% c(":", "}")) ||
          ("OBJECT_VALUE" %in% self$context$context && next_c == "}") ||
          ("ARRAY" %in% self$context$context && next_c %in% c("]", ",")) ||
          (check_comma_in_object_value && self$context$current == "OBJECT_VALUE" && next_c == ",")) {
        break
      }
      i <- i + 1
      next_c <- self$get_char_at( i)
    }
    # If we stopped for a comma in object_value context, let's check if find a "} at the end of the string
    if (next_c == "," && self$context$current == "OBJECT_VALUE") {
      i <- i + 1
      i <- self$skip_to_character( character = rstring_delimiter, idx = i)
      next_c <- self$get_char_at( i)
      # Ok now I found a delimiter, let's skip whitespaces and see if next we find a }
      i <- i + 1
      i <- self$skip_whitespaces_at( idx = i, move_main_index = FALSE)
      next_c <- self$get_char_at( i)
    } else if (next_c == rstring_delimiter &&
               (i == 1 || self$get_char_at( i - 1) != "\\")) {
      # Check if self$index:(self$index+i) is only whitespaces, break if that's the case
      chars_between <- sapply(1:(i-1), function(j) self$get_char_at( j))
      if (all(sapply(chars_between[!is.na(chars_between)], function(x) grepl("^\\s$", x)))) {
        break
      }
      if (self$context$current == "OBJECT_VALUE") {
        # But this might not be it! This could be just a missing comma
        # We found a delimiter and we need to check if this is a key
        # so find a rstring_delimiter and a colon after
        i <- self$skip_to_character( character = rstring_delimiter, idx = i + 1)
        i <- i + 1
        next_c <- self$get_char_at( i)
        while (!isFALSE(next_c) && next_c != ":") {
          if (next_c %in% c(",", "]", "}") ||
              (next_c == rstring_delimiter &&
               (i == 1 || self$get_char_at( i - 1) != "\\"))) {
            break
          }
          i <- i + 1
          next_c <- self$get_char_at( i)
        }
        # Only if we fail to find a ':' then we know this is misplaced quote
        if (next_c != ":") {
          self$log( "While parsing a string, we a misplaced quote that would have closed the string but has a different meaning here, ignoring it")
          unmatched_delimiter <- !unmatched_delimiter
          string_acc <- paste0(string_acc, char)
          self$index <- self$index + 1
          char <- self$get_char_at()
        }
      } else if (self$context$current == "ARRAY") {
        # If we got up to here it means that this is a situation like this:
        # ["bla bla bla "puppy" bla bla bla "kitty" bla bla"]
        # So we need to ignore this quote
        self$log( "While parsing a string in Array context, we detected a quoted section that would have closed the string but has a different meaning here, ignoring it")
        unmatched_delimiter <- !unmatched_delimiter
        string_acc <- paste0(string_acc, char)
        self$index <- self$index + 1
        char <- self$get_char_at()
      } else if (self$context$current == "OBJECT_KEY") {
        # In this case we just ignore this and move on
        self$log( "While parsing a string in Object Key context, we detected a quoted section that would have closed the string but has a different meaning here, ignoring it")
        string_acc <- paste0(string_acc, char)
        self$index <- self$index + 1
        char <- self$get_char_at()
      }
    }
  }
}
}

if (!isFALSE(char) && missing_quotes && self$context$current == "OBJECT_KEY" && grepl("^\\s$", char)) {
  self$log( "While parsing a string, handling an extreme corner case in which the LLM added a comment instead of valid string, invalidate the string and return an empty value")
  self$skip_whitespaces_at()
  if (!(self$get_char_at() %in% c(":", ","))) {
    return("")
  }
}

# A fallout of the previous special case in the while loop,
# we need to update the index only if we had a closing quote
if (char != rstring_delimiter) {
  # if stream_stable = TRUE, unclosed strings do not trim trailing whitespace characters
  if (!self$stream_stable) {
    self$log( "While parsing a string, we missed the closing quote, ignoring")
    string_acc <- trimws(string_acc, which = "right")
  }
} else {
  self$index <- self$index + 1
}

if (!self$stream_stable && (missing_quotes || (nchar(string_acc) > 0 &&
                                               substr(string_acc, nchar(string_acc), nchar(string_acc)) == "\n"))) {
  # Clean the whitespaces for some corner cases
  string_acc <- trimws(string_acc, which = "right")
}

return(string_acc)
},

    #' @description Parse a JSON number value into R numeric type
    #' @details This method handles the parsing of JSON number values in various formats
    #'   including integers, decimals, scientific notation, and negative numbers. It
    #'   provides context-aware parsing that handles array boundaries properly.
    #' @keywords internal
    parse_number = function() {
      # <number> is a valid real number expressed in one of a number of given formats
      number_str <- ""
      char <- self$get_char_at()
      is_array <- self$context$current == "ARRAY"
      while (!isFALSE(char) && char %in% self$NUMBER_CHARS &&
             (!is_array || char != ",")) {
        number_str <- paste0(number_str, char)
        self$index <- self$index + 1
        char <- self$get_char_at()
      }
      if (nchar(number_str) > 0 &&
          substr(number_str, nchar(number_str), nchar(number_str)) %in% c("-", "e", "E", "/", ",")) {
        # The number ends with a non valid character for a number/currency, rolling back one
        number_str <- substr(number_str, 1, nchar(number_str) - 1)
        self$index <- self$index - 1
      } else if (!isFALSE(self$get_char_at()) && grepl("\\p{L}", self$get_char_at(), perl = TRUE)) {
        # this was a string instead, sorry
        self$index <- self$index - nchar(number_str)
        return(self$parse_string())
      }

      tryCatch({
        if (grepl(",", number_str)) {
          return(number_str)  # 返回字符串
        }
        if (grepl("\\.", number_str) || grepl("e", number_str) || grepl("E", number_str)) {
          result <- as.numeric(number_str)
          if (is.na(result)) {
            return(number_str)  # 转换失败返回原字符串
          }
          return(result)  # 返回浮点数
        } else {
          result <- as.integer(number_str)
          if (is.na(result)) {
            return(number_str)  # 转换失败返回原字符串
          }
          return(result)  # 返回整数
        }
      }, error = function(e) {
        return(number_str)  # 其他错误时返回原字符串
      })
    },
    #' @description Parse JSON boolean or null literal values
    #' @details This method handles the parsing of JSON literal values: 'true', 'false',
    #'   and 'null' (unquoted). It performs case-insensitive matching and provides
    #'   rollback functionality for failed matches.
    #' @keywords internal
    parse_boolean_or_null = function() {
      # <boolean> is one of the literal strings 'true', 'false', or 'null' (unquoted)
      starting_index <- self$index
      char <- tolower(self$get_char_at() %||% "")
      value <- NULL

      if (char == "t") {
        value <- list(string = "true", result = TRUE)
      } else if (char == "f") {
        value <- list(string = "false", result = FALSE)
      } else if (char == "n") {
        value <- list(string = "null", result = "NULL")
      }

      if (!is.null(value)) {
        i <- 1  # R中索引从1开始
        while (!is.null(char) && i <= nchar(value$string) &&
               char == substr(value$string, i, i)) {
          i <- i + 1
          self$index <- self$index + 1
          char <- tolower(self$get_char_at() %||% "")
        }
        if (i > nchar(value$string)) {  # 完全匹配
          return(value$result)
        }
      }

      # If nothing works reset the index before returning
      self$index <- starting_index
      return("")
    },
    #' @description Parse and skip code-like comments in JSON
    #' @details This method handles parsing of various comment formats that may appear
    #'   in non-standard JSON. Comments are skipped entirely and do not interfere with
    #'   JSON parsing. Supports context-aware termination based on current parsing state.
    #' @keywords internal
    parse_comment = function() {

      char <- self$get_char_at()
      termination_characters <- c("\n", "\r")

      if ("ARRAY" %in% self$context$context) {
        termination_characters <- c(termination_characters, "]")
      }
      if ("OBJECT_VALUE" %in% self$context$context) {
        termination_characters <- c(termination_characters, "}")
      }
      if ("OBJECT_KEY" %in% self$context$context) {
        termination_characters <- c(termination_characters, ":")
      }

      # Line comment starting with #
      if (char == "#") {
        comment <- ""
        while (!isFALSE(char) && !is.na(char) && !(char %in% termination_characters)) {
          comment <- paste0(comment, char)
          self$index <- self$index + 1
          char <- self$get_char_at()
        }
        self$log(paste0("Found line comment: ", comment))
        return("")
      }

      # Comments starting with '/'
      else if (char == "/") {
        next_char <- self$get_char_at(1)

        # Handle line comment starting with //
        if (!isFALSE(next_char) && !is.na(next_char) && next_char == "/") {
          comment <- "//"
          self$index <- self$index + 2  # Skip both slashes
          char <- self$get_char_at()

          while (!isFALSE(char) && !is.na(char) && !(char %in% termination_characters)) {
            comment <- paste0(comment, char)
            self$index <- self$index + 1
            char <- self$get_char_at()
          }
          self$log(paste0("Found line comment: ", comment))
          return("")
        }

        # Handle block comment starting with /*
        else if (!isFALSE(next_char) && !is.na(next_char) && next_char == "*") {
          comment <- "/*"
          self$index <- self$index + 2  # Skip '/*'

          while (TRUE) {
            char <- self$get_char_at()
            if (isFALSE(char) || is.na(char)) {
              self$log("Reached end-of-string while parsing block comment; unclosed block comment.")
              break
            }
            comment <- paste0(comment, char)
            self$index <- self$index + 1

            if (endsWith(comment, "*/")) {
              break
            }
          }
          self$log(paste0("Found block comment: ", comment))
          return("")
        }

        else {
          # Skip standalone '/' characters that are not part of a comment
          # to avoid getting stuck in an infinite loop
          self$index <- self$index + 1
          return("")
        }
      }

      return("")  # pragma: no cover equivalent
    },
    #' @description Get character at specified position relative to current index
    #' @param count Integer. The position to get character from
    #' @keywords internal
    get_char_at = function(count = 0L) {
      pos <- self$index + count
      # R string indexing starts at 1; return FALSE if out of bounds
      if (pos < 1L || pos > nchar(self$json_str)) {
        return(FALSE)                # Matches the Python version's Literal[False]
      }
      substr(self$json_str, pos, pos)
    },

    #' Skip whitespace characters starting from specified position
    #'
    #' @description Skip whitespace characters starting from specified position
    #' @details Efficiently skips over whitespace characters (spaces, tabs, newlines, etc.)
    #'   starting from the calculated position. Can operate in two modes: updating the
    #'   main parsing index or performing lookahead without affecting main position.
    #' @param idx Integer. The index position to start skipping whitespaces
    #' @param move_main_index Logical. Whether to move the main index pointer
    #' @return Integer. The updated index position
    #' @keywords internal
    skip_whitespaces_at = function(idx = 0L, move_main_index = TRUE) {
      # Get string length
      len_json <- nchar(self$json_str)
      pos <- self$index + idx
      while (pos <= len_json) {
        ch <- substr(self$json_str, pos, pos)
        # Use grepl("^\\s$", ch) in R to check for whitespace
        if (!grepl("^\\s$", ch, perl = TRUE)) break
        if (isTRUE(move_main_index)) {
          self$index <- self$index + 1L
        } else {
          idx <- idx + 1L
        }
        pos <- pos + 1L
      }
      idx
    },
#' @description Search for a target character starting from specified offset
#' @details Safely searches forward through the JSON string to find the next
#'   occurrence of the specified character. Includes error handling for
#'   bounds violations and empty character scenarios.
#' @param character Character. The target character to skip to
#' @param idx Integer. Starting index position
#' @keywords internal
    skip_to_character = function(character, idx = 0) {
      tryCatch({
        char <- substr(self$json_str, self$index + idx, self$index + idx)
        if (nchar(char) == 0) {
          return(idx)
        }
      }, error = function(e) {
        return(idx)
      })

      # Continue searching until we find the target character
      while (char != character) {
        idx <- idx + 1

        tryCatch({
          char <- substr(self$json_str, self$index + idx, self$index + idx)
          if (nchar(char) == 0) {
            return(idx)
          }
        }, error = function(e) {
          return(idx)
        })
      }

      return(idx)
    },
#' Internal logging function with context window
#'
#' @description Internal logging function with context window
#' @details Creates detailed log entries that include both the log message
#'   and a context window showing surrounding characters from the JSON string.
#'   The context window helps with debugging by showing the parsing environment.
#' @param text Character. The text message to log
#' @return NULL (invisible, called for side effects)
#' @keywords internal
    .log = function(text) {
      window <- 10
      start <- max(self$index - window, 1)
      end <- min(self$index + window, nchar(self$json_str))
      context <- substr(self$json_str, start, end)

      self$logger <- append(self$logger, list(list(
        text = text,
        context = context
      )))
    }
                      )
)
