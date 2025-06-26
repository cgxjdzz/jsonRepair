#' Repair (Possibly Broken) JSON
#'
#' Attempts to parse `json_str`.
#'
#'
#' @name repair_json
#' @title Repair Malformed JSON Strings
#' @description
#' Attempts to parse and repair malformed JSON strings. The function first tries
#' to parse the JSON using \code{jsonlite::fromJSON}. If that fails, it uses a
#' custom parser to attempt to repair common JSON formatting issues such as
#' missing quotes, trailing commas, or unescaped characters. The function can
#' return either R objects or a properly formatted JSON string.
#' @param json_str Character. Raw JSON text.
#' @param return_objects Logical. Return R objects if TRUE, or JSON text string
#' @param skip_json_loads Logical. Skip the fast attempt via \code{jsonlite::fromJSON}.
#' @param logging Logical. If save log.
#' @param file_conn Connection. If provided, read JSON from this connection.
#' @param stream_stable Logical. Stream-stable parsing mode.
#'
#' @return Either a JSON string (default) or R list/object.
#' @export
#' @importFrom jsonlite fromJSON toJSON
#' @examples
#' ## 1) Malformed JSON – request the repaired result as an R object
#' repair_json('{ "key": value, "key2": 1 "key3": null }',
#'             return_objects = TRUE)
#' #> $key
#' #> [1] "value"
#' #>
#' #> $key2
#' #> [1] 1
#' #>
#' #> $key3
#' #> "NULL"
#'
#' ## 2) Valid JSON – default behaviour returns a compact JSON string
#' repair_json('{"name": "John", "age": 30, "city": "New York"}')
#' #> {"name":"John","age":30,"city":"New York"}
#'
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

repair_json <- function(json_str = "",
                        return_objects = FALSE,
                        skip_json_loads = FALSE,
                        logging = FALSE,
                        file_conn = NULL,
                        stream_stable = FALSE) {

  if (!is.null(file_conn)) {
    json_str <- paste(readLines(file_conn, warn = FALSE), collapse = "\n")
  }
  if (!skip_json_loads) {
    parsed <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
    if (!is.null(parsed)) return(parsed)
  }
  parser = JSONParser$new(json_str, logging, stream_stable)
  parsed <-parser$parse()
  if (return_objects || logging) {
    return(parsed)
  } else {
    return(jsonlite::toJSON(parsed, auto_unbox = TRUE))
  }
}

