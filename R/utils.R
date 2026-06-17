#' @importFrom httr add_headers POST status_code content
#' @importFrom jsonlite fromJSON toJSON
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

utils::globalVariables(c(".", ".providers"))