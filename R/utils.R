#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom tidyr unite
#' @importFrom tidyselect everything
#' @importFrom readr read_csv write_lines clipboard
#' @importFrom dplyr left_join filter
#' @importFrom httr add_headers POST status_code content
#' @importFrom jsonlite fromJSON toJSON
NULL

utils::globalVariables(".")