#' Turning R dataframe into markdown table
#'
#' @param tbl a `data.frame` object or a vector.
#' @param nm character, only used if `tbl` is a vector.
#' @returns markdown style table string lines
#' @export
#'
#' @examples tbl2md(iris)
tbl2md <- function(tbl, nm = NULL) {
  nm <- if (is.data.frame(tbl)) names(tbl) else nm

  if (is.null(nm)) {
    stop("provide a validate name for the input vector.")
  }

  header <- paste0("| ", paste(nm, collapse = " | "), " |")

  rule <- paste0("| ", paste(rep("---", length(names(tbl))), collapse = " | "), " |")

  content <- if (is.data.frame(tbl) & length(tbl) > 1) {
    do.call(paste, c(tbl, sep = " | "))
  } else if (is.data.frame(tbl) & length(tbl) == 1) {
    tbl[[1]]
  } else if (is.vector(tbl)) {
    tbl
  }

  content <- paste0("| ", content, " |")

  paste0(header, "\n", rule, "\n", paste(content, collapse = "\n")) %>% return()
}

#' Generate connector prompt
#'
#' Generate a prompt to guide the LLM in generating a joint for dataframe joinning, leveraging the two key columns from the tables to be connected.
#' By far(2025/04/10), DeepSeek R1 and gpt-4.1-mini showed the best result, other LLMs might fabricate non-existing data in the result.
#' @param x 1-column `data.frame` or vector of characters, left hand side of the join
#' @param y 1-column `data.frame` or vector of characters, right hand side of the join
#'
#' @returns a connectors `data.frame` for joining.
#' @export
#'
#' @examples
#' # run in Rstudio script panel:
#' \dontrun{
#'   joint_prompt(
#'     data.frame(x = c("01","02","04")),
#'     data.frame(y = c("January","Feb","May"))
#'   )
#' }
joint_prompt <- function(x, y) {

  paste0(
    "I will provide you with two columns. Please perform a SQL-style FULL JOIN operation to combine the two columns into a single table based on their relationships.\r\n",
    "Note that the two columns may not match exactly or could even be in different languages.\r\n",
    "I command you to match carefully with the highest possible accuracy. Leave any unmatched entries blank, and do not generate any data that does not exist in the original tables.\r\n",
    "Repeat: NEVER generate ANY data that does not exist in the tables provided; otherwise, someone could get hurt as a result.\r\n",
    "The final output must be a table in CSV format, with no instructions or additional content included.\r\n\n",
    "column 1:\r\n", tbl2md(x), "\r\n\ncolumn 2:\r\n", tbl2md(y)
  ) %>% return()

}

#' build fuzzy-join joint
#'
#' @param x a `data.frame` to be join on the lhs.
#' @param y a `data.frame` to be join on the rhs.
#' @param key1 string, name of the key column of data.frame `x` waiting for paring.
#' @param key2 string, name of the key column of data.frame `y` waiting for paring.
#' @param ... extra params passed to `chat_llm()`
#'
#' @returns the Fuzzy-joined `data.frame`
#' @export
#' 
#' @examples 
#' \dontrun{
#'   build_joint(
#'     x = data.frame(x = c("01","02","04")),
#'     y = data.frame(y = c("January","Feb","May")),
#'     key1 = "x", key2 = "y"
#'   )
#' }
build_joint <- function(x, y, key1, key2, ...) {
  llm_response <- joint_prompt(unique(x[key1]), unique(y[key2])) %>%
    chat_llm(...)
  
  gsub("```|csv", "", llm_response) %>% read_csv()
}

#' ask LLM to check if the built joint is correct.
#' @param .joint 2-column data.frame, the built joint.
#' @param ... extra params passed to `chat_llm()`
#' @export
check_joint <- function(.joint, ...) {
  
  llm_response <- paste0(
    "Below are some phrases for judgment. ",
    "Please identify any that may be problematic, ", 
    "filter them out, and return only the problematic phrases. ",
    "Do not include any unexpected information: \n\n",
    paste0(.joint[[1]], " is equal to ", .joint[[2]], ",\n") %>% paste0(collapse = "")
  ) %>% chat_llm(...)

  err_mtx <- strsplit(llm_response, " is equal to ") %>% do.call(rbind, .) 
  
  err_rows <- err_mtx[,1] %>% paste(collapse = "|")

  .joint[!grepl(err_rows, .joint[[1]]),]
}

#' Fuzzy join with LLM
#'
#' @param x a `data.frame` to be join on the lhs.
#' @param y a `data.frame` to be join on the rhs.
#' @param key1 string, name of the key column of data.frame `x` waiting for paring.
#' @param key2 string, name of the key column of data.frame `y` waiting for paring.
#' @param ... extra params passed to `chat_llm()`
#' 
#' @returns the Fuzzy-joined `data.frame`
#' @export
#'
#' @examples 
#' \dontrun{
#'   x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
#'   y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))
#' 
#'   llm_join(x, y, key1 = "id", key2 = "month", model = "gpt-4.1-mini")
#' }
llm_join <- function(x, y, key1, key2, ...) {

  joint <- build_joint(x, y, key1, key2, ...) %>% check_joint(...)

  Reduce(\(x, y) left_join(x, y), list(x, joint, y)) %>% return()

}




