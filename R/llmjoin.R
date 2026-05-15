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
    stop("provide a valid name for the input vector.")
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
    "Match each item in column 1 to the most similar item in column 2. ",
    "The columns may differ in spelling, language, or formatting.\n\n",
    "Rules:\n",
    "- Match with highest possible accuracy.\n",
    "- If no reasonable match exists, leave the cell empty.\n",
    "- Do NOT invent or fabricate any data.\n\n",
    "Output ONLY a CSV table with two columns, no explanation or markdown fences.\n",
    "Use comma as delimiter. Quote cells containing commas.\n\n",
    "Example:\n",
    "column 1: | id |\n| --- |\n| 01 |\n| 02 |\n",
    "column 2: | name |\n| --- |\n| Alice |\n| Bob |\n",
    "Output: 01,Alice\\n02,Bob\n\n",
    "column 1:\n", tbl2md(x), "\n\ncolumn 2:\n", tbl2md(y)
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
  
  csv_text <- gsub("```\\w*\\n?|\\n?```", "", llm_response)

  tryCatch(
    read.csv(text = csv_text, stringsAsFactors = FALSE),
    error = \(e) stop(
      "Failed to parse LLM response as CSV.\n",
      "Error: ", e$message, "\n",
      "Raw response:\n", llm_response
    )
  )
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

  err_rows <- err_mtx[,1] %>%
    gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", .) %>%
    paste(collapse = "|")

  .joint[!grepl(err_rows, .joint[[1]]),]
}

#' Fuzzy join with LLM
#'
#' @param x a `data.frame` to be join on the lhs.
#' @param y a `data.frame` to be join on the rhs.
#' @param key1 string, name of the key column of data.frame `x` waiting for paring.
#' @param key2 string, name of the key column of data.frame `y` waiting for paring.
#' @param check logical, ask LLM to validate the joint. Default FALSE.
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
llm_join <- function(x, y, key1, key2, check = FALSE, ...) {

  joint <- build_joint(x, y, key1, key2, ...)
  if (check) joint <- check_joint(joint, ...)

  Reduce(\(x, y) merge(x, y, all.x = TRUE), list(x, joint, y))

}




