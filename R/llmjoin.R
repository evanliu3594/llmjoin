#' Convert a data frame to a markdown table
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

  rule <- paste0("| ", paste(rep("---", length(nm)), collapse = " | "), " |")

  content <- if (is.data.frame(tbl) & length(tbl) > 1) {
    do.call(paste, c(tbl, sep = " | "))
  } else if (is.data.frame(tbl) & length(tbl) == 1) {
    tbl[[1]]
  } else if (is.vector(tbl)) {
    tbl
  }

  content <- paste0("| ", content, " |")

  paste0(header, "\n", rule, "\n", paste(content, collapse = "\n"))
}

#' Generate connector prompt
#'
#' Generate a prompt to guide the LLM in generating a joint for data frame joining, leveraging the two key columns from the tables to be connected.
#' As of 2025/04/10, DeepSeek R1 and gpt-4.1-mini showed the best result; other LLMs might fabricate non-existent data in the result.
#' @param x 1-column `data.frame` or vector of characters, left hand side of the join
#' @param y 1-column `data.frame` or vector of characters, right hand side of the join
#'
#' @returns A character string containing the matching prompt.
#' @export
#'
#' @examples
#' joint_prompt(
#'   data.frame(x = c("01","02","04")),
#'   data.frame(y = c("January","Feb","May"))
#' )
joint_prompt <- function(x, y) {
  paste0(
    "Match each item in column 1 to the most similar item in column 2. ",
    "The columns may differ in spelling, language, or formatting.\n\n",
    "Rules:\n",
    "- Match with highest possible accuracy.\n",
    "- If no reasonable match exists, leave the cell empty.\n",
    "- Do NOT invent or fabricate any data.\n\n",
    "Output format (MUST follow exactly):\n",
    "- Each line is one mapping pair: value_from_column1,value_from_column2\n",
    "- Two columns separated by a single comma, no spaces around comma.\n",
    "- If a value itself contains a comma, wrap that value in double quotes.\n",
    "- No header row, no markdown fences, no extra text before or after.\n",
    "- Do NOT include any explanation, only the CSV lines.\n\n",
    "Example:\n",
    "column 1: | id |\n| --- |\n| 01 |\n| 02 |\n",
    "column 2: | name |\n| --- |\n| Alice |\n| Bob |\n",
    "Correct output:\n01,Alice\n02,Bob\n\n",
    "column 1:\n",
    tbl2md(x),
    "\n\ncolumn 2:\n",
    tbl2md(y)
  )
}

#' Parse LLM response into a fuzzy-join joint data.frame
#'
#' Strips markdown fences, extracts the longest consecutive block of
#' comma-separated lines, ensures a header row matching `key1,key2`
#' is present, and parses the CSV into a 2-column data.frame.
#'
#' @param llm_response character, raw response from the LLM.
#' @param key1 string, name of the lhs key column.
#' @param key2 string, name of the rhs key column.
#'
#' @returns a 2-column `data.frame` mapping values from key1 to key2.
#' @export
#'
#' @examples
#' parse_joint("01,January\n02,Feb\n04,May", key1 = "id", key2 = "month")
parse_joint <- function(llm_response, key1, key2) {
  txt <- gsub("```\\w*\\n?|\\n?```", "", llm_response)
  lines <- strsplit(txt, "\n")[[1]]
  has_comma <- grepl(",", lines, fixed = TRUE) & nchar(trimws(lines)) > 0

  if (!any(has_comma)) {
    stop("Failed to parse LLM response as CSV.\nRaw response:\n", llm_response)
  }

  r <- rle(has_comma)
  best <- which.max(r$lengths * r$values)
  start <- sum(r$lengths[seq_len(best - 1)]) + 1
  end   <- start + r$lengths[best] - 1

  csv_lines <- lines[start:end]

  first_fields <- tolower(trimws(strsplit(csv_lines[1], ",")[[1]]))
  has_header <- length(first_fields) == 2 &&
    first_fields[1] == tolower(key1) &&
    first_fields[2] == tolower(key2)

  if (!has_header) {
    csv_lines <- c(paste(key1, key2, sep = ","), csv_lines)
  }

  tryCatch(
    readr::read_csv(I(paste(csv_lines, collapse = "\n")),
                    col_names = c(key1, key2), skip = 1,
                    col_types = "cc", show_col_types = FALSE,
                    name_repair = "minimal"),
    error = \(e) stop(
      "Failed to parse LLM response as CSV.\n",
      "Error: ", e$message, "\n",
      "Raw response:\n", llm_response
    )
  )
}

#' Build a fuzzy-join joint data.frame via LLM
#'
#' @param x a `data.frame` to be joined on the lhs.
#' @param y a `data.frame` to be joined on the rhs.
#' @param key1 string, name of the key column of data.frame `x` waiting for pairing.
#' @param key2 string, name of the key column of data.frame `y` waiting for pairing.
#' @param ... extra params passed to `chat_llm()`
#'
#' @returns a 2-column `data.frame` mapping values from key1 to key2.
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
  llm_response <- joint_prompt(unique(x[key1]), unique(y[key2])) |>
    chat_llm(...)
  parse_joint(llm_response, key1, key2)
}

#' Fuzzy join with LLM
#'
#' @param x a `data.frame` to be joined on the lhs.
#' @param y a `data.frame` to be joined on the rhs.
#' @param key1 string, name of the key column of data.frame `x` waiting for pairing.
#' @param key2 string, name of the key column of data.frame `y` waiting for pairing.
#' @param ... extra params passed to `chat_llm()`
#'
#' @returns the fuzzy-joined `data.frame`
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
  joint <- build_joint(x, y, key1, key2, ...)
  result <- merge(x, joint, all.x = TRUE)
  merge(result, y, all.x = TRUE)
}
