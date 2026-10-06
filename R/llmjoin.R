#' Convert a data frame to a markdown table
#'
#' @param tbl a data.frame object or a vector.
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
  } else if (is.vector(tbl) || is.factor(tbl)) {
    tbl
  }

  content <- paste0("| ", content, " |")

  paste0(header, "\n", rule, "\n", paste(content, collapse = "\n"))
}

#' Generate connector prompt
#'
#' Generate a prompt to guide the LLM in generating a joint for data frame joining, leveraging the two key columns from the tables to be connected.
#' As of 2025/04/10, DeepSeek R1 and gpt-4.1-mini showed the best result; other LLMs might fabricate non-existent data in the result.
#' @param x 1-column data.frame or vector of characters, left hand side of the join
#' @param y 1-column data.frame or vector of characters, right hand side of the join
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
#' When `x_keys`/`y_keys` are provided, original keys that never appear in
#' the parsed result raise an informational warning naming the column, the
#' count and up to 5 sample values; rows are never dropped by this feedback.
#'
#' @param llm_response character, raw response from the LLM.
#' @param key1 string, name of the lhs key column.
#' @param key2 string, name of the rhs key column.
#' @param x_keys character vector of unique key values from the left-hand-side
#'   data.frame. If provided, values in the parsed key1 column not found in
#'   this set are considered LLM fabrications and dropped with a warning.
#'   The literal string "NA" is accepted when the key set contains actual NA
#'   values.
#' @param y_keys character vector of unique key values from the right-hand-side
#'   data.frame. If provided, values in the parsed key2 column not found in
#'   this set (excluding NA for "no match") are considered LLM fabrications
#'   and dropped with a warning. The literal string "NA" is accepted when the
#'   key set contains actual NA values.
#'
#' @returns a 2-column data.frame mapping values from key1 to key2.
#' @export
#'
#' @examples
#' parse_joint("01,January\n02,Feb\n04,May", key1 = "id", key2 = "month")
parse_joint <- function(llm_response, key1, key2, x_keys = NULL, y_keys = NULL) {
  txt <- gsub("```\\w*\\n?|\\n?```", "", llm_response)
  lines <- strsplit(txt, "\n")[[1]]
  has_comma <- grepl(",", lines, fixed = TRUE) & nchar(trimws(lines)) > 0

  if (!any(has_comma)) {
    stop("Failed to parse LLM response as CSV.\nRaw response:\n", llm_response)
  }

  r <- rle(has_comma)
  best <- which.max(r$lengths * r$values)
  start <- sum(r$lengths[seq_len(best - 1)]) + 1
  end <- start + r$lengths[best] - 1

  csv_lines <- lines[start:end]

  first_raw <- trimws(strsplit(csv_lines[1], ",")[[1]])
  first_low <- tolower(gsub('^"(.*)"$', "\\1", first_raw))

  is_key_header <- length(first_raw) == 2 &&
    first_low[1] == tolower(key1) &&
    first_low[2] == tolower(key2)

  generic_rx <- "^(value|column|col|field|var|key|attr|attribute|item|entry)[_.-]?0*[0-9]+$"
  is_generic_header <- length(first_raw) == 2 &&
    length(unique(first_low)) == 2 &&
    all(grepl(generic_rx, first_low))

  if (is_key_header || is_generic_header) {
    csv_lines <- csv_lines[-1]
  }
  csv_lines <- c(paste(key1, key2, sep = ","), csv_lines)

  result <- tryCatch(
    utils::read.csv(
      text = paste(csv_lines, collapse = "\n"),
      col.names = c(key1, key2),
      colClasses = "character",
      header = TRUE,
      stringsAsFactors = FALSE,
      na.strings = "",
      strip.white = TRUE,
      check.names = FALSE
    ),
    error = \(e) {
      stop(
        "Failed to parse LLM response as CSV.\n",
        "Error: ",
        e$message,
        "\n",
        "Raw response:\n",
        llm_response
      )
    }
  )

  if (!is.null(x_keys)) {
    x_keys <- as.character(unique(x_keys))
    allowed <- c(x_keys, NA_character_)
    # audit #5: tbl2md renders actual NA keys as the string "NA"; a faithful
    # LLM echo is not fabrication. Lift "NA" only when the key set has actual
    # NA, so the whitelist is not weakened unconditionally (P0.3).
    if (anyNA(x_keys)) allowed <- c(allowed, "NA")
    unknown <- setdiff(result[[key1]], allowed)
    if (length(unknown) > 0) {
      shown <- utils::head(unknown, 5)
      msg <- sprintf(
        "Dropped %d LLM-fabricated value(s) in '%s' not found in original data: %s",
        length(unknown), key1,
        paste(sQuote(shown), collapse = ", ")
      )
      if (length(unknown) > 5) {
        msg <- paste0(msg, sprintf(" ... and %d more", length(unknown) - 5))
      }
      warning(msg)
      result <- result[!result[[key1]] %in% unknown, , drop = FALSE]
    }
  }

  if (!is.null(y_keys)) {
    y_keys <- as.character(unique(y_keys))
    allowed <- c(y_keys, NA_character_)
    # audit #5: tbl2md renders actual NA keys as the string "NA"; a faithful
    # LLM echo is not fabrication. Lift "NA" only when the key set has actual
    # NA, so the whitelist is not weakened unconditionally (P0.3).
    if (anyNA(y_keys)) allowed <- c(allowed, "NA")
    unknown <- setdiff(result[[key2]], allowed)
    if (length(unknown) > 0) {
      shown <- utils::head(unknown, 5)
      msg <- sprintf(
        "Dropped %d LLM-fabricated value(s) in '%s' not found in original data: %s",
        length(unknown), key2,
        paste(sQuote(shown), collapse = ", ")
      )
      if (length(unknown) > 5) {
        msg <- paste0(msg, sprintf(" ... and %d more", length(unknown) - 5))
      }
      warning(msg)
      result <- result[!result[[key2]] %in% unknown, , drop = FALSE]
    }
  }

  # E2: report original keys the LLM never mapped. Computed here — after
  # both whitelist filters — because the y-side row drops change the x-side
  # outcome, so both counts must see the final result. Actual NA keys are
  # excluded: the prompt tells the LLM to leave unmappable cells empty, so
  # an unmapped NA is documented behaviour. Informational only — no rows
  # are removed.
  if (!is.null(x_keys)) .warn_unmatched(x_keys, result[[key1]], key1)
  if (!is.null(y_keys)) .warn_unmatched(y_keys, result[[key2]], key2)

  result
}

#' Warn about original key values the LLM never mapped
#'
#' Internal helper for parse_joint(): compares the expected non-NA key values
#' against the parsed key column after the fabrication filters and warns with
#' the count and up to 5 sample values. Informational only — never drops rows.
#' @noRd
.warn_unmatched <- function(expected_keys, parsed_values, key) {
  expected <- expected_keys[!is.na(expected_keys)]
  unmatched <- setdiff(expected, parsed_values)
  if (length(unmatched) == 0) {
    return(invisible(NULL))
  }
  shown <- utils::head(unmatched, 5)
  msg <- sprintf(
    "%d of %d key value(s) in '%s' were not matched by the LLM: %s. Rows with these keys will have no match after joining.",
    length(unmatched), length(expected), key,
    paste(sQuote(shown), collapse = ", ")
  )
  if (length(unmatched) > 5) {
    msg <- paste0(msg, sprintf(" ... and %d more", length(unmatched) - 5))
  }
  warning(msg)
}

#' Validate a key argument against a data.frame's column names
#'
#' Internal helper for build_joint(): stops with an actionable message when a
#' key argument is not a single non-NA string or does not name a column of df.
#' @noRd
.validate_key <- function(key, arg, df) {
  target <- if (arg == "key1") "x" else "y"
  if (!is.character(key) || length(key) != 1 || is.na(key)) {
    stop(
      "'", arg, "' must be a single non-NA string naming a column of ", target,
      ". Check the '", arg, "' argument."
    )
  }
  if (!key %in% names(df)) {
    stop(
      "'", arg, "' column '", key, "' not found in ", target,
      ". Available columns: ",
      if (length(names(df)) > 0) paste(sQuote(names(df)), collapse = ", ") else "(none)",
      ". Check the '", arg, "' argument or run names() on ", target, "."
    )
  }
}

#' Build a fuzzy-join joint data.frame via LLM
#'
#' @param x a data.frame to be joined on the lhs.
#' @param y a data.frame to be joined on the rhs.
#' @param key1 string, name of the key column of data.frame x waiting for
#'   pairing. Must name an existing column in x.
#' @param key2 string, name of the key column of data.frame y waiting for
#'   pairing. Must name an existing column in y.
#' @param ... extra params passed to chat_llm()
#'
#' @returns a 2-column data.frame mapping values from key1 to key2.
#' @export
#'
#' @examples
#' \donttest{
#'   build_joint(
#'     x = data.frame(x = c("01","02","04")),
#'     y = data.frame(y = c("January","Feb","May")),
#'     key1 = "x", key2 = "y"
#'   )
#' }
build_joint <- function(x, y, key1, key2, ...) {
  if (!is.data.frame(x)) {
    stop(
      "'x' must be a data.frame, not ", class(x)[1],
      ". Check the 'x' argument."
    )
  }
  if (!is.data.frame(y)) {
    stop(
      "'y' must be a data.frame, not ", class(y)[1],
      ". Check the 'y' argument."
    )
  }
  .validate_key(key1, "key1", x)
  .validate_key(key2, "key2", y)
  llm_response <- joint_prompt(unique(x[key1]), unique(y[key2])) |>
    chat_llm(...)
  parse_joint(llm_response, key1, key2,
              x_keys = unique(x[[key1]]),
              y_keys = unique(y[[key2]]))
}

#' Fuzzy join with LLM
#'
#' @param x a data.frame to be joined on the lhs.
#' @param y a data.frame to be joined on the rhs.
#' @param key1 string, name of the key column of data.frame x waiting for pairing.
#' @param key2 string, name of the key column of data.frame y waiting for pairing.
#' @param ... extra params passed to chat_llm()
#'
#' @returns the fuzzy-joined data.frame
#' @export
#'
#' @examples
#' \donttest{
#'   x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
#'   y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))
#'
#'   llm_join(x, y, key1 = "id", key2 = "month")
#' }
llm_join <- function(x, y, key1, key2, ...) {
  joint <- build_joint(x, y, key1, key2, ...)
  result <- merge(x, joint, by = key1, all.x = TRUE)
  # If x already contains a column named key2, merge() renamed joint's key2
  # column to <key2>.y; detect the actual name before the second merge.
  k2 <- key2
  if (!k2 %in% names(result)) k2 <- paste0(key2, ".y")
  merge(result, y, by.x = k2, by.y = key2, all.x = TRUE)
}
