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
#' The prompt pins what \code{\link{parse_joint}()} can check: plain CSV lines (no markdown tables, tabs or fences), exactly one line per value of column 1, and every value copied character for character including its leading zeros -- a model that normalises "01" to "1" otherwise has to be re-matched by recovery.
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
  n1 <- if (is.data.frame(x)) nrow(x) else length(x)
  x_md <- if (is.data.frame(x)) tbl2md(x) else tbl2md(x, nm = "column 1")
  y_md <- if (is.data.frame(y)) tbl2md(y) else tbl2md(y, nm = "column 2")

  paste0(
    "Match each item in column 1 to the most similar item in column 2. ",
    "The columns may differ in spelling, language, or formatting.\n\n",
    "Rules:\n",
    "- Match with highest possible accuracy.\n",
    "- Copy each value EXACTLY as written in its column: same characters, same case, ",
    "same leading zeros. Do not expand, shorten, translate, reformat or re-round any value.\n",
    "- If no reasonable match exists, leave the cell empty.\n",
    "- Do NOT invent or fabricate any data.\n\n",
    "Output format (MUST follow exactly):\n",
    "- Output exactly ", n1, " lines: one line per value of column 1, in the same order. ",
    "Never repeat a column 1 value and never merge two values into one line.\n",
    "- Each line is one mapping pair: value_from_column1,value_from_column2\n",
    "- Two columns separated by a single comma, no spaces around comma.\n",
    "- If a value itself contains a comma, wrap that value in double quotes.\n",
    "- Plain CSV only: no markdown tables (no | characters), no tabs, no bullet lists, ",
    "no row numbering.\n",
    "- No header row, no markdown fences, no extra text before or after.\n",
    "- Do NOT include any explanation, only the CSV lines.\n\n",
    "Example:\n",
    "column 1: | id |\n| --- |\n| 01 |\n| 02 |\n",
    "column 2: | name |\n| --- |\n| Alice |\n| Bob |\n",
    "Correct output:\n01,Alice\n02,Bob\n\n",
    "column 1:\n",
    x_md,
    "\n\ncolumn 2:\n",
    y_md
  )
}

#' Canonical form of a key value, used only to recover a normalised echo
#'
#' Lowercases, trims, and reduces a fully numeric value to its numeric form
#' ("01" and "1.50" become "1" and "1.5"). Never used to accept a value the
#' original data does not contain -- see .recover_keys().
#' @noRd
.canon_key <- function(x) {
  s <- trimws(as.character(x))
  out <- tolower(s)
  is_num <- nzchar(s) & grepl("^-?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?$", s)
  if (any(is_num)) {
    out[is_num] <- as.character(as.numeric(s[is_num]))
  }
  out
}

#' Map parsed values back to originals when exactly one key matches
#'
#' A model that answers "1" for the input key "01", or "january" for "January",
#' is normalising rather than inventing, and the row is useless if the value is
#' dropped: the later merge() cannot match it. Recovery is allowed only when the
#' canonical form matches a single original key; values that match several keys
#' are reported as ambiguous and left for the caller to drop, so this never
#' guesses between two real candidates.
#' @noRd
.recover_keys <- function(vals, keys) {
  mapped <- vals
  recovered <- character(0)
  ambiguous <- character(0)
  cand <- which(!is.na(vals) & !(vals %in% keys))
  if (length(cand) == 0L) {
    return(list(mapped = mapped, recovered = recovered, ambiguous = ambiguous))
  }

  kc <- .canon_key(keys)
  for (i in cand) {
    idx <- which(kc == .canon_key(vals[i]))
    if (length(idx) == 1L) {
      mapped[i] <- keys[idx]
      recovered <- c(recovered, paste0("'", vals[i], "' -> '", keys[idx], "'"))
    } else if (length(idx) > 1L) {
      ambiguous <- c(ambiguous, paste0("'", vals[i], "'"))
    }
  }

  list(mapped = mapped, recovered = recovered, ambiguous = ambiguous)
}

#' Count CSV fields per line, ignoring commas inside double quotes
#'
#' Counts separators rather than splitting: strsplit() drops a trailing empty
#' field, and "02," is the prompt's own way of saying "no match for 02" -- it
#' must stay a two-field line.
#' @noRd
.count_fields <- function(lines) {
  vapply(
    lines,
    function(l) 1L + nchar(gsub("[^,]", "", gsub('"[^"]*"', "", l))),
    integer(1)
  )
}

#' Keep only rows whose key column matches the original data, after recovery
#' @noRd
.filter_key_column <- function(result, key, keys) {
  keys_chr <- as.character(unique(keys))
  allowed <- c(keys_chr, NA_character_)
  # audit #5: tbl2md renders actual NA keys as the string "NA"; a faithful
  # LLM echo is not fabrication. Lift "NA" only when the key set has actual
  # NA, so the whitelist is not weakened unconditionally (P0.3).
  if (anyNA(keys_chr)) allowed <- c(allowed, "NA")

  vals <- result[[key]]
  rec <- .recover_keys(vals, keys_chr)
  if (length(rec$recovered) > 0) {
    shown <- utils::head(rec$recovered, 5)
    message(
      "Recovered ", length(rec$recovered),
      " normalised value(s) in '", key, "' to the original data: ",
      paste(shown, collapse = ", "),
      if (length(rec$recovered) > 5) {
        paste0(" ... and ", length(rec$recovered) - 5, " more")
      }, "."
    )
    vals <- rec$mapped
    result[[key]] <- vals
  }

  if (length(rec$ambiguous) > 0) {
    warning(
      "Dropped ", length(rec$ambiguous), " value(s) in '", key,
      "' whose normalised form matches several original keys (ambiguous: ",
      paste(utils::head(rec$ambiguous, 5), collapse = ", "),
      "). The LLM must copy key values exactly as written."
    )
    result <- result[!(vals %in% rec$ambiguous), , drop = FALSE]
  }

  unknown <- setdiff(result[[key]], allowed)
  if (length(unknown) > 0) {
    shown <- utils::head(unknown, 5)
    msg <- sprintf(
      "Dropped %d LLM-fabricated value(s) in '%s' not found in original data: %s",
      length(unknown), key,
      paste(sQuote(shown), collapse = ", ")
    )
    if (length(unknown) > 5) {
      msg <- paste0(msg, sprintf(" ... and %d more", length(unknown) - 5))
    }
    if (sum(result[[key]] %in% unknown) == nrow(result)) {
      # Nothing survived: a refusal or a prose answer that merely contains a
      # comma looks like one bad row, so name that cause instead of leaving the
      # reader with a list of values that happen not to be keys.
      msg <- paste0(
        msg, ". No parsed line matched the original ", key,
        " values; the model may have answered in prose or ignored the ",
        "two-per-line format."
      )
    }
    warning(msg)
    result <- result[!(result[[key]] %in% unknown), , drop = FALSE]
  }

  result
}

#' Parse LLM response into a fuzzy-join joint data.frame
#'
#' Strips markdown fences, extracts the longest consecutive block of
#' comma-separated lines, drops lines that do not hold exactly two fields
#' (naming them in a warning), ensures a header row matching `key1,key2`
#' is present, and parses the CSV into a 2-column data.frame. Exact duplicate
#' rows are dropped with a message, and a `key1` value carrying several mappings
#' is reported because it multiplies the joined rows.
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
#'   A value differing from exactly one original key only by case, padding
#'   whitespace or numeric form ("1" for "01") is recovered to that original and
#'   announced by message, since dropping it would lose a correct match; a value
#'   whose normalised form fits several originals is dropped as ambiguous.
#'   Abbreviation changes ("Feb" answered as "February") are not recoverable --
#'   the prompt asks the model to copy values exactly. The literal string "NA"
#'   is accepted when the key set contains actual NA values.
#' @param y_keys character vector of unique key values from the right-hand-side
#'   data.frame. Handled exactly like `x_keys`, except that an empty cell (the
#'   prompt's way of saying "no reasonable match") is kept as `NA` and never
#'   counts as a fabrication.
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

  # A joint is two columns by definition, so a line whose field count is not 2 is
  # malformed: an unquoted comma inside a value, a stray trailing comma, or a
  # sentence that merely happens to contain a comma. Naming the offending line
  # keeps the diagnosis honest -- before this, such a response collapsed to 0 rows
  # and the whitelist blamed the *valid* key values for being fabrications.
  fields <- .count_fields(csv_lines)
  bad <- which(fields != 2L)
  if (length(bad) == length(csv_lines)) {
    stop(
      "LLM response does not look like a two-column mapping table: none of the ",
      length(csv_lines), " line(s) has exactly 2 comma-separated fields. ",
      "Expected one 'column1_value,column2_value' pair per line.\n",
      "Line(s) seen: ", paste(sQuote(utils::head(csv_lines, 3)), collapse = " | "),
      "\nRaw response:\n", llm_response
    )
  }
  if (length(bad) > 0) {
    warning(
      "Ignored ", length(bad), " malformed line(s) in the LLM response: ",
      paste0(
        "line ", utils::head(bad, 3), " has ", utils::head(fields[bad], 3),
        " fields", collapse = "; "
      ),
      if (length(bad) > 3) paste0("; ... and ", length(bad) - 3, " more"),
      ". The prompt asks for exactly two comma-separated values per line."
    )
    csv_lines <- csv_lines[-bad]
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

  n_parsed <- nrow(result)
  is_dup <- duplicated(result)
  if (any(is_dup)) {
    message(
      "Dropped ", sum(is_dup), " exact duplicate row(s) from the parsed mapping ",
      "table; identical rows would double those keys in the joined result."
    )
    result <- result[!is_dup, , drop = FALSE]
  }

  if (!is.null(x_keys)) {
    result <- .filter_key_column(result, key1, x_keys)
  }
  if (!is.null(y_keys)) {
    result <- .filter_key_column(result, key2, y_keys)
  }

  # One key1 value mapped to several key2 values multiplies the joined rows. The
  # rows are kept -- choosing between them would be our fabrication, not the
  # model's -- but the join's cardinality changes silently otherwise.
  if (nrow(result) > 0) {
    per_key1 <- table(result[[key1]], useNA = "no")
    multi <- per_key1[per_key1 > 1L]
    if (length(multi) > 0) {
      warning(
        "Parsed joint has more than one mapping for ", length(multi),
        " value(s) of '", key1, "': ",
        paste0("'", utils::head(names(multi), 5), "'", collapse = ", "),
        if (length(multi) > 5) {
          paste0(" ... and ", length(multi) - 5, " more")
        },
        ". The join will duplicate those rows; the prompt asks for one mapping per value."
      )
    }
  }

  # E2: report original keys the LLM never mapped. Computed here — after
  # both whitelist filters — because the y-side row drops change the x-side
  # outcome, so both counts must see the final result. Actual NA keys are
  # excluded: the prompt tells the LLM to leave unmappable cells empty, so
  # an unmapped NA is documented behaviour. Informational only — no rows
  # are removed.
  # A line shortfall only means "truncated" when some rows did match; if the
  # whole response was rejected, the format diagnosis above is the useful one.
  hint_lines <- if (nrow(result) > 0) n_parsed else NULL
  if (!is.null(x_keys)) .warn_unmatched(x_keys, result[[key1]], key1, hint_lines)
  if (!is.null(y_keys)) .warn_unmatched(y_keys, result[[key2]], key2, hint_lines)

  result
}

#' Warn about original key values the LLM never mapped
#'
#' Internal helper for parse_joint(): compares the expected non-NA key values
#' against the parsed key column after recovery and the fabrication filters, and
#' warns with the count and up to 5 sample values. Informational only -- never
#' drops rows. When the response returned fewer mapping lines than there are key
#' values, the shortfall is most likely a truncated reply, so the warning also
#' names that cause and the fix (`max_tokens`), which the count alone hides.
#' @noRd
.warn_unmatched <- function(expected_keys, parsed_values, key, lines_parsed = NULL) {
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
  if (!is.null(lines_parsed) && lines_parsed < length(expected)) {
    msg <- paste0(
      msg, " Only ", lines_parsed, " mapping line(s) came back for ",
      length(expected), " key value(s), so the response may be truncated: ",
      "retry with a larger .max_tokens."
    )
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
#' @examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))
#'   build_joint(
#'     x = data.frame(x = c("01","02","04")),
#'     y = data.frame(y = c("January","Feb","May")),
#'     key1 = "x", key2 = "y"
#'   )
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
#' @examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))
#'   x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
#'   y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))
#'
#'   llm_join(x, y, key1 = "id", key2 = "month")
llm_join <- function(x, y, key1, key2, ...) {
  joint <- build_joint(x, y, key1, key2, ...)
  result <- merge(x, joint, by = key1, all.x = TRUE)
  # If x already contains a column named key2, merge() renamed joint's key2
  # column to <key2>.y; detect the actual name before the second merge.
  k2 <- key2
  if (!k2 %in% names(result)) k2 <- paste0(key2, ".y")
  merge(result, y, by.x = k2, by.y = key2, all.x = TRUE)
}
