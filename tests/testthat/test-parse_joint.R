# TDD/BDD: parse_joint() — Parse LLM response into a fuzzy-join mapping table
# Contract: given a raw LLM response string and key names, returns a 2-column
#   data.frame mapping values from key1 to key2.
# Contract: strips markdown fences, extracts CSV block, detects/ensures header.
# Contract: errors with context on unparseable input.

describe("parse_joint", {

  describe("CSV format handling", {

    it("should strip markdown code fences before CSV parsing", {
      csv_content <- "code01,January\ncode02,Feb\ncode04,May"
      mock_response <- paste0("```csv\n", csv_content, "\n```")

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
      expect_equal(colnames(result), c("id", "month"))
      expect_equal(result[["id"]], c("code01", "code02", "code04"))
      expect_equal(result[["month"]], c("January", "Feb", "May"))
    })

    it("should parse plain CSV without markdown fences (DeepSeek-style)", {
      mock_response <- "01,January\n02,Feb\n04,May"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
      expect_equal(colnames(result), c("id", "month"))
      expect_equal(result[["id"]], c("01", "02", "04"))
      expect_equal(result[["month"]], c("January", "Feb", "May"))
    })

    it("should extract CSV from response with surrounding explanatory text", {
      mock_response <- paste0(
        "Here are the matches I found:\n\n",
        "01,January\n02,Feb\n04,May\n\n",
        "All matches look correct."
      )

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
      expect_equal(nrow(result), 3)
      expect_equal(result[["id"]], c("01", "02", "04"))
      expect_equal(result[["month"]], c("January", "Feb", "May"))
    })

    it("should extract CSV from markdown-fenced response with surrounding text", {
      mock_response <- paste0(
        "Sure, here's the mapping:\n\n",
        "```\n01,January\n02,Feb\n04,May\n```\n\n",
        "Done."
      )

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
      expect_equal(result[["id"]], c("01", "02", "04"))
    })

    it("should parse CSV with quoted fields containing commas", {
      mock_response <- '"New York, NY",USA\n"Paris, France",France'

      result <- parse_joint(mock_response, key1 = "city", key2 = "country")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 2)
      expect_equal(result[["city"]], c("New York, NY", "Paris, France"))
      expect_equal(result[["country"]], c("USA", "France"))
    })

    it("should parse CSV with trailing blank lines", {
      mock_response <- "01,January\n02,Feb\n\n\n"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "02"))
      expect_equal(result[["month"]], c("January", "Feb"))
    })

    it("should parse CSV with leading/trailing spaces on lines", {
      mock_response <- "  01 , January  \n  02 , Feb  "

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "02"))
      expect_equal(result[["month"]], c("January", "Feb"))
    })

  })

  describe("header detection", {

    it("should prepend key1,key2 header when CSV has no header", {
      mock_response <- "01,January\n02,Feb\n04,May"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(colnames(result), c("id", "month"))
      expect_equal(nrow(result), 3)
    })

    it("should keep existing header unchanged when it matches key1,key2", {
      mock_response <- "id,month\n01,January\n02,Feb\n04,May"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(colnames(result), c("id", "month"))
      expect_equal(nrow(result), 3)
      expect_equal(result[["id"]], c("01", "02", "04"))
    })

    it("should detect header case-insensitively", {
      mock_response <- "ID,MONTH\n01,January\n02,Feb\n04,May"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(colnames(result), c("id", "month"))
      expect_equal(nrow(result), 3)
      expect_equal(result[["id"]], c("01", "02", "04"))
    })

    it("should prepend header when first line is data, not a header match", {
      mock_response <- "01,January"

      result <- parse_joint(mock_response, key1 = "code", key2 = "name")

      expect_equal(colnames(result), c("code", "name"))
      expect_equal(nrow(result), 1)
    })

    it("should strip a quoted header row (audit #2)", {
      # Given: the LLM echoes the key names as a double-quoted CSV header line
      mock_response <- '"id","month"\n01,January\n02,Feb'
      # When:  parse_joint(mock_response, key1 = "id", key2 = "month") —
      #   manual README workflow, no x_keys/y_keys
      # Then:  the quoted header is consumed as a header — exactly 2 data
      #   rows, and neither "id" nor "month" appears as a data value
      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "02"))
      expect_equal(result[["month"]], c("January", "Feb"))
    })

    it("should strip a generic header row (audit #2)", {
      # Given: the LLM invents a generic two-column header instead of key1,key2
      mock_response <- "value1,value2\n01,January\n02,Feb"
      # When:  parse_joint(mock_response, key1 = "id", key2 = "month")
      # Then:  the generic header is consumed — exactly 2 data rows
      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "02"))
      expect_equal(result[["month"]], c("January", "Feb"))
    })

    it("should return clean rows for the manual no-x_keys workflow (audit #2)", {
      # Given: a realistic manual-workflow response (generic header + data)
      mock_response <- paste0(
        "column1,column2\n",
        "code01,January\ncode02,Feb\ncode04,May"
      )
      # When:  parse_joint(mock_response, key1 = "id", key2 = "month") with
      #   no x_keys/y_keys
      # Then:  exactly the 3 data rows — no header-like garbage leaked
      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(nrow(result), 3)
      expect_equal(result[["id"]], c("code01", "code02", "code04"))
      expect_false(any(grepl("column1", result[["id"]], fixed = TRUE)))
    })

  })

  describe("error handling", {

    it("should error when response contains no comma-separated lines", {
      mock_response <- "I cannot match these items. They are too different."

      expect_error(
        parse_joint(mock_response, key1 = "key", key2 = "key"),
        "Failed to parse LLM response as CSV"
      )
    })

    it("should error when LLM returns empty response", {
      mock_response <- ""

      expect_error(
        parse_joint(mock_response, key1 = "key", key2 = "key"),
        "Failed to parse LLM response as CSV"
      )
    })

    it("should error when response is only markdown fences", {
      mock_response <- "```\n```"

      expect_error(
        parse_joint(mock_response, key1 = "key", key2 = "key"),
        "Failed to parse LLM response as CSV"
      )
    })

  })

  describe("country name fuzzy matching (mocked)", {

    it("should match China to common abbreviations", {
      mock_response <- paste0(
        "CHN,China\n",
        "CN,China\n",
        "PRC,China\n",
        "CHINA,China"
      )

      result <- parse_joint(mock_response, key1 = "code", key2 = "name")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 4)
      expect_equal(colnames(result), c("code", "name"))
      expect_equal(result[["code"]], c("CHN", "CN", "PRC", "CHINA"))
      expect_true(all(result[["name"]] == "China"))
    })

    it("should match Congo-Kinshasa to RD Congo / RDC", {
      mock_response <- paste0(
        "Congo-Kinshasa,RD Congo\n",
        "DR Congo,RD Congo\n",
        "RDC,RD Congo\n",
        "Congo (Democratic Republic),RD Congo"
      )

      result <- parse_joint(mock_response, key1 = "input", key2 = "target")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 4)
      expect_true(all(result[["target"]] == "RD Congo"))
    })

    it("should match multi-language country names", {
      mock_response <- paste0(
        "Germany,Germany\n",
        "Allemagne,Germany\n",
        "Alemania,Germany\n",
        "德国,Germany"
      )

      result <- parse_joint(mock_response, key1 = "name", key2 = "std")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 4)
      expect_equal(colnames(result), c("name", "std"))
      expect_true(all(result[["std"]] == "Germany"))
    })

    it("should leave empty when no reasonable match exists", {
      mock_response <- paste0(
        "France,France\n",
        "Narnia,\n",
        "Germany,Germany"
      )

      result <- parse_joint(mock_response, key1 = "a", key2 = "b")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 3)
      expect_equal(result[["a"]], c("France", "Narnia", "Germany"))
      expect_equal(result[["b"]][result[["a"]] == "Narnia"], NA_character_)
    })

  })

  describe("fabrication defense", {

    it("should filter out LLM-fabricated key1 values not in x_keys", {
      mock_response <- "01,January\n99,Feb\n04,May"

      # warning 1: the fabricated "99" row; warning 2: E2 feedback for the
      # never-mapped "02" (asserted in the E2 describe block)
      msgs <- character(0)
      result <- withCallingHandlers(
        parse_joint(mock_response, key1 = "id", key2 = "month",
                    x_keys = c("01", "02", "04")),
        warning = function(w) {
          msgs <<- c(msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_match(msgs[1], "fabricated", fixed = TRUE)

      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "04"))
      expect_equal(result[["month"]], c("January", "May"))
    })

    it("should filter out LLM-fabricated key2 values not in y_keys", {
      mock_response <- "01,January\n02,FakeCity\n04,May"

      # warning 1: the fabricated "FakeCity" row; warning 2: E2 feedback for
      # the never-mapped "Feb" (asserted in the E2 describe block)
      msgs <- character(0)
      result <- withCallingHandlers(
        parse_joint(mock_response, key1 = "id", key2 = "month",
                    y_keys = c("January", "Feb", "May")),
        warning = function(w) {
          msgs <<- c(msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_match(msgs[1], "fabricated", fixed = TRUE)

      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "04"))
    })

    it("should NOT treat empty key2 (NA) as fabrication", {
      mock_response <- "01,January\n02,\n04,May"

      # the never-mapped "Feb" raises the single E2 unmatched-key warning
      # (its content is asserted in the E2 describe block)
      expect_warning(
        result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                              y_keys = c("January", "Feb", "May")),
        "not matched",
        fixed = TRUE
      )

      expect_equal(nrow(result), 3)
      expect_true(is.na(result[["month"]][result[["id"]] == "02"]))
    })

    it("should filter both fabricated key1 and fabricated key2 on different rows", {
      mock_response <- paste0(
        "01,January\n",
        "02,FakeCity\n",
        "99,Feb\n",
        "04,May\n",
        "05,\n"
      )

      result <- suppressWarnings(
        parse_joint(mock_response, key1 = "id", key2 = "month",
                    x_keys = c("01", "02", "04", "05"),
                    y_keys = c("January", "Feb", "May"))
      )

      # "02,FakeCity" dropped (fabricated key2)
      # "99,Feb" dropped (fabricated key1)
      # Kept: "01,January", "04,May", "05,NA"
      expect_equal(nrow(result), 3)
      expect_equal(result[["id"]], c("01", "04", "05"))
    })

    it("should be backward compatible when x_keys and y_keys are NULL", {
      mock_response <- "01,January\n02,Feb\n04,May"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month")

      expect_equal(nrow(result), 3)
      expect_equal(colnames(result), c("id", "month"))
    })

    it("should return 0-row data.frame when all values are fabricated", {
      mock_response <- "99,Foo\n88,Bar"

      # warning 1: both x values fabricated; warnings 2-3: E2 feedback for
      # the fully unmatched x and y key sets (covered in the E2 block)
      msgs <- character(0)
      result <- withCallingHandlers(
        parse_joint(mock_response, key1 = "id", key2 = "month",
                    x_keys = c("01", "02"),
                    y_keys = c("January", "Feb")),
        warning = function(w) {
          msgs <<- c(msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_match(msgs[1], "fabricated", fixed = TRUE)

      expect_equal(nrow(result), 0)
      expect_s3_class(result, "data.frame")
      expect_equal(colnames(result), c("id", "month"))
    })

    it("should compare key values after coercing to character", {
      mock_response <- "1.5,Jan\n3,Mar"

      # numeric x_keys — should still match their character representation;
      # the never-mapped "5" (x) and "Feb" (y) raise the E2 unmatched-key
      # warnings (covered in the E2 describe block)
      result <- suppressWarnings(
        parse_joint(mock_response, key1 = "weight", key2 = "month",
                    x_keys = c(1.5, 3, 5),
                    y_keys = c("Jan", "Feb", "Mar"))
      )

      expect_equal(nrow(result), 2)
      expect_equal(result[["weight"]], c("1.5", "3"))
    })

  })

  describe("NA key echo handling (audit #5)", {

    it("should accept an 'NA' echo for key1 when the x key column has actual NA", {
      # Given: x key column contains actual NA (tbl2md renders it as "NA" in
      #   the prompt); the LLM faithfully echoes the row as "NA,Feb"
      mock_response <- "01,January\nNA,Feb"
      # When:  parse_joint(mock_response, key1 = "id", key2 = "month",
      #          x_keys = c("01", NA))
      # Then:  no fabrication warning; 2 rows kept; key1 = c("01", "NA") —
      #   the string "NA" is kept as-is (matching happens downstream in merge)
      expect_no_warning(
        result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                              x_keys = c("01", NA))
      )
      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "NA"))
      expect_equal(result[["month"]], c("January", "Feb"))
    })

    it("should accept an 'NA' echo for key2 when the y key column has actual NA", {
      # Given: y key column contains actual NA; the LLM echoes "02,NA"
      mock_response <- "01,January\n02,NA"
      # When:  parse_joint(mock_response, key1 = "id", key2 = "month",
      #          y_keys = c("January", NA))
      # Then:  no fabrication warning; 2 rows; month = c("January", "NA")
      expect_no_warning(
        result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                              y_keys = c("January", NA))
      )
      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "02"))
      expect_equal(result[["month"]], c("January", "NA"))
    })

    it("should keep normal values passing the whitelist (regression)", {
      # Given: no NA anywhere; a faithful echo of one row
      # When:  parse_joint("01,January", key1 = "id", key2 = "month",
      #          x_keys = "01", y_keys = "January")
      # Then:  no warning; 1 row
      expect_no_warning(
        result <- parse_joint("01,January", key1 = "id", key2 = "month",
                              x_keys = "01", y_keys = "January")
      )
      expect_equal(nrow(result), 1)
      expect_equal(result[["id"]], "01")
      expect_equal(result[["month"]], "January")
    })

    it("should still flag 'NA' as fabrication when the key set has no actual NA and no literal 'NA'", {
      # Given: x key column has neither actual NA nor a literal "NA" value
      # When:  parse_joint("01,January\nNA,Feb", ..., x_keys = c("01", "02"))
      # Then:  the fabrication warning fires first and the "NA" row is
      #   dropped (1 row left) — the whitelist must not be weakened
      #   unconditionally (root P0.3); the never-mapped "02" additionally
      #   raises the E2 unmatched-key warning (covered in the E2 block)
      msgs <- character(0)
      result <- withCallingHandlers(
        parse_joint("01,January\nNA,Feb", key1 = "id", key2 = "month",
                    x_keys = c("01", "02")),
        warning = function(w) {
          msgs <<- c(msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_match(msgs[1], "fabricated", fixed = TRUE)

      expect_equal(nrow(result), 1)
      expect_equal(result[["id"]], "01")
      expect_equal(result[["month"]], "January")
    })

    it("should treat 'NA' as a literal value when the key set has both actual NA and a literal 'NA'", {
      # Given: x key column = c("01", NA, "NA") — e.g. country code "NA" plus
      #   a genuine missing value
      # When:  parse_joint("01,January\nNA,Feb", ..., x_keys = c("01", NA, "NA"))
      # Then:  no warning; 2 rows; the echoed "NA" stays the literal string
      expect_no_warning(
        result <- parse_joint("01,January\nNA,Feb", key1 = "id", key2 = "month",
                              x_keys = c("01", NA, "NA"))
      )
      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "NA"))
      expect_equal(result[["month"]], c("January", "Feb"))
    })

    it("should accept an 'NA' echo when the key set has only a literal 'NA' (regression)", {
      # Given: x key column = c("01", "NA") — the literal "NA" is already
      #   whitelisted by exact string match
      # When:  parse_joint("01,January\nNA,Feb", ..., x_keys = c("01", "NA"))
      # Then:  no warning; 2 rows
      expect_no_warning(
        result <- parse_joint("01,January\nNA,Feb", key1 = "id", key2 = "month",
                              x_keys = c("01", "NA"))
      )
      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "NA"))
    })

  })

})

# TDD/BDD: parse_joint() — unmatched-key feedback (E2, 261006: maintainer
#   approved separate warnings for the x and the y side)
# Contract: after the fabrication whitelist filtering, keys present in
#   x_keys (or y_keys) but absent from the parsed key1 (key2) column raise
#   one warning per side naming the column, the count ("N of M ... not
#   matched") and up to 5 sample values; data rows are never deleted by
#   this feedback — it is informational only.
# Contract: actual NA keys are not expected to be matched (the prompt tells
#   the LLM to leave unmappable cells empty) and never count as unmatched.
# Contract: with no key sets provided (manual workflow) no new warnings
#   appear; the existing fabrication warnings keep firing unchanged.

describe("unmatched-key feedback (E2)", {

  it("warns with a count when some x keys are not matched", {
    # Given: response "01,January\n04,May" (02 was never mapped)
    # When:  parse_joint(..., key1 = "id", key2 = "month",
    #          x_keys = c("01", "02", "04"))
    # Then:  one warning containing "1 of 3", the column name 'id' and
    #   "not matched"; the result is unchanged — 2 rows, no rows deleted by
    #   this feedback
    expect_warning(
      result <- parse_joint("01,January\n04,May", key1 = "id", key2 = "month",
                            x_keys = c("01", "02", "04")),
      "1 of 3 key value(s) in 'id' were not matched",
      fixed = TRUE
    )
    expect_equal(nrow(result), 2)
    expect_equal(result[["id"]], c("01", "04"))
    expect_equal(result[["month"]], c("January", "May"))
  })

  it("warns for unmatched y keys", {
    # Given: response "01,January\n02,Feb" with y_keys c("January", "Feb", "May")
    # When:  parse_joint(..., key1 = "id", key2 = "month", y_keys = ...)
    # Then:  one warning containing "1 of 3" and the column name 'month'
    expect_warning(
      result <- parse_joint("01,January\n02,Feb", key1 = "id", key2 = "month",
                            y_keys = c("January", "Feb", "May")),
      "1 of 3 key value(s) in 'month' were not matched",
      fixed = TRUE
    )
    expect_equal(nrow(result), 2)
  })

  it("emits separate warnings for the x and the y side", {
    # Given: response "01,January" with x_keys c("01", "02") and
    #   y_keys c("January", "Feb")
    # When:  parse_joint(...) with both key sets
    # Then:  exactly two warnings — one naming 'id', one naming 'month'
    msgs <- character(0)
    result <- withCallingHandlers(
      parse_joint("01,January", key1 = "id", key2 = "month",
                  x_keys = c("01", "02"), y_keys = c("January", "Feb")),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(msgs, 2)
    expect_match(msgs[1], "in 'id' were not matched", fixed = TRUE)
    expect_match(msgs[2], "in 'month' were not matched", fixed = TRUE)
    expect_equal(nrow(result), 1)
  })

  it("stays silent when every key is matched (regression)", {
    # Given: a full mapping response with x_keys and y_keys provided
    # When:  parse_joint(...) with both key sets
    # Then:  expect_no_warning
    expect_no_warning(
      result <- parse_joint("01,January\n02,Feb\n04,May",
                            key1 = "id", key2 = "month",
                            x_keys = c("01", "02", "04"),
                            y_keys = c("January", "Feb", "May"))
    )
    expect_equal(nrow(result), 3)
  })

  it("stays silent when x_keys/y_keys are not provided (regression)", {
    # Given: the manual workflow — no key sets, any parseable response
    # When:  parse_joint(...)
    # Then:  expect_no_warning
    expect_no_warning(
      result <- parse_joint("column1,column2\ncode01,January\ncode02,Feb",
                            key1 = "id", key2 = "month")
    )
    expect_equal(nrow(result), 2)
  })

  it("does not count actual NA keys as unmatched", {
    # Given: x_keys = c("01", NA); response "01,January" — the NA key is not
    #   expected to be mapped (the LLM is told to leave cells empty)
    # When:  parse_joint(..., key1 = "id", key2 = "month", x_keys = c("01", NA))
    # Then:  expect_no_warning; result keeps 1 row
    expect_no_warning(
      result <- parse_joint("01,January", key1 = "id", key2 = "month",
                            x_keys = c("01", NA))
    )
    expect_equal(nrow(result), 1)
  })

  it("lists up to 5 sample values and an ellipsis beyond that", {
    # Given: 7 x keys, none of them mapped by the response
    # When:  parse_joint(...) with x_keys = the 7 values
    # Then:  the warning contains "7 of 7" and "... and 2 more"
    keys <- c("k1", "k2", "k3", "k4", "k5", "k6", "k7")
    msgs <- character(0)
    result <- withCallingHandlers(
      parse_joint("zz,foo", key1 = "id", key2 = "month",
                  x_keys = keys, y_keys = "foo"),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_true(any(grepl("7 of 7 key value(s) in 'id' were not matched",
                          msgs, fixed = TRUE)))
    expect_true(any(grepl("... and 2 more", msgs, fixed = TRUE)))
    expect_equal(nrow(result), 0)
  })

  it("counts unmatched keys only after fabrication filtering", {
    # Given: response "99,Feb" with x_keys c("01", "02") and
    #   y_keys c("January", "Feb")
    # When:  parse_joint(...) with both key sets
    # Then:  warnings include the fabricated drop for x ("99") plus the
    #   unmatched feedback for both sides; the fabricated value 99 never
    #   appears in an unmatched message; result keeps 0 rows
    msgs <- character(0)
    result <- withCallingHandlers(
      parse_joint("99,Feb", key1 = "id", key2 = "month",
                  x_keys = c("01", "02"), y_keys = c("January", "Feb")),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(msgs, 3)
    expect_match(msgs[1], "fabricated", fixed = TRUE)
    expect_match(msgs[2], "2 of 2 key value(s) in 'id' were not matched", fixed = TRUE)
    expect_match(msgs[3], "2 of 2 key value(s) in 'month' were not matched", fixed = TRUE)
    expect_false(grepl("99", msgs[2], fixed = TRUE))
    expect_false(grepl("99", msgs[3], fixed = TRUE))
    expect_equal(nrow(result), 0)
  })

})
