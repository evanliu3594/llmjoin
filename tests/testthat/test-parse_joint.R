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

      expect_warning(
        result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                              x_keys = c("01", "02", "04")),
        "fabricated"
      )

      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "04"))
      expect_equal(result[["month"]], c("January", "May"))
    })

    it("should filter out LLM-fabricated key2 values not in y_keys", {
      mock_response <- "01,January\n02,FakeCity\n04,May"

      expect_warning(
        result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                              y_keys = c("January", "Feb", "May")),
        "fabricated"
      )

      expect_equal(nrow(result), 2)
      expect_equal(result[["id"]], c("01", "04"))
    })

    it("should NOT treat empty key2 (NA) as fabrication", {
      mock_response <- "01,January\n02,\n04,May"

      result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                            y_keys = c("January", "Feb", "May"))

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

      expect_warning(
        result <- parse_joint(mock_response, key1 = "id", key2 = "month",
                              x_keys = c("01", "02"),
                              y_keys = c("January", "Feb")),
        "fabricated"
      )

      expect_equal(nrow(result), 0)
      expect_s3_class(result, "data.frame")
      expect_equal(colnames(result), c("id", "month"))
    })

    it("should compare key values after coercing to character", {
      mock_response <- "1.5,Jan\n3,Mar"

      # numeric x_keys — should still match their character representation
      result <- parse_joint(mock_response, key1 = "weight", key2 = "month",
                            x_keys = c(1.5, 3, 5),
                            y_keys = c("Jan", "Feb", "Mar"))

      expect_equal(nrow(result), 2)
      expect_equal(result[["weight"]], c("1.5", "3"))
    })

  })

})
