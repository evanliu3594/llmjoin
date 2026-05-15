# TDD/BDD: build_joint() — Build a fuzzy-join mapping table via LLM
# Contract: given two data.frames with key columns, returns a 2-column data.frame
# mapping values from key1 to key2.
# Contract: given a malformed LLM response, errors with context.

skip_if_no_llm <- function() {
  result <- test_llm_service_minimal()
  if (!isTRUE(result$success)) {
    skip(paste("LLM service not available:", result$message))
  }
}

describe("build_joint", {

  describe("successful joint building", {

    it("should return a 2-column data.frame mapping numeric IDs to month names", {
      skip_if_no_llm()

      # Given: two data.frames with fuzzy-matching keys (README example)
      x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
      y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

      # When: building the joint
      result <- build_joint(x, y, key1 = "id", key2 = "month")

      # Then: result is a data.frame
      expect_s3_class(result, "data.frame")

      # Then: result has exactly 2 columns (the mapping)
      expect_equal(ncol(result), 2)

      # Then: columns are named after the key columns
      expect_equal(colnames(result), c("id", "month"))

      # Then: result has at least one row (a mapping was found)
      expect_gt(nrow(result), 0)
    })

    it("should map all input key values to a target value", {
      skip_if_no_llm()

      # Given: keys where all inputs should match
      x <- data.frame(x = c("01", "02", "04"))
      y <- data.frame(y = c("January", "Feb", "May"))

      # When: building the joint
      result <- build_joint(x, y, key1 = "x", key2 = "y")

      # Then: all input values appear in the result
      expect_true(all(result[["x"]] %in% c("01", "02", "04")))

      # Then: all output values come from the target set
      expect_true(all(result[["y"]] %in% c("January", "Feb", "May")))
    })

    it("should handle single-row mappings", {
      skip_if_no_llm()

      # Given: each data.frame has only one key value
      x <- data.frame(key = "01")
      y <- data.frame(key = "January")

      # When: building the joint
      result <- build_joint(x, y, key1 = "key", key2 = "key")

      # Then: single row result — "01" maps to "January"
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
    })

    it("should pass extra arguments to chat_llm via ...", {
      skip_if_no_llm()

      # Given: data with explicit .temperature
      x <- data.frame(id = c("01", "02", "04"))
      y <- data.frame(month = c("January", "Feb", "May"))

      # When: building the joint with extra params
      result <- build_joint(x, y, key1 = "id", key2 = "month",
                            .temperature = 0)

      # Then: should succeed — extra args passed through to chat_llm
      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
    })

  })

  describe("error handling", {

    it("should error with context when LLM response is not valid CSV", {
      # Given: mock chat_llm to return unparseable CSV (unterminated quote)
      local_mocked_bindings(
        chat_llm = function(...) '"unclosed quote,value',
        .package = "llmjoin"
      )

      x <- data.frame(key = "A")
      y <- data.frame(key = "B")

      # When & Then: parsing should fail with "Failed to parse LLM response as CSV"
      expect_error(
        build_joint(x, y, key1 = "key", key2 = "key"),
        "Failed to parse LLM response as CSV"
      )
    })

    it("should strip markdown code fences from LLM response before CSV parsing", {
      # Given: a mocked LLM response that returns CSV inside markdown fences
      # Headers are set by build_joint from key1/key2
      csv_content <- "code01,January\ncode02,Feb\ncode04,May"
      mock_response <- paste0("```csv\n", csv_content, "\n```")

      local_mocked_bindings(
        chat_llm = function(...) mock_response,
        .package = "llmjoin"
      )

      x <- data.frame(id = c("code01", "code02", "code04"))
      y <- data.frame(month = c("January", "Feb", "May"))

      # When: building the joint
      result <- build_joint(x, y, key1 = "id", key2 = "month")

      # Then: markdown fences are stripped before CSV parsing
      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
      expect_equal(colnames(result), c("id", "month"))
      expect_equal(result[["id"]], c("code01", "code02", "code04"))
      expect_equal(result[["month"]], c("January", "Feb", "May"))
    })

  })

})
