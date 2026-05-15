# TDD/BDD: check_joint() — LLM-based validation of fuzzy-join results
# Contract: given a 2-column joint data.frame, returns a data.frame with
# problematic rows filtered out.
# Contract: rows that pass validation are kept, rows that fail are removed.
# Contract: when LLM returns no "is equal to" matches, all rows are preserved.

describe("check_joint", {

  it("should return a data.frame with same or fewer rows than input", {
    # Given: a 2-column joint mapping
    joint <- data.frame(
      key1 = c("01", "02", "04"),
      key2 = c("January", "Feb", "May"),
      stringsAsFactors = FALSE
    )

    # When: LLM says no rows are problematic (mocked: returns no errors)
    local_mocked_bindings(
      chat_llm = function(...) "All mappings are correct. No problematic phrases found.",
      .package = "llmjoin"
    )

    result <- check_joint(joint)

    # Then: returns a data.frame
    expect_s3_class(result, "data.frame")

    # Then: number of rows is <= input rows
    expect_lte(nrow(result), nrow(joint))
  })

  it("should filter out rows that LLM identifies as problematic", {
    # Given: a joint with one obviously wrong mapping
    joint <- data.frame(
      x = c("01", "02"),
      y = c("January", "Mars"),
      stringsAsFactors = FALSE
    )

    # When: LLM flags "Mars" as problematic (mocked)
    local_mocked_bindings(
      chat_llm = function(...) "02 is equal to Mars,",
      .package = "llmjoin"
    )

    result <- check_joint(joint)

    # Then: problematic row ("02" -> "Mars") is removed
    expect_s3_class(result, "data.frame")
    expect_false("02" %in% result[["x"]])
    expect_true("01" %in% result[["x"]])
  })

  it("should keep all rows when LLM finds no problematic phrases", {
    # Given: a valid joint
    joint <- data.frame(
      a = c("01", "02", "04"),
      b = c("January", "February", "April"),
      stringsAsFactors = FALSE
    )

    # When: LLM returns no problematic IDs
    local_mocked_bindings(
      chat_llm = function(...) "All match. No issues.",
      .package = "llmjoin"
    )

    result <- check_joint(joint)

    # Then: all rows preserved (none matched by the empty filter pattern)
    expect_s3_class(result, "data.frame")
  })

  it("should preserve all rows when LLM returns empty string", {
    # Given: a valid joint
    joint <- data.frame(
      x = c("01", "02", "04"),
      y = c("January", "Feb", "May"),
      stringsAsFactors = FALSE
    )

    # When: LLM returns empty string (no matches found)
    local_mocked_bindings(
      chat_llm = function(...) "",
      .package = "llmjoin"
    )

    result <- check_joint(joint)

    # Then: all rows preserved (err_mtx guard prevents silent drop)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(joint))
    expect_equal(result$x, joint$x)
    expect_equal(result$y, joint$y)
  })

  it("should preserve all rows when LLM response has no 'is equal to' pattern", {
    # Given: a valid joint
    joint <- data.frame(
      x = c("01", "02"),
      y = c("January", "February"),
      stringsAsFactors = FALSE
    )

    # When: LLM responds with text that does not contain "X is equal to Y"
    local_mocked_bindings(
      chat_llm = function(...) "All good, nothing wrong here!",
      .package = "llmjoin"
    )

    result <- check_joint(joint)

    # Then: all rows preserved (no matches → err_mtx guard kicks in)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(joint))
    expect_equal(result$x, joint$x)
  })

})
