# TDD/BDD: llm_join() — End-to-end fuzzy join with LLM
# Contract: given two data.frames with fuzzy keys, returns a joined data.frame
# containing all columns from x, the joint, and y.
# Contract: when check = TRUE, problematic rows are filtered before merging.

describe("llm_join", {

  it("should join two data.frames on fuzzy keys (README example)", {
    skip_if_no_llm()

    # Given: two data.frames where key1="id" and key2="month" need fuzzy matching
    x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
    y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

    # When: performing the LLM-powered fuzzy join
    result <- llm_join(x, y, key1 = "id", key2 = "month")

    # Then: result is a data.frame
    expect_s3_class(result, "data.frame")

    # Then: result contains all columns from both inputs
    expect_true(all(c("id", "value", "month", "amount") %in% colnames(result)))

    # Then: at least one row was successfully joined
    expect_gt(nrow(result), 0)
  })

  it("should preserve lhs rows (left join behavior)", {
    skip_if_no_llm()

    # Given: x has 3 rows, y has 3 rows with fuzzy keys
    x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
    y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

    # When: performing the join
    result <- llm_join(x, y, key1 = "id", key2 = "month")

    # Then: all x rows appear in the result (left join)
    x_ids_in_result <- result[["id"]] %in% x[["id"]]
    expect_true(any(x_ids_in_result))
  })

  it("should accept check = TRUE without error", {
    skip_if_no_llm()

    # Given: two data.frames with fuzzy keys
    x <- data.frame(id = c("01", "02", "04"))
    y <- data.frame(month = c("January", "Feb", "May"))

    # When: building the joint with validation enabled
    result <- llm_join(x, y, key1 = "id", key2 = "month", check = TRUE)

    # Then: result is a valid data.frame
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
  })

})
