# TDD/BDD: llm_join() — explicit merge keys (audit #1)
# Contract: llm_join() joins x and y through the LLM-built joint on key1
#   first, then key2 — never on the accidental intersection of same-named
#   columns. All LLM calls are stubbed; no network access.

describe("llm_join", {

  it("joins plain frames on explicit keys (regression)", {
    # Given: x and y share no same-named non-key columns
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))
    local_mocked_bindings(
      chat_llm = function(...) "01,January\n02,Feb",
      .package = "llmjoin"
    )
    # When:  llm_join(x, y, key1 = "id", key2 = "month")
    result <- llm_join(x, y, key1 = "id", key2 = "month")
    # Then:  2 rows; id/value/month/amount all present (merge puts the by
    #   column first); y data attached per the mapping; no all-NA rows
    expect_equal(nrow(result), 2)
    expect_setequal(names(result), c("id", "value", "month", "amount"))
    expect_false(anyNA(result$amount))
    expect_equal(result$amount[result$id == "01"], 100)
    expect_equal(result$amount[result$id == "02"], 200)
  })

  it("survives same-named non-key columns in x and y", {
    # Given: x and y both carry a non-key column named "value" (different data)
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), value = c(100, 200))
    local_mocked_bindings(
      chat_llm = function(...) "01,January\n02,Feb",
      .package = "llmjoin"
    )
    # When:  llm_join(x, y, key1 = "id", key2 = "month")
    result <- llm_join(x, y, key1 = "id", key2 = "month")
    # Then:  joined on key2 only; y's value data intact (value.x/value.y
    #   kept, both complete); no NAs caused by mis-joining on value
    expect_equal(nrow(result), 2)
    expect_setequal(names(result), c("id", "value.x", "month", "value.y"))
    expect_false(anyNA(result$value.x))
    expect_false(anyNA(result$value.y))
    expect_equal(result$value.x[result$id == "01"], 10)
    expect_equal(result$value.x[result$id == "02"], 20)
    expect_equal(sort(result$value.y), c(100, 200))
  })

  it("survives x already containing a column named key2", {
    # Given: x already has a column literally named "month" (== key2)
    x <- data.frame(id = c("01", "02"), month = c("q1", "q2"))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))
    local_mocked_bindings(
      chat_llm = function(...) "01,January\n02,Feb",
      .package = "llmjoin"
    )
    # When:  llm_join(x, y, key1 = "id", key2 = "month")
    result <- llm_join(x, y, key1 = "id", key2 = "month")
    # Then:  first merge on key1 only, second merge on joint's key2 mapping
    #   column; amounts 100/200 attached (no NA); x's original month column
    #   and the LLM mapping column both preserved under merge suffixes
    expect_equal(nrow(result), 2)
    expect_false(anyNA(result$amount))
    expect_equal(result$amount[result$id == "01"], 100)
    expect_equal(result$amount[result$id == "02"], 200)
    expect_true(all(c("month.x", "month.y") %in% names(result)))
    expect_equal(result$month.x[result$id == "01"], "q1")
    expect_equal(result$month.y[result$id == "01"], "January")
  })

  it("preserves rows whose key1 is NA", {
    # Given: x's key column contains NA; the LLM declines to map the NA key
    x <- data.frame(id = c("01", NA), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))
    local_mocked_bindings(
      chat_llm = function(...) "01,January",
      .package = "llmjoin"
    )
    # When:  llm_join(x, y, key1 = "id", key2 = "month")
    result <- llm_join(x, y, key1 = "id", key2 = "month")
    # Then:  2 rows; the NA-key row survives; its y-side cells are NA
    expect_equal(nrow(result), 2)
    expect_equal(sum(is.na(result$id)), 1)
    na_row <- is.na(result$id)
    expect_true(is.na(result$amount[na_row]))
    expect_equal(result$amount[!na_row], 100)
  })

})
