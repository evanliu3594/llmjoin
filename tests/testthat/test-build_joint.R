# TDD/BDD: build_joint() — key argument validation (audit #8)
# Contract: key1 must be a single non-NA string naming a column of x; key2
#   likewise for y; x and y must be data.frames. Violations error naming the
#   argument, the offending value and the available columns — never the opaque
#   "undefined columns selected" from the old x[key1] subsetting.
# Contract: validation runs before the LLM call, so a misspelled key never
#   reaches chat_llm(); all LLM calls are stubbed, no network access.

describe("build_joint", {

  it("errors naming 'key1' when key1 is misspelled", {
    # Given: x = data.frame(id = c("01","02"), value = c(10,20)),
    #   y = data.frame(month = c("January","Feb"), amount = c(100,200));
    #   key1 = "id2" (typo)
    # When:  build_joint(x, y, key1 = "id2", key2 = "month")
    # Then:  error text contains "key1" and "not found in x" and lists the
    #   available columns; it does NOT contain "undefined columns selected";
    #   the chat_llm stub is never invoked (validation precedes the LLM call)
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    err <- tryCatch(
      build_joint(x, y, key1 = "id2", key2 = "month"),
      error = function(e) conditionMessage(e)
    )
    expect_match(err, "'key1'", fixed = TRUE)
    expect_match(err, "'id2'", fixed = TRUE)
    expect_match(err, "not found in x", fixed = TRUE)
    expect_match(err, "Available columns", fixed = TRUE)
    expect_match(err, "value", fixed = TRUE)
    expect_match(err, "Check the 'key1' argument", fixed = TRUE)
    expect_false(grepl("undefined columns selected", err, fixed = TRUE))
    expect_false(grepl("chat_llm must not be called", err, fixed = TRUE))
  })

  it("errors naming 'key2' when key2 is misspelled", {
    # Given: the same x/y; key2 = "month2" (typo)
    # When:  build_joint(x, y, key1 = "id", key2 = "month2")
    # Then:  error text contains "key2" and "not found in y" and lists the
    #   available columns of y
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    err <- tryCatch(
      build_joint(x, y, key1 = "id", key2 = "month2"),
      error = function(e) conditionMessage(e)
    )
    expect_match(err, "'key2'", fixed = TRUE)
    expect_match(err, "month2", fixed = TRUE)
    expect_match(err, "not found in y", fixed = TRUE)
    expect_match(err, "Available columns", fixed = TRUE)
    expect_match(err, "amount", fixed = TRUE)
    expect_match(err, "Check the 'key2' argument", fixed = TRUE)
  })

  it("reports key1 first when both keys are misspelled", {
    # Given: key1 = "id2" and key2 = "month2", both wrong
    # When:  build_joint(x, y, key1 = "id2", key2 = "month2")
    # Then:  the error names 'key1' (fixed check order) and does not name
    #   'key2'
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    err <- tryCatch(
      build_joint(x, y, key1 = "id2", key2 = "month2"),
      error = function(e) conditionMessage(e)
    )
    expect_match(err, "'key1'", fixed = TRUE)
    expect_match(err, "not found in x", fixed = TRUE)
    expect_false(grepl("key2", err, fixed = TRUE))
  })

  it("errors naming the argument when a key is not a single string", {
    # Given: key1 = c("id", "value") (length 2) and key1 = 123 (non-character)
    # When:  build_joint(x, y, key1 = <each>, key2 = "month")
    # Then:  each call errors with text naming 'key1' and stating the single
    #   non-NA string requirement
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    for (bad in list(c("id", "value"), 123)) {
      err <- tryCatch(
        build_joint(x, y, key1 = bad, key2 = "month"),
        error = function(e) conditionMessage(e)
      )
      expect_match(err, "'key1'", fixed = TRUE,
                   info = paste("bad key1 class:", paste(class(bad), collapse = "/")))
      expect_match(err, "single non-NA string", fixed = TRUE,
                   info = paste("bad key1 class:", paste(class(bad), collapse = "/")))
    }
  })

  it("errors naming 'x' when x is not a data.frame", {
    # Given: x = c("01", "02") (plain vector)
    # When:  build_joint(x, y, key1 = "id", key2 = "month")
    # Then:  error text names 'x' and requires a data.frame
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    err <- tryCatch(
      build_joint(c("01", "02"), y, key1 = "id", key2 = "month"),
      error = function(e) conditionMessage(e)
    )
    expect_match(err, "'x'", fixed = TRUE)
    expect_match(err, "data.frame", fixed = TRUE)
    expect_match(err, "character", fixed = TRUE)
    expect_match(err, "Check the 'x' argument", fixed = TRUE)
  })

  it("errors naming 'y' when y is not a data.frame", {
    # Given: y = c("January", "Feb") (plain vector)
    # When:  build_joint(x, y, key1 = "id", key2 = "month")
    # Then:  error text names 'y' and requires a data.frame
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))

    err <- tryCatch(
      build_joint(x, c("January", "Feb"), key1 = "id", key2 = "month"),
      error = function(e) conditionMessage(e)
    )
    expect_match(err, "'y'", fixed = TRUE)
    expect_match(err, "data.frame", fixed = TRUE)
    expect_match(err, "character", fixed = TRUE)
    expect_match(err, "Check the 'y' argument", fixed = TRUE)
  })

  it("returns a 2-column joint when keys are valid (regression)", {
    # Given: valid x/y and key1 = "id", key2 = "month"; the LLM stub returns
    #   "01,January\n02,Feb"
    # When:  build_joint(x, y, key1 = "id", key2 = "month")
    # Then:  a 2-row data.frame with columns id and month; rows map
    #   01→January and 02→Feb
    local_mocked_bindings(
      chat_llm = function(...) "01,January\n02,Feb",
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    res <- build_joint(x, y, key1 = "id", key2 = "month")
    expect_s3_class(res, "data.frame")
    expect_identical(names(res), c("id", "month"))
    expect_identical(nrow(res), 2L)
    expect_identical(res$id, c("01", "02"))
    expect_identical(res$month, c("January", "Feb"))
  })

  it("llm_join surfaces the same clear error for a misspelled key", {
    # Given: key1 = "id2" via llm_join (which delegates to build_joint)
    # When:  llm_join(x, y, key1 = "id2", key2 = "month")
    # Then:  the same named error — not "undefined columns selected"
    local_mocked_bindings(
      chat_llm = function(...) stop("chat_llm must not be called"),
      .package = "llmjoin"
    )
    x <- data.frame(id = c("01", "02"), value = c(10, 20))
    y <- data.frame(month = c("January", "Feb"), amount = c(100, 200))

    err <- tryCatch(
      llm_join(x, y, key1 = "id2", key2 = "month"),
      error = function(e) conditionMessage(e)
    )
    expect_match(err, "'key1'", fixed = TRUE)
    expect_match(err, "not found in x", fixed = TRUE)
    expect_false(grepl("undefined columns selected", err, fixed = TRUE))
    expect_false(grepl("chat_llm must not be called", err, fixed = TRUE))
  })

})
