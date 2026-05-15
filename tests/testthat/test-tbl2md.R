# TDD/BDD: tbl2md() — Convert R objects to Markdown tables
# Contract: given any data.frame or named vector, returns a markdown table string.
# Contract: given a vector without a name, errors.

describe("tbl2md", {

  it("should convert a data.frame to a markdown table", {
    # Given: a data.frame with known content
    df <- data.frame(name = c("Alice", "Bob"), score = c(10, 20))

    # When: converting to markdown
    result <- tbl2md(df)

    # Then: result is a single markdown-formatted string
    expect_type(result, "character")
    expect_length(result, 1)

    # Then: contains header row with column names
    expect_match(result, "| name | score |")

    # Then: contains separator row
    expect_match(result, "| --- | --- |")

    # Then: contains data rows
    expect_match(result, "| Alice | 10 |")
    expect_match(result, "| Bob | 20 |")
  })

  it("should convert a single-column data.frame to a markdown table", {
    # Given: a data.frame with one column
    df <- data.frame(id = c("01", "02", "04"))

    # When: converting to markdown
    result <- tbl2md(df)

    # Then: result is valid markdown
    expect_type(result, "character")
    expect_match(result, "| id |")
    expect_match(result, "| 01 |")
    expect_match(result, "| 02 |")
    expect_match(result, "| 04 |")
  })

  it("should convert a vector to a markdown table when given a name", {
    # Given: a vector with a column name
    vec <- c("January", "Feb", "May")

    # When: converting to markdown with explicit name
    result <- tbl2md(vec, nm = "month")

    # Then: result contains the named header and values
    expect_match(result, "| month |")
    expect_match(result, "| January |")
    expect_match(result, "| Feb |")
    expect_match(result, "| May |")
  })

  it("should error when given a vector without a name", {
    # Given: a vector
    vec <- c("a", "b", "c")

    # When & Then: calling tbl2md without nm should error
    expect_error(tbl2md(vec), "provide a valid name for the input vector")
  })

  it("should produce newline-separated rows", {
    # Given: a multi-row data.frame
    df <- data.frame(x = c("a", "b"))

    # When: converting
    result <- tbl2md(df)

    # Then: rows are separated by newlines
    parts <- strsplit(result, "\n")[[1]]
    expect_equal(length(parts), 4)  # header, separator, 2 data rows
  })

})
