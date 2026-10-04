# TDD/BDD: tbl2md() — factor input handling (audit #3)
# Contract: factor vectors render exactly like character vectors — never an
#   empty table.

describe("tbl2md", {

  it("renders a factor vector like a character vector", {
    # Given: a factor vector (levels deliberately not in display order)
    f <- factor(c("a", "b"), levels = c("b", "a"))
    # When:  tbl2md(f, nm = "k")
    # Then:  output identical to tbl2md(as.character(f), nm = "k"); contains
    #   the "k" header and data rows "| a |", "| b |"
    got <- tbl2md(f, nm = "k")
    expected <- tbl2md(as.character(f), nm = "k")
    expect_identical(got, expected)
    expect_match(got, "| k |", fixed = TRUE)
    expect_match(got, "| a |", fixed = TRUE)
    expect_match(got, "| b |", fixed = TRUE)
  })

  it("keeps single-column factor data.frame rendering (regression)", {
    # Given: a one-column data.frame of factors
    d <- data.frame(k = factor(c("a", "b")))
    # When:  tbl2md(d)
    # Then:  header from the column name; one markdown row per element
    got <- tbl2md(d)
    expect_match(got, "| k |", fixed = TRUE)
    expect_match(got, "| a |", fixed = TRUE)
    expect_match(got, "| b |", fixed = TRUE)
  })

  it("renders NA elements of a factor as NA cells", {
    # Given: a factor vector containing NA
    f <- factor(c("a", NA))
    # When:  tbl2md(f, nm = "k")
    # Then:  the NA element renders as a "| NA |" row
    got <- tbl2md(f, nm = "k")
    expect_match(got, "| NA |", fixed = TRUE)
  })

})
