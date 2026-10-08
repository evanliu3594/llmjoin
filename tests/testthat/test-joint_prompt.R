# TDD/BDD: joint_prompt() — what the prompt promises about the response shape
# Contract: the prompt must pin the three things parse_joint() can otherwise only
#   guess at — values copied character for character, exactly one line per
#   column-1 value (so a short response is provably incomplete), and plain CSV
#   rather than a markdown table or a tab-separated block.
# Contract: the documented "@param x 1-column data.frame or vector" form must
#   both work. Passing a vector used to reach tbl2md(x) with nm = NULL and fail
#   with "provide a valid name for the input vector." (measured 261008), so the
#   vector branch is pinned here and labelled internally as column 1 / column 2.
# Contract: the expected line count follows the column-1 input, so a one-column
#   data.frame and the equivalent vector promise the same number of lines.
# No network: this function only builds a string.

describe("joint_prompt", {

  describe("response-shape promises", {

    it("asks for values copied exactly as written", {
      # Given: two small key columns
      # When:  the prompt is built
      # Then:  it forbids reformatting, case changes, abbreviation expansion and
      #   leading-zero changes — the normalisations that made the model's correct
      #   answer look like a fabricated key to parse_joint()
      p <- joint_prompt(data.frame(x = c("01", "02")), data.frame(y = c("January", "Feb")))
      expect_match(p, "EXACTLY as written", fixed = TRUE)
      expect_match(p, "leading zero", fixed = TRUE)
    })

    it("pins one line per column-1 value and the total line count", {
      # Given: three unique values in column 1
      # When:  the prompt is built
      # Then:  it demands exactly 3 lines, one per value, never repeated — which
      #   lets parse_joint() treat a shorter response as incomplete rather than
      #   losing the difference silently
      p <- joint_prompt(
        data.frame(id = c("01", "02", "04")),
        data.frame(month = c("January", "Feb", "May"))
      )
      expect_match(p, "Output exactly 3 lines", fixed = TRUE)
      expect_match(p, "Never repeat", fixed = TRUE)
    })

    it("counts lines the same way for the form build_joint() passes", {
      # Given: unique(x[key1]) — a one-column data.frame of four rows
      # When:  the prompt is built
      # Then:  the promised line count is four
      p <- joint_prompt(data.frame(id = c("a", "b", "c", "d")), data.frame(m = c("1", "2")))
      expect_match(p, "Output exactly 4 lines", fixed = TRUE)
    })

    it("forbids markdown tables and tab-separated lines", {
      # Given: any input
      # When:  the prompt is built
      # Then:  the two layouts models fall back to most often are named explicitly
      #   (the old prompt banned only markdown fences, which does not cover a
      #   pipe table — measured: a pipe-table response parses as "no CSV at all")
      p <- joint_prompt(data.frame(x = "x"), data.frame(y = "y"))
      expect_match(p, "no markdown tables", fixed = TRUE)
      expect_match(p, "no tabs", fixed = TRUE)
    })

    it("keeps the CSV contract that already worked (regression)", {
      # Given: any input
      # When:  the prompt is built
      # Then:  the two-column / single-comma / quoting / no-header / leave-empty
      #   rules stay exactly as they were
      p <- joint_prompt(data.frame(x = "x"), data.frame(y = "y"))
      expect_match(p, "Two columns separated by a single comma", fixed = TRUE)
      expect_match(p, "wrap that value in double quotes", fixed = TRUE)
      expect_match(p, "No header row", fixed = TRUE)
      expect_match(p, "leave the cell empty", fixed = TRUE)
    })

  })

  describe("documented input forms", {

    it("accepts plain character vectors, as the docs promise", {
      # Given: vectors rather than data.frames (the @param text allows both)
      # When:  the prompt is built
      # Then:  no error, both value sets appear in the prompt, and the line count
      #   still comes from column 1
      p <- expect_no_error(joint_prompt(c("01", "02"), c("January", "Feb")))
      expect_match(p, "Output exactly 2 lines", fixed = TRUE)
      expect_match(p, "01", fixed = TRUE)
      expect_match(p, "January", fixed = TRUE)
    })

    it("accepts a data.frame plus a vector mix", {
      # Given: one side as data.frame, the other as vector
      # When:  the prompt is built
      # Then:  it still builds and labels both columns
      p <- expect_no_error(joint_prompt(data.frame(id = c("a", "b")), c("1", "2", "3")))
      expect_match(p, "Output exactly 2 lines", fixed = TRUE)
      expect_match(p, "column 2", fixed = TRUE)
    })

  })

})
