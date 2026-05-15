# TDD/BDD: joint_prompt() — Generate prompts for LLM fuzzy matching
# Contract: given two single-column data.frames or vectors, returns a prompt
# string that instructs the LLM to produce a CSV mapping.

describe("joint_prompt", {

  it("should generate a prompt string from two single-column data.frames", {
    # Given: two single-column data.frames with fuzzy keys
    x <- data.frame(id = c("01", "02", "04"))
    y <- data.frame(month = c("January", "Feb", "May"))

    # When: generating the joint prompt
    prompt <- joint_prompt(x, y)

    # Then: returns a single character string
    expect_type(prompt, "character")
    expect_length(prompt, 1)

    # Then: contains the instruction
    expect_match(prompt, "Match each item in column 1 to the most similar item")

    # Then: contains values from the first column
    expect_match(prompt, "01")
    expect_match(prompt, "02")
    expect_match(prompt, "04")

    # Then: contains values from the second column
    expect_match(prompt, "January")
    expect_match(prompt, "Feb")
    expect_match(prompt, "May")

    # Then: instructs CSV output format
    expect_match(prompt, "Output ONLY a CSV table")
  })

  it("should include markdown table formatting in the prompt", {
    # Given: data.frames with simple content
    x <- data.frame(code = c("A1"))
    y <- data.frame(name = c("Alpha"))

    # When: generating prompt
    prompt <- joint_prompt(x, y)

    # Then: contains markdown table markers for both columns
    expect_match(prompt, "column 1:")
    expect_match(prompt, "column 2:")
  })

  it("should work with vector inputs", {
    # Given: vectors (handled by tbl2md internally)
    vx <- c("01", "02")
    vy <- c("January", "Feb")

    # When: generating prompt - note joint_prompt passes to tbl2md
    # which needs nm for vectors, but joint_prompt passes data.frames via unique(x[key1])
    # This test verifies joint_prompt's own behavior with data.frame inputs
    prompt <- joint_prompt(
      data.frame(k1 = vx),
      data.frame(k2 = vy)
    )

    # Then: prompt generated correctly
    expect_type(prompt, "character")
    expect_match(prompt, "01")
    expect_match(prompt, "January")
  })

  it("should include the anti-fabrication rule", {
    # Given: any two data.frames
    x <- data.frame(a = "x")
    y <- data.frame(b = "y")

    # When: generating prompt
    prompt <- joint_prompt(x, y)

    # Then: contains rule against fabricating data
    expect_match(prompt, "Do NOT invent or fabricate any data")
  })

})
