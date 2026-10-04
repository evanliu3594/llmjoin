# TDD/BDD: provider_parse() — claude multi text-block handling (audit #4)
# Contract: all text blocks are concatenated in order; thinking-only replies
#   still error. provider_parse() is internal — call it directly in tests.

describe("provider_parse", {

  it("returns a single claude text block unchanged (regression)", {
    # Given: parsed claude JSON with exactly one text block
    parsed <- list(content = list(list(type = "text", text = "01,January\n02,Feb")))
    # When:  provider_parse("claude", parsed)
    # Then:  returns the text verbatim
    expect_identical(provider_parse("claude", parsed), "01,January\n02,Feb")
  })

  it("concatenates all claude text blocks in order", {
    # Given: content = [text "01,Jan", thinking "hmm", text "uary\n02,Feb"]
    parsed <- list(content = list(
      list(type = "text", text = "01,Jan"),
      list(type = "thinking", thinking = "hmm"),
      list(type = "text", text = "uary\n02,Feb")
    ))
    # When:  provider_parse("claude", parsed)
    # Then:  returns "01,January\n02,Feb" — thinking block excluded, blocks
    #   pasted together with no separator
    expect_identical(provider_parse("claude", parsed), "01,January\n02,Feb")
  })

  it("errors when a claude reply has no text block", {
    # Given: content contains only thinking blocks
    parsed <- list(content = list(list(type = "thinking", thinking = "hmm")))
    # When / Then: provider_parse("claude", parsed) errors "no text block found"
    expect_error(provider_parse("claude", parsed), "no text block found")
  })

})
