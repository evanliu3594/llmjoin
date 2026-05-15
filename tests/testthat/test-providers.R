# TDD/BDD: provider_body(), provider_parse() — thinking model support
# Contract: provider_body(thinking=TRUE) injects thinking params for all providers.
# Contract: provider_body(thinking=FALSE) does NOT inject thinking params.
# Contract: provider_body removes temperature for Claude when thinking=TRUE.
# Contract: provider_body bumps max_tokens for Claude when too small.
# Contract: provider_parse filters Claude thinking blocks, returns only text.
# Contract: provider_parse ignores reasoning_content in OpenAI response.

describe("provider_body with thinking", {

  it("should inject reasoning_effort for OpenAI when thinking=TRUE", {
    body <- provider_body("openai", "gpt-4.1-nano", "hi", 0, 1000, thinking = TRUE)
    expect_equal(body$reasoning_effort, "high")
    expect_equal(body$temperature, 0)
  })

  it("should NOT inject reasoning_effort for OpenAI when thinking=FALSE", {
    body <- provider_body("openai", "gpt-4.1-nano", "hi", 0, 1000, thinking = FALSE)
    expect_null(body$reasoning_effort)
    expect_equal(body$temperature, 0)
  })

  it("should NOT inject thinking by default (thinking=FALSE)", {
    body <- provider_body("openai", "gpt-4.1-nano", "hi", 0, 1000)
    expect_null(body$reasoning_effort)
  })

  it("should inject thinking and remove temperature for Claude when thinking=TRUE", {
    body <- provider_body("claude", "claude-haiku-4-5-20251001", "hi", 0.5, 20000,
                          thinking = TRUE)
    expect_equal(body$thinking$type, "enabled")
    expect_equal(body$thinking$budget_tokens, 16000)
    expect_null(body$temperature)
  })

  it("should bump max_tokens for Claude when too small for thinking budget", {
    body <- provider_body("claude", "claude-opus-4-7", "hi", 0, 1000,
                          thinking = TRUE)
    expect_equal(body$max_tokens, 17000)
    expect_equal(body$thinking$type, "enabled")
    expect_null(body$temperature)
  })

  it("should NOT bump max_tokens for Claude when already large enough", {
    body <- provider_body("claude", "claude-opus-4-7", "hi", 0, 20000,
                          thinking = TRUE)
    expect_equal(body$max_tokens, 20000)
  })

  it("should inject reasoning_effort for Gemini when thinking=TRUE", {
    body <- provider_body("gemini", "gemini-2.5-flash", "hi", 0, 1000,
                          thinking = TRUE)
    expect_equal(body$reasoning_effort, "high")
    expect_equal(body$temperature, 0)
  })

})

describe("provider_parse with thinking", {

  it("should extract only text block from Claude response with thinking", {
    parsed <- list(content = list(
      list(type = "thinking", thinking = "Let me think..."),
      list(type = "text", text = "The answer is 42.")
    ))
    result <- provider_parse("claude", parsed)
    expect_equal(result, "The answer is 42.")
    expect_false(grepl("think", result))
  })

  it("should extract and concatenate multiple text blocks", {
    parsed <- list(content = list(
      list(type = "text", text = "First."),
      list(type = "thinking", thinking = "hmm"),
      list(type = "text", text = "Second.")
    ))
    result <- provider_parse("claude", parsed)
    expect_equal(result, "First.Second.")
  })

  it("should handle Claude response without thinking (backward compat)", {
    parsed <- list(content = list(
      list(type = "text", text = "Hello")
    ))
    result <- provider_parse("claude", parsed)
    expect_equal(result, "Hello")
  })

  it("should handle Claude content block without type field", {
    parsed <- list(content = list(
      list(text = "Legacy response")
    ))
    result <- provider_parse("claude", parsed)
    expect_equal(result, "Legacy response")
  })

  it("should error when Claude response has no text blocks", {
    parsed <- list(content = list(
      list(type = "thinking", thinking = "hmm")
    ))
    expect_error(
      provider_parse("claude", parsed),
      "no text content found"
    )
  })

  it("should extract content from OpenAI response", {
    parsed <- list(choices = list(
      list(message = list(content = "Hello from OpenAI"))
    ))
    result <- provider_parse("openai", parsed)
    expect_equal(result, "Hello from OpenAI")
  })

  it("should ignore reasoning_content in OpenAI response", {
    parsed <- list(choices = list(
      list(message = list(
        content = "Final answer",
        reasoning_content = "Step 1: analyze..."
      ))
    ))
    result <- provider_parse("openai", parsed)
    expect_equal(result, "Final answer")
    expect_false(grepl("Step 1", result))
  })

})
