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

# TDD/BDD: provider layer — 261006 batch
# Contract: default models are openai "gpt-6-luna", gemini "gemini-3.8-flash";
#   a new first-class provider deepseek defaults to "deepseek-flash"
#   (261006, maintainer-approved).
# Contract: OpenAI GPT-5+/o-series models (names matching ^o[0-9] or
#   ^gpt-[5-9]) reject the official API's max_tokens and temperature
#   parameters: provider_body() must send max_completion_tokens and omit
#   temperature for them, while every other model keeps the classic
#   max_tokens + temperature shape (third-party OpenAI-compatible endpoints —
#   Ollama, proxies — are routed through provider="openai" and must not
#   break). A non-zero temperature on a reasoning model warns that it is
#   ignored; the default 0 stays silent.
# Contract: provider_parse() warns on truncated replies — openai/gemini/
#   deepseek finish_reason "length", claude stop_reason "max_tokens" — and
#   still returns the parsed text; normal completion stays silent; the
#   empty-content error for reasoning replies keeps precedence over any
#   truncation warning.

describe("provider registry defaults (261006)", {

  it("uses gpt-6-luna as the openai default model", {
    # Given: the .providers registry
    # When:  the openai entry is inspected
    # Then:  default_model equals "gpt-6-luna"
    expect_identical(.providers$openai$default_model, "gpt-6-luna")
  })

  it("uses gemini-3.8-flash as the gemini default model", {
    # Given: the .providers registry
    # When:  the gemini entry is inspected
    # Then:  default_model equals "gemini-3.8-flash"
    expect_identical(.providers$gemini$default_model, "gemini-3.8-flash")
  })

  it("registers deepseek as a first-class provider", {
    # Given: the .providers registry
    # When:  the deepseek entry is inspected
    # Then:  default_model equals "deepseek-flash", base_url equals
    #   "https://api.deepseek.com/v1", endpoint equals "/chat/completions",
    #   auth_type equals "bearer"
    ds <- .providers$deepseek
    expect_identical(ds$default_model, "deepseek-flash")
    expect_identical(ds$base_url, "https://api.deepseek.com/v1")
    expect_identical(ds$endpoint, "/chat/completions")
    expect_identical(ds$auth_type, "bearer")
  })

})

describe("provider_body — openai reasoning models (gpt-5+/o-series)", {

  it("sends max_completion_tokens and omits temperature for gpt-5+/o-series", {
    # Given: provider "openai" and model in {"gpt-6-luna", "gpt-5.4-mini",
    #   "o4-mini"}
    # When:  provider_body("openai", model, "hi", 0, 30000)
    # Then:  max_completion_tokens == 30000; no max_tokens element; no
    #   temperature element; messages[[1]]$content == "hi"
    for (model in c("gpt-6-luna", "gpt-5.4-mini", "o4-mini")) {
      body <- provider_body("openai", model, "hi", 0, 30000)
      expect_identical(body$max_completion_tokens, 30000, info = model)
      expect_null(body$max_tokens, info = model)
      expect_null(body$temperature, info = model)
      expect_identical(body$messages[[1]]$content, "hi", info = model)
    }
  })

  it("keeps max_tokens and temperature for other openai models", {
    # Given: model "gpt-4.1-mini" (third-party compatible endpoints rely on
    #   the classic body shape)
    # When:  provider_body("openai", "gpt-4.1-mini", "hi", 0, 100)
    # Then:  max_tokens == 100, temperature == 0, no max_completion_tokens
    body <- provider_body("openai", "gpt-4.1-mini", "hi", 0, 100)
    expect_identical(body$max_tokens, 100)
    expect_identical(body$temperature, 0)
    expect_null(body$max_completion_tokens)
  })

  it("warns that a non-zero temperature is ignored for gpt-5+/o-series", {
    # Given: model "gpt-6-luna", temperature 0.3
    # When:  provider_body("openai", "gpt-6-luna", "hi", 0.3, 30000)
    # Then:  a warning mentioning 'temperature' fires; the body still omits
    #   the temperature element
    expect_warning(
      body <- provider_body("openai", "gpt-6-luna", "hi", 0.3, 30000),
      "temperature"
    )
    expect_null(body$temperature)
    # the warning names the model, says temperature was not sent, and gives
    # an actionable fix hint
    expect_warning(
      provider_body("openai", "gpt-6-luna", "hi", 0.3, 30000),
      "gpt-6-luna",
      fixed = TRUE
    )
    expect_warning(
      provider_body("openai", "gpt-6-luna", "hi", 0.3, 30000),
      "was not sent",
      fixed = TRUE
    )
    expect_warning(
      provider_body("openai", "gpt-6-luna", "hi", 0.3, 30000),
      "Remove the .temperature argument to silence this warning.",
      fixed = TRUE
    )
  })

  it("stays silent for the default zero temperature", {
    # Given: model "gpt-6-luna", temperature 0 (the chat_llm default)
    # When:  provider_body("openai", "gpt-6-luna", "hi", 0, 30000)
    # Then:  no warning
    expect_no_warning(provider_body("openai", "gpt-6-luna", "hi", 0, 30000))
  })

  it("sends the classic body for gemini and deepseek", {
    # Given: provider in {"gemini", "deepseek"} with its default model
    # When:  provider_body(<provider>, <default model>, "hi", 0, 100)
    # Then:  max_tokens == 100, temperature == 0, no max_completion_tokens
    for (provider in c("gemini", "deepseek")) {
      body <- provider_body(
        provider, .providers[[provider]]$default_model, "hi", 0, 100
      )
      expect_identical(body$max_tokens, 100, info = provider)
      expect_identical(body$temperature, 0, info = provider)
      expect_null(body$max_completion_tokens, info = provider)
    }
  })

})

describe("provider_parse — truncation feedback (E1)", {

  it("warns on openai finish_reason 'length' and still returns the text", {
    # Given: parsed JSON with choices[[1]]$message$content "01,Jan" and
    #   finish_reason "length"
    # When:  provider_parse("openai", parsed)
    # Then:  a warning mentioning the truncation fires; result "01,Jan"
    parsed <- list(choices = list(list(
      message = list(content = "01,Jan"),
      finish_reason = "length"
    )))
    expect_warning(
      out <- provider_parse("openai", parsed),
      "finish_reason 'length'",
      fixed = TRUE
    )
    expect_identical(out, "01,Jan")
  })

  it("warns on gemini finish_reason 'length'", {
    # Given: same shape as openai, provider "gemini"
    # When:  provider_parse("gemini", parsed)
    # Then:  a warning mentioning the truncation fires; text returned
    parsed <- list(choices = list(list(
      message = list(content = "02,Feb"),
      finish_reason = "length"
    )))
    expect_warning(
      out <- provider_parse("gemini", parsed),
      "finish_reason 'length'",
      fixed = TRUE
    )
    expect_identical(out, "02,Feb")
  })

  it("warns on claude stop_reason 'max_tokens' and still returns the text", {
    # Given: claude content with one text block, stop_reason "max_tokens"
    # When:  provider_parse("claude", parsed)
    # Then:  a warning mentioning 'max_tokens' fires; the text is returned
    parsed <- list(
      content = list(list(type = "text", text = "03,Mar")),
      stop_reason = "max_tokens"
    )
    expect_warning(
      out <- provider_parse("claude", parsed),
      "stop_reason 'max_tokens'",
      fixed = TRUE
    )
    expect_identical(out, "03,Mar")
  })

  it("stays silent on normal completion (regression)", {
    # Given: openai reply with finish_reason "stop" and claude reply with
    #   stop_reason "end_turn"
    # When:  provider_parse() on both
    # Then:  no warnings from either call
    openai_parsed <- list(choices = list(list(
      message = list(content = "01,Jan"),
      finish_reason = "stop"
    )))
    claude_parsed <- list(
      content = list(list(type = "text", text = "02,Feb")),
      stop_reason = "end_turn"
    )
    expect_no_warning(provider_parse("openai", openai_parsed))
    expect_no_warning(provider_parse("claude", claude_parsed))
  })

  it("keeps the empty-content error ahead of any truncation warning", {
    # Given: openai reply with NULL message content and finish_reason "length"
    # When:  provider_parse("openai", parsed)
    # Then:  the existing "empty message content" error fires (no warning)
    parsed <- list(choices = list(list(
      message = list(content = NULL),
      finish_reason = "length"
    )))
    expect_no_warning(
      expect_error(
        provider_parse("openai", parsed),
        "empty message content",
        fixed = TRUE
      )
    )
  })

})

describe("deepseek provider plumbing", {

  it("builds bearer headers for deepseek", {
    # Given: key "sk-test"
    # When:  provider_headers("deepseek", "sk-test")
    # Then:  Authorization equals "Bearer sk-test"; no x-api-key element
    headers <- provider_headers("deepseek", "sk-test")
    expect_identical(headers$Authorization, "Bearer sk-test")
    expect_null(headers$`x-api-key`)
  })

  it("builds the deepseek endpoint url", {
    # Given: base_url "https://api.deepseek.com/v1"
    # When:  provider_url("deepseek", "https://api.deepseek.com/v1")
    # Then:  "https://api.deepseek.com/v1/chat/completions"
    expect_identical(
      provider_url("deepseek", "https://api.deepseek.com/v1"),
      "https://api.deepseek.com/v1/chat/completions"
    )
  })

  it("parses deepseek responses with the openai shape", {
    # Given: parsed JSON choices[[1]]$message$content "01,Jan" (no
    #   finish_reason field)
    # When:  provider_parse("deepseek", parsed)
    # Then:  "01,Jan", no warning
    parsed <- list(choices = list(list(message = list(content = "01,Jan"))))
    expect_no_warning(out <- provider_parse("deepseek", parsed))
    expect_identical(out, "01,Jan")
  })

})
