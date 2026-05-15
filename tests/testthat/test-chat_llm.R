# TDD/BDD: chat_llm() — Send messages to LLM and retrieve responses
# Contract: given a valid message and config, returns a character string.
# Contract: given an empty message, errors.
# Contract: given out-of-range temperature, warns and clamps.
# Contract: .thinking=TRUE by default enables extended thinking.
# Contract: .thinking=FALSE disables thinking params.

describe("chat_llm", {

  describe("message validation", {

    it("should error when message is an empty string", {
      # Given: an empty message
      # When & Then: calling chat_llm with "" should error
      expect_error(chat_llm(""), "Message cannot be empty")
    })

    it("should error when message is character(0)", {
      # Given: a zero-length character vector
      # When & Then: calling chat_llm with character(0) should error
      expect_error(chat_llm(character(0)))
    })

    it("should error when message is missing", {
      # Given: no message argument
      # When & Then: calling chat_llm() without .message should error
      expect_error(chat_llm(), "Message cannot be empty")
    })

  })

  describe("temperature validation", {

    it("should warn and clamp when temperature is above 1", {
      skip_if_no_llm()

      # Given: temperature > 1
      # When: calling chat_llm
      # Then: should warn about range, but still return a result
      expect_warning(
        result <- chat_llm("tell a joke", .temperature = 1.5),
        "Temperature must be between 0 and 1"
      )
      expect_type(result, "character")
      expect_true(nchar(result) > 0)
    })

    it("should warn and clamp when temperature is below 0", {
      skip_if_no_llm()

      # Given: negative temperature
      # When: calling chat_llm
      # Then: should warn about range, but still return a result
      expect_warning(
        result <- chat_llm("tell a joke", .temperature = -1),
        "Temperature must be between 0 and 1"
      )
      expect_type(result, "character")
      expect_true(nchar(result) > 0)
    })

  })

  describe("response parsing (mocked)", {

    it("should parse valid OpenAI JSON response and return content string", {
      skip_if_not_installed("httr")

      # Given: mock httr POST returns 200 with valid OpenAI JSON
      mock_response <- list(status_code = 200L, body = '{"choices":[{"message":{"content":"Hello world"}}]}')
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "openai", LLM_URL = "https://api.example.com",
          LLM_key = "sk-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When: calling chat_llm with .thinking=FALSE (avoids temperature=NULL for non-Claude)
      result <- chat_llm("hello", .thinking = FALSE)

      # Then: returns parsed content string
      expect_type(result, "character")
      expect_equal(result, "Hello world")
    })

    it("should error when JSON response has no choices", {
      skip_if_not_installed("httr")

      mock_response <- list(status_code = 200L, body = '{"choices":[]}')
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "openai", LLM_URL = "https://api.example.com",
          LLM_key = "sk-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When & Then: should error about no choices
      expect_error(chat_llm("hello", .thinking = FALSE), "no choices found")
    })

    it("should error when API returns non-200 status", {
      skip_if_not_installed("httr")

      mock_response <- list(status_code = 401L, body = "Unauthorized")
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "openai", LLM_URL = "https://api.example.com",
          LLM_key = "sk-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When & Then: should error with API status
      expect_error(chat_llm("hello", .thinking = FALSE), "API request failed with status 401")
    })

    it("should suggest .thinking=FALSE when 400 status with thinking enabled", {
      skip_if_not_installed("httr")

      mock_response <- list(status_code = 400L, body = "thinking not supported")
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "openai", LLM_URL = "https://api.example.com",
          LLM_key = "sk-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When & Then: should suggest disabling thinking
      expect_error(
        chat_llm("hello", .thinking = TRUE),
        "\\.thinking = FALSE"
      )
    })

    it("should error when response is not valid JSON", {
      skip_if_not_installed("httr")

      mock_response <- list(status_code = 200L, body = "not json at all")
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "openai", LLM_URL = "https://api.example.com",
          LLM_key = "sk-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When & Then: should error about parse failure
      expect_error(chat_llm("hello", .thinking = FALSE), "Failed to parse response")
    })

    it("should parse Claude response and filter thinking blocks", {
      skip_if_not_installed("httr")

      # Given: mock Claude response with thinking + text blocks
      mock_response <- list(status_code = 200L,
        body = '{"content":[{"type":"thinking","thinking":"hmm..."},{"type":"text","text":"Bonjour"}]}')
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "claude", LLM_URL = "https://api.anthropic.com/v1/messages",
          LLM_key = "sk-ant-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When: calling chat_llm with .thinking=FALSE (Claude mock response doesnt need real thinking)
      result <- chat_llm("bonjour", .thinking = FALSE)

      # Then: only text blocks returned, thinking filtered out
      expect_type(result, "character")
      expect_equal(result, "Bonjour")
      expect_false(grepl("hmm", result))
    })

    it("should parse Claude response without thinking (single text block)", {
      skip_if_not_installed("httr")

      mock_response <- list(status_code = 200L,
        body = '{"content":[{"type":"text","text":"Hello"}]}')
      local_mocked_bindings(
        validate_llm_config = function() list(
          LLM_provider = "claude", LLM_URL = "https://api.anthropic.com/v1/messages",
          LLM_key = "sk-ant-test"
        ),
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(...) mock_response,
        status_code = function(x) x$status_code,
        content = function(x, ...) x$body,
        timeout = function(x) x,
        .package = "httr"
      )

      # When: calling chat_llm
      result <- chat_llm("hello", .thinking = FALSE)

      # Then: text content parsed correctly
      expect_type(result, "character")
      expect_equal(result, "Hello")
    })

  })

  describe("successful LLM interaction", {

    it("should return a non-empty string when asked to tell a joke", {
      skip_if_no_llm()

      # Given: a simple "tell a joke" prompt
      # When: sending to the LLM
      response <- chat_llm("tell a joke")

      # Then: response is a character string with content
      expect_type(response, "character")
      expect_length(response, 1)
      expect_true(nchar(response) > 0)
    })

    it("should accept .max_tokens parameter", {
      skip_if_no_llm()

      # Given: a tight .max_tokens limit
      # When: asking for a joke
      response <- chat_llm("tell a joke", .max_tokens = 10)

      # Then: returns a string (may be truncated but should not error)
      expect_type(response, "character")
    })

    it("should default .temperature to 0 for deterministic output", {
      skip_if_no_llm()

      # Given: no explicit temperature
      # When: calling chat_llm
      response <- chat_llm("tell a joke")

      # Then: should return a result (deterministic by default)
      expect_type(response, "character")
      expect_true(nchar(response) > 0)
    })

  })

})
