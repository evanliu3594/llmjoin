# TDD/BDD: set_llm() — Configure LLM service and store credentials
# Contract: writes config to tools::R_user_dir("llmjoin", "config")/LLMJOIN.yml
# Contract: validates provider is known, key is non-empty
# Contract: auto-fills URL and model from provider defaults when NULL

describe("set_llm", {

  describe("validation", {

    it("should error when provider is unknown", {
      expect_error(
        set_llm(provider = "unknown_provider", key = "sk-abc"),
        "Unknown provider"
      )
    })

    it("should error when key is NULL", {
      expect_error(
        set_llm(provider = "openai", key = NULL),
        "'key' must be provided"
      )
    })

    it("should error when key is empty string", {
      expect_error(
        set_llm(provider = "openai", key = ""),
        "'key' must be provided"
      )
    })

    it("should warn when URL does not start with http", {
      local_mocked_bindings(
        writeLines = function(...) invisible(),
        dir.create = function(...) invisible(TRUE),
        cat = function(...) invisible(),
        .package = "base"
      )

      expect_warning(
        set_llm(provider = "openai", url = "no-http.com", key = "sk-abc"),
        "URL should start with"
      )
    })

  })

  describe("config file writing", {

    it("should write config to tools::R_user_dir with auto-filled defaults", {
      # Given: a mock to capture writeLines calls
      writeLines_calls <- list()
      local_mocked_bindings(
        writeLines = function(text, con, ...) {
          writeLines_calls <<- c(writeLines_calls, list(list(text = text, path = con)))
        },
        dir.create = function(...) invisible(TRUE),
        cat = function(...) invisible(),
        .package = "base"
      )

      # When: setting up an OpenAI provider with minimal args
      set_llm(provider = "openai", key = "sk-test-key")

      # Then: writeLines was called once
      expect_equal(length(writeLines_calls), 1)

      # Then: path is inside tools::R_user_dir
      expected_dir <- tools::R_user_dir("llmjoin", "config")
      expect_match(writeLines_calls[[1]]$path, expected_dir, fixed = TRUE)
      expect_match(writeLines_calls[[1]]$path, "LLMJOIN.yml", fixed = TRUE)

      # Then: config content has correct YAML structure
      text <- writeLines_calls[[1]]$text
      expect_match(text, "LLM_provider: 'openai'", fixed = TRUE)
      expect_match(text, "LLM_key: 'sk-test-key'", fixed = TRUE)
      expect_match(text, "LLM_model: 'gpt-5.4-mini'", fixed = TRUE)
      expect_match(text, "LLM_URL: 'https://api.openai.com/v1/chat/completions'",
                   fixed = TRUE)
    })

    it("should use custom URL and model when provided", {
      writeLines_calls <- list()
      local_mocked_bindings(
        writeLines = function(text, con, ...) {
          writeLines_calls <<- c(writeLines_calls, list(list(text = text, path = con)))
        },
        dir.create = function(...) invisible(TRUE),
        cat = function(...) invisible(),
        .package = "base"
      )

      set_llm(provider = "openai", url = "https://custom.api.com/v1",
              key = "sk-custom", model = "gpt-4.1-mini")

      text <- writeLines_calls[[1]]$text
      expect_match(text, "LLM_URL: 'https://custom.api.com/v1'", fixed = TRUE)
      expect_match(text, "LLM_model: 'gpt-4.1-mini'", fixed = TRUE)
    })

    it("should escape single quotes in values", {
      writeLines_calls <- list()
      local_mocked_bindings(
        writeLines = function(text, con, ...) {
          writeLines_calls <<- c(writeLines_calls, list(list(text = text, path = con)))
        },
        dir.create = function(...) invisible(TRUE),
        cat = function(...) invisible(),
        .package = "base"
      )

      set_llm(provider = "openai", key = "sk-with-'quote'-chars")

      text <- writeLines_calls[[1]]$text
      expect_match(text, "LLM_key: 'sk-with-''quote''-chars'", fixed = TRUE)
    })

    it("should support claude provider with API key auth", {
      writeLines_calls <- list()
      local_mocked_bindings(
        writeLines = function(text, con, ...) {
          writeLines_calls <<- c(writeLines_calls, list(list(text = text, path = con)))
        },
        dir.create = function(...) invisible(TRUE),
        cat = function(...) invisible(),
        .package = "base"
      )

      set_llm(provider = "claude", key = "sk-ant-test")

      text <- writeLines_calls[[1]]$text
      expect_match(text, "LLM_provider: 'claude'", fixed = TRUE)
      expect_match(text, "LLM_model: 'claude-haiku-4-5'", fixed = TRUE)
    })

    it("should support gemini provider", {
      writeLines_calls <- list()
      local_mocked_bindings(
        writeLines = function(text, con, ...) {
          writeLines_calls <<- c(writeLines_calls, list(list(text = text, path = con)))
        },
        dir.create = function(...) invisible(TRUE),
        cat = function(...) invisible(),
        .package = "base"
      )

      set_llm(provider = "gemini", key = "gemini-key")

      text <- writeLines_calls[[1]]$text
      expect_match(text, "LLM_provider: 'gemini'", fixed = TRUE)
      expect_match(text, "LLM_model: 'gemini-3-flash'", fixed = TRUE)
    })

  })

})
