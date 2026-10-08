# TDD/BDD: chat_llm() — .message input coercion (audit #7)
# Contract: .message accepts any non-empty input; elements are coerced via
#   as.character(), NA elements become blank lines, and a length > 1 input is
#   pasted into a single newline-separated string before sending.
# Contract: missing / NULL / zero-length / all-blank input errors with a
#   message naming '.message' and how to fix it (never the internal
#   "condition has length > 1" error).
# Contract: validation runs before the config read, so the error-path tests
#   need no configuration; coercion tests run with the config redirected to a
#   temp dir (R_USER_CONFIG_DIR, verified on R 4.6.1) and the HTTP layer fully
#   stubbed. No real network, no real user config is touched.

# Helper: fresh config dir for the calling test only; returns the LLMJOIN.yml
# path. Duplicated in test-get_llm.R because testthat gives every file its own
# environment and tests/AGENTS.md keeps fixtures inline — keep the two in sync.
.local_config_dir <- function() {
  withr::local_envvar(
    R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"),
    .local_envir = parent.frame()
  )
  file.path(tools::R_user_dir("llmjoin", "config"), "LLMJOIN.yml")
}

describe("chat_llm", {

  describe("message validation errors (validation precedes the config read)", {

    it("errors on missing .message", {
      # Given: chat_llm() called without .message
      # When:  chat_llm()
      # Then:  error text names '.message' and shows how to fix
      expect_error(chat_llm(), "'\\.message' is required")
      expect_error(chat_llm(), "tell a joke")
    })

    it("errors on NULL .message", {
      # Given: .message = NULL
      # When:  chat_llm(.message = NULL)
      # Then:  error text names '.message'
      expect_error(chat_llm(.message = NULL), "'\\.message' is required")
    })

    it("errors on zero-length .message", {
      # Given: .message = character(0)
      # When:  chat_llm(.message = character(0))
      # Then:  error text names '.message' (previously an internal
      #   "length = 0 in coercion to logical(1)" error leaked out)
      expect_error(chat_llm(.message = character(0)), "'\\.message' is required")
    })

    it("errors when every element is empty, blank or NA", {
      # Given: .message in { "", "  ", c(NA, NA), c("", "  ") }
      # When:  chat_llm(.message = <each>)
      # Then:  each call errors with text naming '.message'
      for (m in list("", "  ", c(NA, NA), c("", "  "))) {
        expect_error(
          chat_llm(.message = m),
          "'\\.message' must contain non-blank text"
        )
      }
    })

  })

  describe("message coercion (temp-dir config, HTTP stubbed)", {
    # Fixture template for every test below (inline per tests/AGENTS.md):
    #   withr::local_envvar(R_USER_CONFIG_DIR = <fresh temp dir>)
    #   suppressMessages(set_llm(provider = "openai", key = "test-key",
    #                            model = "test-model"))
    #   captured <- new.env()
    #   fake_response <- structure(list(status_code = 200L), class = "response")
    #   local_mocked_bindings(
    #     POST = function(url, body, ...) { captured$body <- body; fake_response },
    #     content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
    #     status_code = function(x) 200L,
    #     .package = "httr"
    #   )
    # The sent message text is jsonlite::fromJSON(captured$body)$messages[[1]]$content.

    it("sends a single string unchanged (regression)", {
      # Given: .message = "tell a joke."
      # When:  chat_llm(.message = "tell a joke.")
      # Then:  the outgoing content equals "tell a joke."; the returned text
      #   equals "01,January"
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      suppressMessages(set_llm(provider = "openai", key = "test-key", model = "test-model"))
      captured <- new.env()
      fake_response <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        POST = function(url, body, ...) { captured$body <- body; fake_response },
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      result <- chat_llm(.message = "tell a joke.")
      sent <- jsonlite::fromJSON(captured$body, simplifyVector = FALSE)$messages[[1]]$content
      expect_identical(sent, "tell a joke.")
      expect_identical(result, "01,January")
    })

    it("pastes a length > 1 vector into one newline-separated string", {
      # Given: .message = c("line 1", "line 2")
      # When:  chat_llm(.message = c("line 1", "line 2"))
      # Then:  the outgoing content equals "line 1\nline 2" — the request is
      #   sent instead of erroring on "condition has length > 1"
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      suppressMessages(set_llm(provider = "openai", key = "test-key", model = "test-model"))
      captured <- new.env()
      fake_response <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        POST = function(url, body, ...) { captured$body <- body; fake_response },
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      result <- chat_llm(.message = c("line 1", "line 2"))
      sent <- jsonlite::fromJSON(captured$body, simplifyVector = FALSE)$messages[[1]]$content
      expect_identical(sent, "line 1\nline 2")
      expect_identical(result, "01,January")
    })

    it("coerces non-character input via as.character", {
      # Given: .message = 123 (numeric) and .message = factor("tell a joke.")
      # When:  chat_llm(.message = <each>)
      # Then:  the outgoing content equals "123" resp. "tell a joke."
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      suppressMessages(set_llm(provider = "openai", key = "test-key", model = "test-model"))
      for (m in list(123, factor("tell a joke."))) {
        captured <- new.env()
        fake_response <- structure(list(status_code = 200L), class = "response")
        local_mocked_bindings(
          POST = function(url, body, ...) { captured$body <- body; fake_response },
          content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
          status_code = function(x) 200L,
          .package = "httr"
        )
        chat_llm(.message = m)
        sent <- jsonlite::fromJSON(captured$body, simplifyVector = FALSE)$messages[[1]]$content
        expect_identical(sent, as.character(m))
      }
    })

    it("treats NA elements as blank lines in a mixed vector", {
      # Given: .message = c("a", NA)
      # When:  chat_llm(.message = c("a", NA))
      # Then:  the outgoing content equals "a\n" — NA must NOT reach the body
      #   as the literal string "NA" (paste() would render it so without the
      #   pre-blanking step; verified on R 4.6.1)
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      suppressMessages(set_llm(provider = "openai", key = "test-key", model = "test-model"))
      captured <- new.env()
      fake_response <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        POST = function(url, body, ...) { captured$body <- body; fake_response },
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      chat_llm(.message = c("a", NA))
      sent <- jsonlite::fromJSON(captured$body, simplifyVector = FALSE)$messages[[1]]$content
      expect_identical(sent, "a\n")
    })

    it("returns the parsed provider text", {
      # Given: the stubbed openai-style response
      # When:  chat_llm(.message = "hi")
      # Then:  the result is the character string "01,January"
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      suppressMessages(set_llm(provider = "openai", key = "test-key", model = "test-model"))
      captured <- new.env()
      fake_response <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        POST = function(url, body, ...) { captured$body <- body; fake_response },
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      result <- chat_llm(.message = "hi")
      expect_identical(result, "01,January")
    })

  })

  describe("config read errors (characterization before the .read_config() refactor)", {

    it("errors when no config file exists", {
      # Given: a fresh temp config dir holding no LLMJOIN.yml
      # When:  chat_llm(.message = "hi")
      # Then:  the error names set_llm(); no HTTP stub is installed, so passing
      #   proves the failure happens before any request is attempted
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      expect_error(
        chat_llm(.message = "hi"),
        "LLM service not configured. Use `set_llm()` to set up your API key and endpoint.",
        fixed = TRUE
      )
    })

    it("errors when the config file is not valid YAML", {
      # Given: a hand-written config whose first token is a tab (the yaml
      #   scanner rejects it; verified on config 0.6 + R 4.6.1)
      # When:  chat_llm(.message = "hi")
      # Then:  the error reports 'Invalid config file', the config path, and
      #   the set_llm() reconfigure guidance
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      cfg_dir <- tools::R_user_dir("llmjoin", "config")
      dir.create(cfg_dir, showWarnings = FALSE, recursive = TRUE)
      writeLines("\t- not: a mapping:\tabc", file.path(cfg_dir, "LLMJOIN.yml"))
      err <- tryCatch(chat_llm(.message = "hi"), error = function(e) conditionMessage(e))
      expect_match(err, "Invalid config file", fixed = TRUE)
      expect_match(err, "LLMJOIN.yml", fixed = TRUE)
      expect_match(err, "Use set_llm() to reconfigure.", fixed = TRUE)
    })

    it("errors when the stored key is an empty string (never reaches the network)", {
      # Given: a hand-edited config with LLM_key: '' — .read_config()'s is.null
      #   guard lets an empty string through, so without the check chat_llm()
      #   would send a request with a blank Authorization header and surface the
      #   provider's 401 instead of the real problem
      # When:  chat_llm(.message = "hi") runs with the HTTP layer stubbed
      #   (POST records whether it was called)
      # Then:  the error names the empty LLM_key and how to fix it, and POST
      #   was never reached
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(
        paste0(
          "default:\n  LLM_provider: 'openai'\n",
          "  LLM_URL: 'https://api.openai.com/v1/chat/completions'\n",
          "  LLM_key: ''\n  LLM_model: 'test-model'"
        ),
        cfg_path
      )
      called <- new.env()
      fake <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        POST = function(url, ...) {
          called$ran <- TRUE
          fake
        },
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      err <- tryCatch(chat_llm(.message = "hi"), error = function(e) conditionMessage(e))
      expect_match(err, "empty 'LLM_key'", fixed = TRUE)
      expect_match(err, "LLMJOIN.yml", fixed = TRUE)
      expect_match(err, "set_llm()", fixed = TRUE)
      expect_false(isTRUE(called$ran))
    })

  })

  describe("request path keeps the plaintext key (refactor guard)", {

    it("passes the unmasked key to provider_headers", {
      # Given: a 19-character stored key, provider_headers and httr stubbed
      # When:  chat_llm(.message = "hi")
      # Then:  provider_headers receives the plaintext key — masking in
      #   get_llm() is a display concern and must never reach the request
      .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      captured <- new.env()
      fake_response <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        provider_headers = function(provider, key) {
          captured$key <- key
          list(`Content-Type` = "application/json")
        },
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(url, ...) fake_response,
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      chat_llm(.message = "hi")
      expect_identical(captured$key, "sk-1234567890abcdef")
      expect_false(identical(captured$key, "****cdef"))
    })

  })

})
