# TDD/BDD: get_llm() — read the stored LLM config without opening the YAML file
# Contract: returns an invisible named list (provider, url, model, key,
#   config_path) and reports the same fields through message(). The API key is
#   masked by default; show_key = TRUE is the only way to get the plaintext back.
# Contract: masking rule (.mask_key) — empty -> "<empty>"; nchar <= 8 -> "****";
#   nchar > 8 -> "****" plus the last 4 characters.
# Contract: a config failure raises the same error text chat_llm() raises.
# All tests run against a temp R_USER_CONFIG_DIR (the real user config is never
#   touched) and install no HTTP stubs because get_llm() makes no request.

# Helper: fresh config dir for the calling test only; returns the LLMJOIN.yml
# path. Defined in test-chat_llm.R as well — keep the two copies in sync.
.local_config_dir <- function() {
  withr::local_envvar(
    R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"),
    .local_envir = parent.frame()
  )
  file.path(tools::R_user_dir("llmjoin", "config"), "LLMJOIN.yml")
}

# Helper: write a config file by hand, bypassing set_llm(), to reach states a
# user can create with a text editor (missing lines, empty values). Pass the
# path from .local_config_dir() so the temp dir stays scoped to the test.
.write_raw_config <- function(cfg_path, lines) {
  dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(paste(lines, collapse = "\n"), cfg_path)
  cfg_path
}

# Helper: evaluate expr with messages muffled; returns list(value, messages)
.messages_of <- function(expr) {
  collected <- character()
  value <- withCallingHandlers(
    expr,
    message = function(m) {
      collected <<- c(collected, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = paste(collected, collapse = ""))
}

describe("get_llm", {

  describe("normal read (masked by default)", {

    it("reports provider, model, url and config path", {
      # Given: set_llm(provider="openai", key="sk-1234567890abcdef", model="test-model")
      # When:  get_llm()
      # Then:  the list carries the stored values and the messages name the
      #   provider and model
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      out <- .messages_of(get_llm())
      expect_identical(out$value$provider, "openai")
      expect_identical(out$value$model, "test-model")
      expect_identical(out$value$url, "https://api.openai.com/v1/chat/completions")
      expect_identical(out$value$config_path, cfg_path)
      expect_match(out$messages, "Provider: openai", fixed = TRUE)
      expect_match(out$messages, "Model: test-model", fixed = TRUE)
      expect_match(out$messages, "URL: https://api.openai.com/v1/chat/completions", fixed = TRUE)
    })

    it("masks the key in the value and in the messages", {
      # Given: a stored 19-character key
      # When:  get_llm()
      # Then:  value$key is "****cdef", the plaintext appears in neither value
      #   nor messages, and the show_key hint is printed
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      out <- .messages_of(get_llm())
      expect_identical(out$value$key, "****cdef")
      expect_match(out$messages, "Key: ****cdef", fixed = TRUE)
      expect_false(grepl("sk-1234567890abcdef", out$messages, fixed = TRUE))
      expect_match(out$messages, "show_key = TRUE", fixed = TRUE)
    })

    it("returns the plaintext key only when show_key = TRUE", {
      # Given: the same stored config
      # When:  get_llm(show_key = TRUE)
      # Then:  value$key is the full key, the messages carry it, and the
      #   show_key hint is absent
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      out <- .messages_of(get_llm(show_key = TRUE))
      expect_identical(out$value$key, "sk-1234567890abcdef")
      expect_match(out$messages, "sk-1234567890abcdef", fixed = TRUE)
      expect_false(grepl("show_key = TRUE", out$messages, fixed = TRUE))
    })

    it("prints nothing on standard output (returns invisibly)", {
      # Given: the same stored config
      # When:  get_llm() is evaluated with its visibility observed
      # Then:  the result is a list whose visibility is FALSE, so the console
      #   only ever shows the message() lines
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      vis <- withVisible(
        withCallingHandlers(
          get_llm(),
          message = function(m) invokeRestart("muffleMessage")
        )
      )
      expect_false(vis$visible)
      expect_type(vis$value, "list")
    })

    it("does not modify the config file", {
      # Given: the same stored config
      # When:  get_llm() runs twice
      # Then:  LLMJOIN.yml is byte-identical before and after (read-only API)
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      before <- paste(readLines(cfg_path, warn = FALSE), collapse = "\n")
      suppressMessages(get_llm())
      suppressMessages(get_llm())
      after <- paste(readLines(cfg_path, warn = FALSE), collapse = "\n")
      expect_identical(before, after)
    })

    it("falls back to the provider default model when the config omits it", {
      # Given: a hand-written config with no LLM_model line
      # When:  get_llm()
      # Then:  model is the .providers registry default for openai
      .write_raw_config(.local_config_dir(), c(
        "default:",
        "  LLM_provider: 'openai'",
        "  LLM_URL: 'https://api.openai.com/v1/chat/completions'",
        "  LLM_key: 'sk-1234567890abcdef'"
      ))
      out <- .messages_of(get_llm())
      expect_identical(out$value$model, .providers$openai$default_model)
      expect_match(out$messages, "Provider: openai", fixed = TRUE)
    })

  })

  describe("key masking boundaries", {

    it("fully masks keys of 8 characters or fewer", {
      # Given: keys of length 8, 7 and 1
      # When:  get_llm() for each
      # Then:  the masked value is exactly "****" — no tail is leaked for short
      #   keys, where the tail would be half the key or more
      for (k in c("test-key", "1234567", "a")) {
        .write_raw_config(.local_config_dir(), c(
          "default:",
          "  LLM_provider: 'openai'",
          "  LLM_URL: 'https://api.openai.com/v1/chat/completions'",
          paste0("  LLM_key: '", k, "'"),
          "  LLM_model: 'test-model'"
        ))
        out <- .messages_of(get_llm())
        expect_identical(out$value$key, "****")
      }
    })

    it("reveals only the last 4 characters of a 9-character key", {
      # Given: a 9-character key — the first length allowed to show a tail
      # When:  get_llm()
      # Then:  the masked value is "****6789"
      .write_raw_config(.local_config_dir(), c(
        "default:",
        "  LLM_provider: 'openai'",
        "  LLM_URL: 'https://api.openai.com/v1/chat/completions'",
        "  LLM_key: '123456789'",
        "  LLM_model: 'test-model'"
      ))
      out <- .messages_of(get_llm())
      expect_identical(out$value$key, "****6789")
    })

    it("marks a blank stored key as <empty>", {
      # Given: hand-edited config with LLM_key: '' — chat_llm's is.null guard
      #   lets an empty string through, so this state is reachable
      # When:  get_llm()
      # Then:  value$key is "<empty>" and the messages show it instead of a
      #   silent blank
      .write_raw_config(.local_config_dir(), c(
        "default:",
        "  LLM_provider: 'openai'",
        "  LLM_URL: 'https://api.openai.com/v1/chat/completions'",
        "  LLM_key: ''",
        "  LLM_model: 'test-model'"
      ))
      out <- .messages_of(get_llm())
      expect_identical(out$value$key, "<empty>")
      expect_match(out$messages, "Key: <empty>", fixed = TRUE)
    })

  })

  describe("config failures report the stored path", {

    it("errors when no config file exists", {
      # Given: an empty temp config dir (no LLMJOIN.yml written at all)
      # When:  get_llm()
      # Then:  the error text is byte-identical to the one chat_llm() raises
      cfg_path <- .local_config_dir()
      expect_false(file.exists(cfg_path))
      expect_error(
        get_llm(),
        "LLM service not configured. Use `set_llm()` to set up your API key and endpoint.",
        fixed = TRUE
      )
    })

    it("errors when the config file is not valid YAML", {
      # Given: a config whose first token is a tab (the yaml scanner rejects it)
      # When:  get_llm()
      # Then:  the error reports 'Invalid config file', the config path and the
      #   set_llm() reconfigure guidance
      .write_raw_config(.local_config_dir(), c("\t- not: a mapping:\tabc"))
      err <- tryCatch(get_llm(), error = function(e) conditionMessage(e))
      expect_match(err, "Invalid config file", fixed = TRUE)
      expect_match(err, "LLMJOIN.yml", fixed = TRUE)
      expect_match(err, "Use set_llm() to reconfigure.", fixed = TRUE)
    })

    it("errors when the stored config has no key", {
      # Given: a hand-written config missing the LLM_key line
      # When:  get_llm()
      # Then:  the error names the missing fields and how to fix them
      .write_raw_config(.local_config_dir(), c(
        "default:",
        "  LLM_provider: 'openai'",
        "  LLM_URL: 'https://api.openai.com/v1/chat/completions'",
        "  LLM_model: 'test-model'"
      ))
      expect_error(
        get_llm(),
        "Config is missing URL or key. Use set_llm() to reconfigure.",
        fixed = TRUE
      )
    })

    it("errors when the stored config names an unknown provider", {
      # Given: a hand-written config with LLM_provider: 'nope'
      # When:  get_llm()
      # Then:  the error names the offending provider and points at set_llm()
      .write_raw_config(.local_config_dir(), c(
        "default:",
        "  LLM_provider: 'nope'",
        "  LLM_URL: 'https://example.org/chat'",
        "  LLM_key: 'sk-1234567890abcdef'",
        "  LLM_model: 'test-model'"
      ))
      expect_error(
        get_llm(),
        "Unknown provider 'nope' in config. Run set_llm() to reconfigure.",
        fixed = TRUE
      )
    })

  })

  describe("show_key argument validation", {

    it("rejects non-logical, NA and length > 1 input", {
      # Given: a valid stored config
      # When:  get_llm(show_key = "yes"), get_llm(show_key = NA),
      #   get_llm(show_key = c(TRUE, TRUE))
      # Then:  each errors naming 'show_key' and showing the working call
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      for (bad in list("yes", NA, c(TRUE, TRUE))) {
        err <- tryCatch(get_llm(show_key = bad), error = function(e) conditionMessage(e))
        expect_match(err, "'show_key' must be a single TRUE or FALSE.", fixed = TRUE)
        expect_match(err, "get_llm(show_key = TRUE)", fixed = TRUE)
      }
    })

  })

})
