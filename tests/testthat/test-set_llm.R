# TDD/BDD: set_llm() — key argument guard
# Contract: 'key' must be a single non-empty string. Every other shape (missing,
#   NULL, "", character(0), length > 1, NA_character_) raises the SAME error text,
#   which names the argument and says what to check — not an internal
#   "condition has length > 1" / "argument is of length zero" error.
# Why this matters now: the README tells users to pass Sys.getenv("LLMJOIN_API_KEY").
#   If that variable is unset the call receives "", and the guidance has to say so.
# Contract: provider validation still runs before the key check, and a valid call
#   writes the config and reports provider / model / URL (never the key — pinned
#   in test-no_key_leak.R).
# Tests use a temp R_USER_CONFIG_DIR; helper duplicated per tests/AGENTS.md.

# Helper: fresh config dir for the calling test only; returns the LLMJOIN.yml
# path. Fourth copy of this 4-line helper — keep the copies in sync.
.local_config_dir <- function() {
  withr::local_envvar(
    R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"),
    .local_envir = parent.frame()
  )
  file.path(tools::R_user_dir("llmjoin", "config"), "LLMJOIN.yml")
}

describe("set_llm", {

  describe("key guard (every wrong shape gives the same guidance)", {

    it("errors when key is missing", {
      # Given: set_llm(provider = "openai") with no key
      # When:  the call runs
      # Then:  the error names 'key' as a single non-empty string
      .local_config_dir()
      err <- tryCatch(
        set_llm(provider = "openai"),
        error = function(e) conditionMessage(e)
      )
      expect_match(err, "'key' must be a single non-empty string.", fixed = TRUE)
    })

    it("errors when key is an empty string (the unset-variable case)", {
      # Given: key = "" — what Sys.getenv() returns when the variable is unset
      # When:  set_llm(key = "")
      # Then:  the error names the argument AND points at the environment check
      .local_config_dir()
      err <- tryCatch(
        set_llm(provider = "openai", key = ""),
        error = function(e) conditionMessage(e)
      )
      expect_match(err, "'key' must be a single non-empty string.", fixed = TRUE)
      expect_match(err, 'nzchar(Sys.getenv("LLMJOIN_API_KEY"))', fixed = TRUE)
      expect_match(err, ".Renviron", fixed = TRUE)
    })

    it("errors when key is NULL, NA, zero-length or length > 1", {
      # Given: key in {NULL, NA_character_, character(0), c("a","b"), 123}
      # When:  set_llm(key = <each>)
      # Then:  each raises the same guidance error rather than an internal
      #   "condition has length > 1" / "argument is of length zero" message
      .local_config_dir()
      for (bad in list(NULL, NA_character_, character(0), c("a", "b"), 123)) {
        err <- tryCatch(
          set_llm(provider = "openai", key = bad),
          error = function(e) conditionMessage(e)
        )
        expect_match(err, "'key' must be a single non-empty string.", fixed = TRUE)
        expect_false(grepl("length > 1", err, fixed = TRUE))
        expect_false(grepl("length zero", err, fixed = TRUE))
      }
    })

  })

  describe("happy path and check order", {

    it("writes the config and reports the service", {
      # Given: a valid provider and key in a temp config dir
      # When:  set_llm() succeeds
      # Then:  the file exists and the messages name provider / model / URL
      cfg_path <- .local_config_dir()
      collected <- character()
      withCallingHandlers(
        set_llm(provider = "deepseek", key = "test-key", model = "test-model"),
        message = function(m) {
          collected <<- c(collected, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
      output <- paste(collected, collapse = "")
      expect_true(file.exists(cfg_path))
      expect_match(output, "Provider: deepseek", fixed = TRUE)
      expect_match(output, "Model: test-model", fixed = TRUE)
      expect_match(output, "URL: https://api.deepseek.com/chat/completions", fixed = TRUE)
    })

    it("rejects an unknown provider before checking the key", {
      # Given: a bad provider AND a missing key
      # When:  set_llm(provider = "nope")
      # Then:  the provider error wins (order unchanged from 0.3.1)
      .local_config_dir()
      err <- tryCatch(
        set_llm(provider = "nope"),
        error = function(e) conditionMessage(e)
      )
      expect_match(err, "Unknown provider 'nope'", fixed = TRUE)
      expect_match(err, "Supported:", fixed = TRUE)
    })

  })

})
