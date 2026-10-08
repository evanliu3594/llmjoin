# TDD/BDD: credential echo safety — the package must never print a plaintext key
# Property under test: every user-visible channel the package owns
#   (set_llm() progress messages, chat_llm() .verbose messages, chat_llm() error
#   texts) carries the provider / model / URL but never the stored API key.
# Why this is a guard, not a red-first test: the current implementation already
#   avoids echoing the key, so these assertions pass on arrival. Their job is to
#   fail loudly the next time someone adds a diagnostic line, a verbose dump or
#   a friendlier error message that interpolates the credential.
# Display masking of get_llm() is pinned separately in test-get_llm.R (A2).
# Tests use a temp R_USER_CONFIG_DIR and fully stubbed HTTP; no real request.

# Helper: fresh config dir for the calling test only; returns the LLMJOIN.yml
# path. Third copy of this 4-line helper (test-chat_llm.R, test-get_llm.R) —
# testthat gives each file its own environment, so keep the copies in sync.
.local_config_dir <- function() {
  withr::local_envvar(
    R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"),
    .local_envir = parent.frame()
  )
  file.path(tools::R_user_dir("llmjoin", "config"), "LLMJOIN.yml")
}

# Fixture: a distinctive key so an accidental echo cannot be confused with the
# provider, model or URL strings that legitimately appear in the same output.
.TEST_KEY <- "sk-1234567890abcdef"

# Helper: collect every message and error text raised by expr into one string.
.all_output_of <- function(expr) {
  collected <- character()
  record <- function(cond) {
    collected <<- c(collected, conditionMessage(cond))
    if (inherits(cond, "message")) invokeRestart("muffleMessage")
  }
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) conditionMessage(e)),
    message = record,
    warning = record
  )
  if (is.character(value)) collected <- c(collected, value)
  paste(collected, collapse = "\n")
}

# NOTE: local_mocked_bindings() must be called directly inside each it() block.
# Wrapping it in a helper would scope the mock's teardown to the helper frame,
# so the bindings revert before the request runs and the real httr::POST would
# hit the live endpoint.

describe("credential echo safety", {

  it("set_llm() reports the service but not the key", {
    # Given: a call that stores a 19-character key
    # When:  every message set_llm() emits is collected
    # Then:  the key appears nowhere, while provider and model do (so the test
    #   would notice a channel that stopped reporting at all)
    .local_config_dir()
    output <- .all_output_of(
      set_llm(provider = "openai", key = .TEST_KEY, model = "test-model")
    )
    expect_false(grepl(.TEST_KEY, output, fixed = TRUE))
    expect_match(output, "Provider: openai", fixed = TRUE)
    expect_match(output, "Model: test-model", fixed = TRUE)
    expect_match(output, "URL: https://api.openai.com/v1/chat/completions", fixed = TRUE)
  })

  it("chat_llm() transport failure error omits the key", {
    # Given: a stored config and a POST that raises a connection error
    # When:  chat_llm(.message = "hi") fails
    # Then:  the error names URL / provider / model but never the key
    .local_config_dir()
    suppressMessages(
      set_llm(provider = "openai", key = .TEST_KEY, model = "test-model")
    )
    local_mocked_bindings(
      POST = function(url, ...) stop("Failed to connect: connection refused"),
      content = function(x, ...) "",
      status_code = function(x) 0L,
      .package = "httr"
    )
    output <- .all_output_of(chat_llm(.message = "hi"))
    expect_false(grepl(.TEST_KEY, output, fixed = TRUE))
    expect_match(output, "Failed to connect", fixed = TRUE)
    expect_match(output, "Model: test-model", fixed = TRUE)
  })

  it("chat_llm() non-200 error omits the key", {
    # Given: a stored config and a 401 reply from the provider
    # When:  chat_llm(.message = "hi") raises the status error
    # Then:  the status code and the provider's own message are reported, the
    #   key is not
    .local_config_dir()
    suppressMessages(
      set_llm(provider = "openai", key = .TEST_KEY, model = "test-model")
    )
    fake <- structure(list(status_code = 401L), class = "response")
    local_mocked_bindings(
      POST = function(url, ...) fake,
      content = function(x, ...) '{"error":{"message":"Invalid Authentication"}}',
      status_code = function(x) 401L,
      .package = "httr"
    )
    output <- .all_output_of(chat_llm(.message = "hi"))
    expect_false(grepl(.TEST_KEY, output, fixed = TRUE))
    expect_match(output, "API request failed with status 401", fixed = TRUE)
    expect_match(output, "Invalid Authentication", fixed = TRUE)
  })

  it("chat_llm() parse failure with .verbose omits the key", {
    # Given: a 200 response whose body is not JSON, and .verbose = TRUE (the
    #   branch that prints the raw response in full)
    # When:  chat_llm(.message = "hi") fails to parse
    # Then:  the raw body is echoed for debugging, the key is not
    .local_config_dir()
    suppressMessages(
      set_llm(provider = "openai", key = .TEST_KEY, model = "test-model")
    )
    fake <- structure(list(status_code = 200L), class = "response")
    local_mocked_bindings(
      POST = function(url, ...) fake,
      content = function(x, ...) "gateway returned plain text",
      status_code = function(x) 200L,
      .package = "httr"
    )
    output <- .all_output_of(chat_llm(.message = "hi", .verbose = TRUE))
    expect_false(grepl(.TEST_KEY, output, fixed = TRUE))
    expect_match(output, "Failed to parse response", fixed = TRUE)
    expect_match(output, "gateway returned plain text", fixed = TRUE)
  })

  it("chat_llm() .verbose progress messages omit the key", {
    # Given: a working stubbed round trip
    # When:  chat_llm(.message = "hi", .verbose = TRUE) succeeds
    # Then:  the progress lines report provider and model only
    .local_config_dir()
    suppressMessages(
      set_llm(provider = "openai", key = .TEST_KEY, model = "test-model")
    )
    fake <- structure(list(status_code = 200L), class = "response")
    local_mocked_bindings(
      POST = function(url, ...) fake,
      content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
      status_code = function(x) 200L,
      .package = "httr"
    )
    output <- .all_output_of(chat_llm(.message = "hi", .verbose = TRUE))
    expect_false(grepl(.TEST_KEY, output, fixed = TRUE))
    expect_match(output, "Sending request to openai using model test-model", fixed = TRUE)
    expect_match(output, "Response received", fixed = TRUE)
  })

  it("the collector does notice a plaintext key (positive control)", {
    # Given: the one supported way to print the key — get_llm(show_key = TRUE)
    # When:  its output is collected the same way as the five cases above
    # Then:  the key IS found, proving the checks above are not passing because
    #   the collector or the matching is broken
    .local_config_dir()
    suppressMessages(
      set_llm(provider = "openai", key = .TEST_KEY, model = "test-model")
    )
    output <- .all_output_of(get_llm(show_key = TRUE))
    expect_true(grepl(.TEST_KEY, output, fixed = TRUE))
  })

})
