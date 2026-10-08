#' Set up your LLM service
#' @description Set up your LLM service with native support for OpenAI, Claude (Anthropic), Gemini (via OpenAI-compatible endpoint), and DeepSeek.
#'   For custom endpoints like Ollama, proxies, Kimi, and others, use provider = "openai" along with your custom URL to connect through the compatible API interface.
#'   All information is stored strictly locally in your system configuration and is never uploaded or shared.
#'
#' @param provider character, LLM provider. One of "openai",
#'   "claude", "gemini", "deepseek". Default "openai".
#' @param url url to your LLM provider endpoint. If NULL, auto-set based on provider.
#' @param key api-key of your service. Prefer passing it from the environment
#'   (\code{Sys.getenv("LLMJOIN_API_KEY")}) over a string literal if you want to
#'   keep it out of your \code{.Rhistory}; the package never prints the stored key
#'   back (see \code{\link{get_llm}()}, which masks it by default).
#' @param model character, model name. If NULL, auto-set from provider default.
#'
#' @returns NULL invisibly. Called for side effect of writing the config file.
#' @examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))
#' # the key is read from the environment so a deliberate run cannot overwrite
#' # your real config with a placeholder credential
#' set_llm(provider = "openai", key = Sys.getenv("LLMJOIN_API_KEY"))
#' @export
#'
set_llm <- function(provider = "openai", url = NULL, key = NULL, model = NULL) {
  provider <- tolower(provider)
  if (!provider %in% names(.providers)) {
    stop(
      "Unknown provider '",
      provider,
      "'. Supported: ",
      paste(names(.providers), collapse = ", ")
    )
  }

  if (
    is.null(key) ||
    !is.character(key) ||
    length(key) != 1L ||
    is.na(key) ||
    !nzchar(key)
  ) {
    stop(
      "'key' must be a single non-empty string. ",
      "An empty value usually means an environment variable is unset: check ",
      'nzchar(Sys.getenv("LLMJOIN_API_KEY")), and restart R so that .Renviron ',
      "is read."
    )
  }

  p <- .providers[[provider]]

  if (is.null(url)) {
    url <- provider_url(provider, p$base_url)
  } else if (!grepl("^https?://", url)) {
    warning("URL should start with 'http://' or 'https://'")
  }

  if (is.null(model)) {
    model <- p$default_model
  }

  config_content <- sprintf(
    "default:\n  LLM_provider: '%s'\n  LLM_URL: '%s'\n  LLM_key: '%s'\n  LLM_model: '%s'",
    gsub("'", "''", provider, fixed = TRUE),
    gsub("'", "''", url, fixed = TRUE),
    gsub("'", "''", key, fixed = TRUE),
    gsub("'", "''", model, fixed = TRUE)
  )

  tryCatch(
    {
      config_dir <- tools::R_user_dir("llmjoin", "config")
      dir.create(config_dir, showWarnings = FALSE, recursive = TRUE)
      config_file <- file.path(config_dir, "LLMJOIN.yml")
      writeLines(config_content, config_file)
      message("LLM services stored in `", config_file, "`.")
      message("  Provider: ", provider)
      message("  Model: ", model)
      message("  URL: ", url)
    },
    error = function(e) {
      stop("Failed to write config file: ", e$message)
    }
  )
}

#' Read and validate the stored LLM configuration
#' @noRd
.read_config <- function() {
  config_dir <- tools::R_user_dir("llmjoin", "config")
  config_path <- file.path(config_dir, "LLMJOIN.yml")
  if (!file.exists(config_path)) {
    stop(
      "LLM service not configured. Use `set_llm()` to set up your API key and endpoint."
    )
  }
  raw <- tryCatch(
    config::get(file = config_path, use_parent = FALSE),
    error = \(e) {
      stop(
        "Invalid config file (",
        config_path,
        "): ",
        e$message,
        "\nUse set_llm() to reconfigure."
      )
    }
  )
  if (is.null(raw$LLM_URL) || is.null(raw$LLM_key)) {
    stop("Config is missing URL or key. Use set_llm() to reconfigure.")
  }

  provider <- raw$LLM_provider %||% "openai"
  if (!provider %in% names(.providers)) {
    stop(
      "Unknown provider '",
      provider,
      "' in config. Run set_llm() to reconfigure."
    )
  }

  list(
    provider = provider,
    url = raw$LLM_URL,
    key = raw$LLM_key,
    model = raw$LLM_model %||% .providers[[provider]]$default_model,
    config_path = config_path
  )
}

#' Mask a stored API key for display
#' @noRd
.mask_key <- function(key) {
  if (!nzchar(key)) return("<empty>")
  if (nchar(key) <= 8L) return("****")
  paste0("****", substr(key, nchar(key) - 3L, nchar(key)))
}

#' Show the current LLM service configuration
#' @description Reads the configuration written by \code{\link{set_llm}()} and
#'   reports it, so you do not have to open the YAML file yourself. The API key
#'   is masked by default; pass \code{show_key = TRUE} to print it in full.
#'   Requests made by \code{\link{chat_llm}()} always use the stored key as-is,
#'   whatever this function displays.
#'
#' @param show_key logical, print and return the API key in full instead of a
#'   masked form. Default \code{FALSE}.
#'
#' @returns A named list invisibly, with elements \code{provider}, \code{url},
#'   \code{model}, \code{key} (masked unless \code{show_key = TRUE}) and
#'   \code{config_path}. The same fields are reported through \code{message()}.
#' @examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))
#' # the key is read from the environment so a deliberate run cannot overwrite
#' # your real config with a placeholder credential
#' set_llm(provider = "openai", key = Sys.getenv("LLMJOIN_API_KEY"))
#' get_llm()
#' get_llm(show_key = TRUE)
#' @export
get_llm <- function(show_key = FALSE) {
  if (length(show_key) != 1L || !is.logical(show_key) || is.na(show_key)) {
    stop(
      "'show_key' must be a single TRUE or FALSE. ",
      "Use get_llm(show_key = TRUE) to print the full key."
    )
  }

  cfg <- .read_config()
  key_display <- if (isTRUE(show_key)) cfg$key else .mask_key(cfg$key)

  message("LLM config read from `", cfg$config_path, "`.")
  message("  Provider: ", cfg$provider)
  message("  Model: ", cfg$model)
  message("  URL: ", cfg$url)
  message("  Key: ", key_display)
  if (!isTRUE(show_key)) {
    message("  (pass show_key = TRUE to print the full key)")
  }

  cfg$key <- key_display
  invisible(cfg)
}

#' Send message to LLM server
#'
#' This function sends a message to the LLM model and retrieves the result.
#'
#' @param .message the message to send: character, or any input coercible with
#'   \code{\link{as.character}}. A length > 1 input is pasted into a single
#'   string separated by newlines, and NA elements are treated as blank lines.
#'   Missing, NULL, zero-length, or all-blank input (after trimming whitespace)
#'   raises an error.
#' @param .model character, LLM model to use. By default NULL (uses config value).
#' @param .temperature OpenAI style randomness control (0~1), by default 0.
#'   For OpenAI gpt-5+/o-series models the parameter is not sent (the
#'   official API rejects it); a non-zero value triggers a warning.
#' @param .max_tokens Max tokens to spend. Sent as
#'   \code{max_completion_tokens} for OpenAI gpt-5+/o-series models, as
#'   \code{max_tokens} for every other model and provider.
#' @param .timeout Max seconds to communicate with LLM.
#' @param .verbose logical, print progress messages. Default \code{getOption("llmjoin.verbose", FALSE)}.
#'
#' @returns A character string with the LLM's response text.
#' @export
#'
#' @examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))
#'   chat_llm("tell a joke.")
chat_llm <- function(
  .message,
  .model = NULL,
  .temperature = 0,
  .max_tokens = 30000,
  .timeout = 300,
  .verbose = getOption("llmjoin.verbose", FALSE)
) {
  if (missing(.message) || is.null(.message) || length(.message) == 0) {
    stop(
      "'.message' is required. Pass the prompt text, ",
      'e.g. chat_llm(.message = "tell a joke.")'
    )
  }
  msg <- as.character(.message)
  msg[is.na(msg)] <- ""
  .message <- paste(msg, collapse = "\n")
  if (nchar(trimws(.message)) == 0) {
    stop(
      "'.message' must contain non-blank text; all elements were empty or NA. ",
      'Pass the prompt text, e.g. chat_llm(.message = "tell a joke.")'
    )
  }

  if (.temperature < 0 || .temperature > 1) {
    warning("Temperature must be between 0 and 1")
    .temperature <- min(max(.temperature, 0), 1)
  }

  # Load and validate config
  cfg <- .read_config()
  if (!nzchar(cfg$key)) {
    stop(
      "Config has an empty 'LLM_key' in ", cfg$config_path, ". ",
      "Add your API key to that file, or use set_llm() to store a new one."
    )
  }
  provider <- cfg$provider
  model <- .model %||% cfg$model
  url <- cfg$url

  if (.verbose) {
    message("Sending request to ", provider, " using model ", model, "...")
  }
  body <- provider_body(
    provider,
    model,
    .message,
    .temperature,
    .max_tokens
  )
  headers <- do.call(
    httr::add_headers,
    provider_headers(provider, cfg$key)
  )

  response <- tryCatch(
    {
      httr::POST(
        url = url,
        headers,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(.timeout)
      )
    },
    error = \(e) {
      stop(
        "Request failed: ",
        e$message,
        "\n",
        "  URL: ",
        url,
        "\n",
        "  Provider: ",
        provider,
        "\n",
        "  Model: ",
        model
      )
    }
  )

  status <- httr::status_code(response)

  if (status == 200) {
    content_text <- httr::content(response, "text", encoding = "UTF-8")

    tryCatch(
      {
        content <- jsonlite::fromJSON(content_text, simplifyVector = FALSE)
        result <- provider_parse(provider, content)
        if (.verbose) {
          message("Response received (", nchar(content_text), " bytes)")
        }
        result
      },
      error = \(e) {
        detail <- if (.verbose) content_text else substr(content_text, 1, 200)
        stop(
          "Failed to parse response: ",
          e$message,
          "\n",
          "  Provider: ",
          provider,
          "\n",
          "  Model: ",
          model,
          "\n",
          "  URL: ",
          url,
          "\n",
          "  Raw response: ",
          detail
        )
      }
    )
  } else {
    error_msg <- httr::content(response, "text")
    stop(
      "API request failed with status ",
      status,
      "\n",
      "  Provider: ",
      provider,
      "\n",
      "  Model: ",
      model,
      "\n",
      "  URL: ",
      url,
      "\n",
      "  Response: ",
      error_msg
    )
  }
}
