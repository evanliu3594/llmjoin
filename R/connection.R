#' Set up your LLM service
#' @description Set up your LLM service. Supports OpenAI, Claude (Anthropic),
#'   and Gemini (via OpenAI-compatible endpoint). For custom endpoints
#'   (Ollama, proxies, DeepSeek, Kimi, etc.), use provider = "openai" with
#'   the custom URL.
#'   All information is stored locally in your system configuration and is never
#'   uploaded or shared.
#'
#' @param provider character, LLM provider. One of "openai",
#'   "claude", "gemini". Default "openai".
#' @param url url to your LLM provider endpoint. If NULL, auto-set based on provider.
#' @param key api-key of your service.
#' @param model character, model name. If NULL, auto-set from provider default.
#'
#' @returns NULL invisibly. Called for side effect of writing the config file.
#' @examples
#' \dontrun{
#'   set_llm(provider = "openai", key = "<your-openai-api-key>", model = "gpt-5.4-mini")
#' }
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

  if (is.null(key) || !is.character(key) || key == "") {
    stop("'key' must be provided")
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

#' Send message to LLM server
#'
#' This function sends a message to the LLM model and retrieves the result.
#'
#' @param .message the message to send.
#' @param .model character, LLM model to use. By default NULL (uses config value).
#' @param .temperature OpenAI style randomness control (0~1), by default 0.
#' @param .max_tokens Max tokens to spend.
#' @param .timeout Max seconds to communicate with LLM.
#' @param .verbose logical, print progress messages. Default \code{getOption("llmjoin.verbose", FALSE)}.
#'
#' @returns A character string with the LLM's response text.
#' @export
#'
#' @examples
#' \dontrun{
#' chat_llm("tell a joke.")
#' }
chat_llm <- function(
  .message,
  .model = NULL,
  .temperature = 0,
  .max_tokens = 30000,
  .timeout = 300,
  .verbose = getOption("llmjoin.verbose", FALSE)
) {
  if (missing(.message) || is.null(.message) || .message == "") {
    stop("Message cannot be empty")
  }

  if (.temperature < 0 || .temperature > 1) {
    warning("Temperature must be between 0 and 1")
    .temperature <- min(max(.temperature, 0), 1)
  }

  # Load and validate config
  config_dir <- tools::R_user_dir("llmjoin", "config")
  config_path <- file.path(config_dir, "LLMJOIN.yml")
  if (!file.exists(config_path)) {
    stop(
      "LLM service not configured. Use `set_llm()` to set up your API key and endpoint."
    )
  }
  LLMJOIN_CONFIG <- tryCatch(
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
  if (is.null(LLMJOIN_CONFIG$LLM_URL) || is.null(LLMJOIN_CONFIG$LLM_key)) {
    stop("Config is missing URL or key. Use set_llm() to reconfigure.")
  }

  provider <- LLMJOIN_CONFIG$LLM_provider %||% "openai"
  if (!provider %in% names(.providers)) {
    stop(
      "Unknown provider '",
      provider,
      "' in config. Run set_llm() to reconfigure."
    )
  }
  model <- .model %||%
    LLMJOIN_CONFIG$LLM_model %||%
    .providers[[provider]]$default_model
  url <- LLMJOIN_CONFIG$LLM_URL

  if (.verbose) {
    message("Sending request to ", provider, " using model ", model, "...")
  }
  body <- provider_body(
    provider,
    model,
    as.character(.message),
    .temperature,
    .max_tokens
  )
  headers <- do.call(
    httr::add_headers,
    provider_headers(provider, LLMJOIN_CONFIG$LLM_key)
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
