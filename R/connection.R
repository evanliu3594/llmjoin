#' Test LLM service with minimal request
#' @return list with status and message
#' @export
test_llm_service_minimal <- function() {

  config_path <- "~/.LLMJOIN.yml"

  if (!file.exists(config_path)) {
    return(list(success = FALSE, message = "Config file not found"))
  }

  config <- config::get(file = config_path, use_parent = FALSE)
  provider <- config$LLM_provider %||% "openai"
  if (!provider %in% names(.providers)) {
    return(list(success = FALSE, message = paste("Unknown provider:", provider)))
  }
  model <- config$LLM_model %||% .providers[[provider]]$default_model
  key <- config$LLM_key
  url <- config$LLM_URL

  tryCatch(
    {
      body <- provider_body(provider, model, "hi", 0, 1)
      headers <- do.call(httr::add_headers, provider_headers(provider, key))

      response <- httr::POST(
        url = url,
        headers,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(10)
      )

      status <- httr::status_code(response)

      if (status == 200) {
        return(list(success = TRUE, message = "Service is working"))
      } else if (status == 401) {
        return(list(
          success = FALSE,
          message = "Authentication failed - check your API key"
        ))
      } else if (status == 404) {
        return(list(
          success = FALSE,
          message = "Endpoint not found - check your URL"
        ))
      } else {
        error_content <- httr::content(response, "text")
        return(list(
          success = FALSE,
          message = paste("Service error:", error_content)
        ))
      }
    },
    error = \(e) {
      return(list(
        success = FALSE,
        message = paste("Connection error:", e$message)
      ))
    }
  )
}

#' Validate LLM configuration with multiple checks
#' @return list with detailed validation results
#' @export
validate_llm_config <- function() {

  config_path <- "~/.LLMJOIN.yml"

  # 1. Check if the configuration file exists.
  if (!file.exists(config_path)) {
    stop("Configuration file not set. Use `set_llm()` to set up your LLM services.")
  }

  # 2. Check whether the configuration content is valid.
  config <- tryCatch(
    config::get(file = config_path, use_parent = FALSE),
    error = \(e) stop(paste("Invalid config file:", e$message))
  )

  if (is.null(config) || is.null(config$LLM_URL) || is.null(config$LLM_key)) {
    stop("Invalid configuration: missing URL or key.")
  }

  # 3. Check if the configuration has already been verified
  if (isTRUE(config$VERIFIED)) {
    cat("Configuration already verified, pass verification.\n")
    return(config)
  }
  cat("Using new LLM service, verifying ...\n")

  # 4. Check URL format
  if (!grepl("^https?://", config$LLM_URL)) {
    stop("Invalid URL format")
  }

  # 5. Test Network Connection (Simple Ping)
  cat("Test Network Connection...\n")
  url_test <- tryCatch(
    httr::HEAD(config$LLM_URL, httr::timeout(5)),
    error = \(e) stop(paste("Cannot reach URL:", e$message))
  )

  if (!is.null(url_test)) {
    cat("Network Connection Test Passed.\n")
  } else {
    stop("Network Connection Test Failed.\n")
  }

  # 6. Test Authentication (Send a Minimal Request)
  cat("Test Authentication...\n")
  auth_test <- test_llm_service_minimal()
  if (auth_test$success) {
    cat("LLM service configured and working correctly!\n")

    # 7. Rewrite config with VERIFIED flag properly nested
    provider <- config$LLM_provider %||% "openai"
    model <- config$LLM_model %||% .providers[[provider]]$default_model
    config_content <- sprintf(
      'default:\n  LLM_provider: "%s"\n  LLM_URL: "%s"\n  LLM_key: "%s"\n  LLM_model: "%s"\n  VERIFIED: true',
      provider, config$LLM_URL, config$LLM_key, model
    )
    writeLines(config_content, config_path)
    config$VERIFIED <- TRUE
    return(config)
  } else {
    stop("There might be an issue with the authentication, causing your LLM service to be inaccessible.")
  }

}

#' set up your LLM service
#' @description Set up your LLM service. Supports OpenAI, Claude (Anthropic),
#'   and Gemini (via OpenAI-compatible endpoint). For custom endpoints
#'   (Ollama, proxies, DeepSeek, Kimi, etc.), use provider = "openai" with
#'   the custom URL.
#'   All information will be stored in your system env and won't upload to anywhere else.
#'   Rest assured, your privacy is secure.
#'
#' @param provider character, LLM provider. One of "openai",
#'   "claude", "gemini". Default "openai".
#' @param url url to your LLM provider endpoint. If NULL, auto-set from provider.
#' @param key api-key of your service.
#' @param model character, model name. If NULL, auto-set from provider default.
#'
#' @export
#'
set_llm <- function(provider = "openai", url = NULL, key = NULL, model = NULL) {
  # Validate provider
  provider <- tolower(provider)
  if (!provider %in% names(.providers)) {
    stop("Unknown provider '", provider,
         "'. Supported: ", paste(names(.providers), collapse = ", "))
  }

  # Require key
  if (is.null(key) || !is.character(key) || key == "") {
    stop("'key' must be provided")
  }

  p <- .providers[[provider]]

  # Resolve URL
  if (is.null(url)) {
    url <- provider_url(provider, p$base_url)
  } else if (!grepl("^https?://", url)) {
    warning("URL should start with 'http://' or 'https://'")
  }

  # Resolve model
  if (is.null(model)) {
    model <- p$default_model
  }

  # Write configuration
  config_content <- sprintf(
    'default:\n  LLM_provider: "%s"\n  LLM_URL: "%s"\n  LLM_key: "%s"\n  LLM_model: "%s"',
    provider, url, key, model
  )

  tryCatch(
    {
      writeLines(config_content, "~/.LLMJOIN.yml")
      cat("LLM services stored in `~/.LLMJOIN.yml`.\n")
      cat("  Provider:", provider, "\n")
      cat("  Model:", model, "\n")
      cat("  URL:", url, "\n")
    },
    error = function(e) {
      stop("Failed to write config file: ", e$message)
    }
  )
}

#' Send message to LLM server
#'
#' This function send message to LLM model and retrive the result.
#'
#' @param .message the message to send.
#' @param .model character, LLM model to use. By default NULL (uses config value).
#' @param .temperature OpenAI style randomness control (0~1), by default 0.
#' @param .max_tokens Max tokens to spend.
#' @param .timeout Max seconds to communicate with LLM
#'
#' @returns LLM answer, strings
#' @export
#'
#' @examples
#' chat_llm("tell a joke.")
chat_llm <- function(
  .message,
  .model = NULL,
  .temperature = 0,
  .max_tokens = 1000,
  .timeout = 30
) {
  # VERIFY PARAMS
  if (missing(.message) || is.null(.message) || .message == "") {
    stop("Message cannot be empty")
  }

  if (.temperature < 0 || .temperature > 1) {
    warning("Temperature must be between 0 and 1")
    .temperature <- min(max(.temperature, 0), 1)
  }

  LLMJOIN_CONFIG <- validate_llm_config()

  # RESOLVE PROVIDER AND MODEL
  provider <- LLMJOIN_CONFIG$LLM_provider %||% "openai"
  if (!provider %in% names(.providers)) {
    stop("Unknown provider '", provider, "' in config. Run set_llm() to reconfigure.")
  }
  model <- .model %||% LLMJOIN_CONFIG$LLM_model %||% .providers[[provider]]$default_model

  # BUILD URL
  url <- LLMJOIN_CONFIG$LLM_URL

  # BUILD REQUEST
  body <- provider_body(provider, model, as.character(.message), .temperature, .max_tokens)
  headers <- do.call(httr::add_headers, provider_headers(provider, LLMJOIN_CONFIG$LLM_key))

  # SEND REQUEST
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
    error = \(e) stop("Request failed: ", e$message)
  )

  # PROCESS RESPONSE
  status <- httr::status_code(response)

  if (status == 200) {
    content_text <- httr::content(response, "text", encoding = "UTF-8")

    tryCatch(
      {
        content <- jsonlite::fromJSON(content_text)
        provider_parse(provider, content)
      },
      error = \(e) {
        stop("Failed to parse response: ", e$message, "\nRaw response: ", content_text)
      }
    )
  } else {
    error_msg <- httr::content(response, "text")
    stop("API request failed with status ", status, ": ", error_msg)
  }
}
