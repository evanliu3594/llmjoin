#' Test LLM service with minimal request
#' @return list with status and message
#' @export
test_llm_service_minimal <- function() {

  if (!file.exists("~/.LLMJOIN.yml")) {
    return(list(success = FALSE, message = "Config file not found"))
  }

  tryCatch(
    {
      config <- config::get(file = "~/.LLMJOIN.yml", use_parent = FALSE)
      # minimum request
      body <- list(
        model = "gpt-4.1-nano",
        messages = list(list(role = "user", content = "hi")),
        max_tokens = 1,
        temperature = 0
      )

      response <- httr::POST(
        url = config$LLM_URL,
        httr::add_headers(
          "Authorization" = paste("Bearer", config$LLM_key),
          "Content-Type" = "application/json"
        ),
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

#' Validate LLM configuration with multiple checks and return 
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
    stop("Invalid configuration: missing URL or key.\n")
  }

  # 3. Check if the configuration has already been verified
  if (!is.null(config$VERIFIED) && config$VERIFIED == TRUE) {
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
  auth_test <- test_llm_service_minimal()()
  if (auth_test$success) {
    cat("LLM service configured and working correctly!\n") 
    
    # 7. Write verification status to the configuration file
    write_lines("  VERIFIED : true", config_path, append = TRUE)
    return(config)
  } else {
    stop("There might be an issue with the authentication, causing your LLM service to be inaccessible.")
  }
  
}

#' set up your LLM service
#' @description Set up your LLM service (of course you can use your local ollama).
#' All information will be stored in your system env and won't upload to anywhere else.
#' Rest assured, your privacy is secure.
#'
#' @param url url to your LLM provider
#' @param key api-key of your service
#'
#' @export
#'
set_llm <- function(url = NULL, key = NULL) {
  # Format Check
  if (is.null(url) || is.null(key)) {
    stop("Both 'url' and 'key' must be provided")
  }

  if (!is.character(url) || !is.character(key)) {
    stop("Both 'url' and 'key' must be character strings")
  }

  if (!grepl("^https?://", url)) {
    warning("URL should start with 'http://' or 'https://'")
  }

  # Create configuration content
  config_content <- sprintf('default:\n  LLM_URL: "%s"\n  LLM_key: "%s"',url, key)

  # Safely write to a file
  tryCatch(
    {
      writeLines(config_content, "~/.LLMJOIN.yml")
      cat("LLM services stored in `~/.LLMJOIN.yml`.\n")
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
#' @param .model character, LLM model to use. By default gpt-4.1-mini.
#' @param .temperature OpenAI style randomness control (0~1), by default 0.1, the larger, the more rigorous.
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
  .model = "gpt-4.1-mini",
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

  # BUILD REQUEST
  body <- list(
    model = .model,
    messages = list(list(role = "user", content = as.character(.message))),
    temperature = .temperature,
    max_tokens = .max_tokens
  )

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", LLMJOIN_CONFIG$LLM_key),
    "Content-Type" = "application/json"
  )

  # SEND REQUEST
  response <- tryCatch(
    {
      POST(
        url = LLMJOIN_CONFIG$LLM_URL,
        headers,
        body = toJSON(body, auto_unbox = TRUE),
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
        content <- fromJSON(content_text)

        # CHECK RESPONSE
        if (is.null(content$choices) || length(content$choices) == 0) {
          stop("Invalid response structure: no choices found")
        }

        message_content <- content$choices$message$content

        if (is.null(message_content)) {
          stop("Invalid response structure: no message content found")
        }

        return(as.character(message_content))
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