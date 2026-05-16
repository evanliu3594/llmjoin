# Provider registry
.providers <- list(
  openai = list(
    name = "openai",
    base_url = "https://api.openai.com/v1",
    endpoint = "/chat/completions",
    default_model = "gpt-5.4-mini",
    auth_type = "bearer"
  ),
  claude = list(
    name = "claude",
    base_url = "https://api.anthropic.com/v1",
    endpoint = "/messages",
    default_model = "claude-haiku-4-5",
    auth_type = "x-api-key"
  ),
  gemini = list(
    name = "gemini",
    base_url = "https://generativelanguage.googleapis.com/v1beta/openai",
    endpoint = "/chat/completions",
    default_model = "gemini-3-flash",
    auth_type = "bearer"
  )
)

#' Build authentication headers for a provider
#' @noRd
provider_headers <- function(provider, key) {
  p <- .providers[[provider]]
  headers <- list(`Content-Type` = "application/json")

  switch(p$auth_type,
    bearer = {
      headers$Authorization <- paste("Bearer", key)
    },
    `x-api-key` = {
      headers$`x-api-key` <- key
      headers$`anthropic-version` <- "2023-06-01"
    },
    stop("Unknown auth_type: ", p$auth_type)
  )

  headers
}

#' Build request body for a provider
#' @noRd
provider_body <- function(provider, model, message, temperature, max_tokens) {
  switch(provider,
    openai = , gemini = {
      list(
        model = model,
        messages = list(list(role = "user", content = message)),
        max_tokens = max_tokens,
        temperature = temperature
      )
    },
    claude = {
      list(
        model = model,
        max_tokens = max_tokens,
        messages = list(list(role = "user", content = message)),
        temperature = temperature
      )
    },
    stop("Unknown provider: ", provider)
  )
}

#' Parse LLM response text from a provider
#' @noRd
provider_parse <- function(provider, parsed_json) {
  switch(provider,
    openai = , gemini = {
      if (is.null(parsed_json$choices) || length(parsed_json$choices) == 0) {
        stop("Invalid response structure: no choices found")
      }
      msg <- parsed_json$choices[[1]]$message$content
      if (is.null(msg) || msg == "") {
        stop(
          "Invalid response structure: empty message content. ",
          "Reasoning models may consume all max_tokens on reasoning. ",
          "Try increasing .max_tokens (e.g., 16000)."
        )
      }
      as.character(msg)
    },
    claude = {
      if (is.null(parsed_json$content) || length(parsed_json$content) == 0) {
        stop("Invalid response structure: no content found")
      }
      text_blocks <- Filter(
        function(b) identical(b$type, "text"),
        parsed_json$content
      )
      if (length(text_blocks) == 0) {
        stop("Invalid response structure: no text block found")
      }
      as.character(text_blocks[[1]]$text)
    },
    stop("Unknown provider: ", provider)
  )
}

#' Build full endpoint URL for a provider
#' @noRd
provider_url <- function(provider, base_url) {
  paste0(base_url, .providers[[provider]]$endpoint)
}
