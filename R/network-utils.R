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
  if (!is.null(url) & !is.null(key)) {
    config_content <- sprintf('default:\n  LLM_URL: "%s"\n  LLM_key: "%s"', url, key)

    writeLines(config_content, "~/.LLMJOIN.yml")
    cat("LLM services stored in `~/.LLMJOIN.yml`. \n")
  }

  cat("Testing service...\n")
  responce <- chat_llm(.message = "This is a connection test. If you receive this message, please reply with 'Yes'")
  if (responce == "Yes") {
    cat("LLM service configured successfully!")
  }

}

#' Send message to LLM server
#' 
#' This function send message to LLM model and retrive the result.
#'
#' @param .message the message to send.
#' @param .model character, LLM model to use. By default gpt-4.1-nano.
#' @param .temperature GPT style randomness control (0~1), by default 0.1, the larger, the more rigorous.
#'
#' @returns LLM answer, strings
#' @export
#'
#' @examples
#' chat_llm("tell a joke.")
chat_llm <- function(
  .message,
  .model = "gpt-4.1-nano",
  .temperature = 0.1
) {

  # 根据set_LLM中的设置构建chat config
  if (file.exists("~/.LLMJOIN.yml")) {
    LLMJOIN_CONFIG <- config::get(file = "~/.LLMJOIN.yml", use_parent = FALSE)
  } else {
    stop("No LLM service detected, please setup LLM service using `set_llm()`.")
  }

  config <- list(
    url = LLMJOIN_CONFIG$LLM_URL,
    model = .model,
    auth_header = paste("Bearer", LLMJOIN_CONFIG$LLM_key)
  )

  # 构建http request
  body <- list(
    model = config$model,
    messages = list(list(role = "user", content = .message)),
    temperature = .temperature
  )

  headers <- add_headers(
    "Authorization" = config$auth_header,
    "Content-Type" = "application/json"
  )

  # 发送请求
  response <- POST(
    url = config$url,
    headers,
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  # 处理响应
  if (status_code(response) == 200) {
    content <- fromJSON(content(response, "text"))    
    return(content$choices$message$content)

  } else {
    stop("API_FAIL: ", content(response, "text"))
  }
}