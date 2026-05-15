# Shared test helpers

skip_if_no_llm <- function() {
  result <- test_llm_service_minimal()
  if (!isTRUE(result$success)) {
    skip(paste("LLM service not available:", result$message))
  }
}
