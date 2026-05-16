# Shared test helpers

skip_if_no_llm <- function() {
  config_file <- file.path(tools::R_user_dir("llmjoin", "config"), "LLMJOIN.yml")
  if (!file.exists(config_file)) {
    skip("LLM service not configured. Run set_llm() first.")
  }
}
