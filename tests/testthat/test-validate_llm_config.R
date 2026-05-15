# TDD/BDD: validate_llm_config() — Validate the LLMJOIN YAML configuration
# Contract: given no config file, errors.
# Contract: given valid config that is cached as VERIFIED, passes through.

describe("validate_llm_config", {

  it("should error when config file does not exist", {
    # Given: the config path exists but we temporarily rename it
    config_path <- path.expand("~/.LLMJOIN.yml")
    backup_path <- paste0(config_path, ".test-backup")

    if (file.exists(config_path)) {
      file.rename(config_path, backup_path)
      on.exit(file.rename(backup_path, config_path))
    }

    # When & Then: should error clearly
    expect_error(validate_llm_config(), "Configuration file not set")
  })

  it("should skip verification when config is already VERIFIED", {
    # Given: a valid config exists (the one we just created via set_llm)
    config_path <- path.expand("~/.LLMJOIN.yml")
    backup_path <- paste0(config_path, ".test-backup")

    if (file.exists(config_path)) {
      file.rename(config_path, backup_path)
      on.exit(file.rename(backup_path, config_path))
    }

    # Write a temp config with VERIFIED = TRUE
    config_content <- sprintf(
      'default:\n  LLM_provider: "openai"\n  LLM_URL: "https://api.openai.com/v1/chat/completions"\n  LLM_key: "sk-test"\n  LLM_model: "gpt-4.1-nano"\n  VERIFIED: true'
    )
    writeLines(config_content, config_path)

    # When: validating config
    result <- validate_llm_config()

    # Then: returns config with VERIFIED flag
    expect_true(isTRUE(result$VERIFIED))
    expect_equal(result$LLM_provider, "openai")
  })

})
