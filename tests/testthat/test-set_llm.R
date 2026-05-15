# TDD/BDD: set_llm() — Configure LLM service settings
# Contract: given valid provider and key, writes config to ~/.LLMJOIN.yml.
# Contract: given unknown provider, errors.
# Contract: given empty key, errors.

describe("set_llm", {

  it("should error when provider is not recognized", {
    # Given: an invalid provider name
    # When & Then: calling set_llm with bad provider should error
    expect_error(
      set_llm(provider = "nonexistent", key = "sk-test"),
      "Unknown provider"
    )
  })

  it("should error when key is NULL", {
    # Given: NULL API key
    # When & Then: should error with clear message
    expect_error(
      set_llm(provider = "openai", key = NULL),
      "'key' must be provided"
    )
  })

  it("should error when key is empty string", {
    # Given: empty API key
    # When & Then: should error
    expect_error(
      set_llm(provider = "openai", key = ""),
      "'key' must be provided"
    )
  })

  it("should write config to ~/.LLMJOIN.yml with correct provider and key", {
    # Given: no existing config (backup if present)
    config_path <- path.expand("~/.LLMJOIN.yml")
    backup_path <- paste0(config_path, ".test-backup")
    if (file.exists(config_path)) {
      file.rename(config_path, backup_path)
      on.exit(file.rename(backup_path, config_path))
    } else {
      on.exit(unlink(config_path))
    }

    # When: setting LLM config with valid provider and key
    set_llm(provider = "openai", key = "sk-test123")

    # Then: config file was created
    expect_true(file.exists(config_path))

    # Then: config content contains provider and key
    config_content <- paste(readLines(config_path), collapse = "\n")
    expect_match(config_content, 'LLM_provider: "openai"')
    expect_match(config_content, 'LLM_key: "sk-test123"')
  })

  it("should auto-resolve URL from provider when URL not given", {
    # Given: openai provider without explicit URL
    config_path <- path.expand("~/.LLMJOIN.yml")
    backup_path <- paste0(config_path, ".test-backup")
    if (file.exists(config_path)) {
      file.rename(config_path, backup_path)
      on.exit(file.rename(backup_path, config_path))
    } else {
      on.exit(unlink(config_path))
    }

    # When: setting LLM config without URL
    set_llm(provider = "openai", key = "sk-test")

    # Then: URL auto-resolved to OpenAI endpoint
    config_content <- paste(readLines(config_path), collapse = "\n")
    expect_match(config_content, "api.openai.com")
  })

  it("should warn when URL does not start with http", {
    # Given: a URL without protocol
    config_path <- path.expand("~/.LLMJOIN.yml")
    backup_path <- paste0(config_path, ".test-backup")
    if (file.exists(config_path)) {
      file.rename(config_path, backup_path)
      on.exit(file.rename(backup_path, config_path))
    } else {
      on.exit(unlink(config_path))
    }

    # When & Then: should warn about URL format
    expect_warning(
      set_llm(provider = "openai", key = "sk-test", url = "bad-url"),
      "URL should start with"
    )
  })

})
