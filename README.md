
# llmjoin: LLM-Powered Fuzzy Join for R <img src="man/figures/logo.png" align="right" width="150" />

[![CRAN version](https://www.r-pkg.org/badges/version/llmjoin)](https://cran.r-project.org/package=llmjoin)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.2.0](https://img.shields.io/badge/R-%3E%3D4.2.0-blue)](https://www.r-project.org/)

## Introduction
llmjoin is an R package designed to use Large Language Models (LLMs, such as GPT-5, Claude, DeepSeek, etc.) for fuzzy joining of data.frames. When the key columns of two data.frames have spelling differences, are in different languages, or cannot be matched exactly, llmjoin can automatically generate prompts and utilize LLMs to assist in high-quality joining.

## Installation
You can install the released version of llmjoin from [CRAN](https://cran.r-project.org/package=llmjoin) with:
```R
install.packages("llmjoin")
```

Or install the development version from [GitHub](https://github.com/evanliu3594/llmjoin) with:
```R
devtools::install_github("evanliu3594/llmjoin")
```

## Usage

### 1. setup your LLM services.

You need an API key from your own provider first — for example, [apply for a DeepSeek API key](https://platform.deepseek.com).

> Please note that all information is stored strictly locally in your system configuration, and is never uploaded or shared. Run `get_llm()` at any time to see the active provider, model, endpoint and config file path — the API key is masked, use `get_llm(show_key = TRUE)` to reveal it.
```R
library(llmjoin)

# OpenAI
set_llm(provider = "openai", key = "your-api-key")

# Claude (Anthropic)
set_llm(provider = "claude", key = "your-api-key")

# Gemini (via OpenAI-compatible endpoint)
set_llm(provider = "gemini", key = "your-api-key")

# DeepSeek
set_llm(provider = "deepseek", key = "your-api-key")

# Custom endpoint (Ollama, proxies, Kimi, ...)
set_llm(provider = "openai",
        url = "http://localhost:11434/v1/chat/completions",
        key = "your-api-key",
        model = "your-model")
```

Check what is currently configured with:
```R
get_llm()
#> LLM config read from `~/.local/r/config/llmjoin/LLMJOIN.yml`.
#>   Provider: openai
#>   Model: gpt-6-luna
#>   URL: https://api.openai.com/v1/chat/completions
#>   Key: ****cdef
#>   (pass show_key = TRUE to print the full key)
```
### 2. use LLM-JOIN

> **Below examples used `deepseek-flash`.**

#### Example 1: Numbers ↔ Months matching

Match numeric month codes to month names — the LLM understands that "01" means January.

```R
x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

llm_join(x, y, key1 = "id", key2 = "month")
#     month id value amount
# 1     Feb 02    20    200
# 2 January 01    10    100
# 3    <NA> 04    40     NA
```

#### Example 2: Fuzzy number matching

Match approximate or differently-formatted numeric identifiers — the LLM handles rounding, unit conversion, and format differences.

```R
left <- data.frame(
  weight_kg = c(1.0, 2.5, 5.0),
  product   = c("Widget", "Gadget", "Thing")
)
right <- data.frame(
  weight_lb = c("2.2 lb", "5.5 lb", "11 lb"),
  price = c(4.99, 9.99, 19.99)
)

llm_join(left, right, key1 = "weight_kg", key2 = "weight_lb")
#   weight_lb weight_kg product price
# 1     11 lb       5.0   Thing 19.99
# 2    2.2 lb       1.0  Widget  4.99
# 3    5.5 lb       2.5  Gadget  9.99
```

#### Example 3: Country name ↔ code matching

Match country names to ISO codes — the LLM bridges different naming conventions, languages, and abbreviations.

```R
left <- data.frame(
  country = c("China", "United States", "Germany", "日本"),
  sales = c(1500, 3200, 2100, 800)
)
right <- data.frame(
  code = c("CN", "US", "DE", "JP"),
  region = c("Asia", "Americas", "Europe", "Asia")
)

llm_join(left, right, key1 = "country", key2 = "code")
#   code       country sales   region
# 1   CN         China  1500     Asia
# 2   DE       Germany  2100   Europe
# 3   JP          日本   800     Asia
# 4   US United States  3200 Americas
```

### Or if you don't want to setup LLM services in R
```R
x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

joint_prompt(unique(x["id"]), unique(y["month"]))
```
The prompt prints to the console — copy it into your LLM chat, and copy the answer back. Then in R:

```R
joint <- parse_joint(
  switch(Sys.info()[["sysname"]],
    Windows = paste(readClipboard(), collapse = "\n"),
    Darwin  = paste(system("pbpaste", intern = TRUE), collapse = "\n"),
    paste(readLines(stdin()), collapse = "\n")   # Linux fallback
  ),
  key1 = "id", key2 = "month"
)

# pass `by` explicitly: never rely on the intersection of same-named columns
merge(merge(x, joint, by = "id", all.x = TRUE), y, by = "month", all.x = TRUE)
#     month id value amount
# 1     Feb 02    20    200
# 2 January 01    10    100
# 3    <NA> 04    40     NA
```



## License
MIT License