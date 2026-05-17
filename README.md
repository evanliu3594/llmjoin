
# llm-join: Fuzzy Dataframe Join in R with LLMs
## Introduction
llm-join is an R package designed to use Large Language Models (LLMs, such as GPT-5, Claude, DeepSeek, etc.) for fuzzy joining of dataframes. When the key columns of two dataframes have spelling differences, are in different languages, or cannot be matched exactly, llm-join can automatically generate prompts and utilize LLMs to assist in high-quality joining.

## Installation
You can install the development version of llmjoin from [GitHub](https://github.com/) with:
```R
devtools::install_github("evanliu3594/llmjoin")
```

## Usage

### 1. setup your LLM services.

> Please note that all information is stored strictly locally in your system configuration (run `tools::R_user_dir("llmjoin", "config")` to see the full path), and is never uploaded or shared.
```R
library(llmjoin)

# OpenAI
set_llm(provider = "openai", key = "your-api-key")

# Claude (Anthropic)
set_llm(provider = "claude", key = "your-api-key")

# Gemini (via OpenAI-compatible endpoint)
set_llm(provider = "gemini", key = "your-api-key")

# Custom endpoint (Ollama, DeepSeek, Kimi, Grok, Qwen, etc.)
set_llm(provider = "openai",
        url = "https://your-custom-endpoint/v1/chat/completions",
        key = "your-api-key",
        model = "your-model-name")
```
### 2. use LLM-JOIN

> **Below examples used Deepseek-V4-Flash.**

### Example 1: Numbers ↔ Months matching

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

joint_prompt(unique(x["id"]), unique(y["month"])) |> writeClipboard()
```
Paste the prompts to ask your LLM model, and copy the answer, going back to R and continue run:

```R
joint <- parse_joint(readr::clipboard(), key1 = "id", key2 = "month")

Reduce(\(x, y) merge(x, y, all.x = TRUE), list(x, joint, y))
#     month id value amount
# 1     Feb 02    20    200
# 2 January 01    10    100
# 3    <NA> 04    40     NA
```



## License
MIT License