
# llm-join: Fuzzy Dataframe Join in R with LLMs
## Introduction
llm-join is an R package designed to leverage Large Language Models (LLMs, such as GPT-4o, DeepSeek R1, etc.) for fuzzy joining of dataframes. When the key columns of two dataframes have spelling differences, are in different languages, or cannot be matched exactly, llm-join can automatically generate prompts and utilize LLMs to assist in high-quality joining.

## Installation
You can install the development version of llmjoin from [GitHub](https://github.com/) with:
```R
devtools::install_github("evanliu3594/llmjoin")
```

## Usage

1. setup your LLM services.
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
2. use LLM-JOIN
```R
x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

result <- llm_join(x, y, key1 = "id", key2 = "month")
print(result)
```

### Or if you don't want to set LLM services in R
```R
x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

joint_prompt(unique(x["id"]), unique(y["month"])) %>% clipr::write_clip()
```
Paste the prompts to ask your LLM model, and copy the answer.

```R
joint <- read_csv(clipboard())

Reduce(\(x, y) left_join(x, y), list(x, joint, y))
```



## License
MIT License

> For further assistance or suggestions, feel free to submit an issue or contact the author.