
# llm-join: Fuzzy Dataframe Join in R with LLMs
## Introduction
llm-join is an R package designed to leverage Large Language Models (LLMs, such as GPT-4o, DeepSeek R1, etc.) for fuzzy joining of dataframes. When the key columns of two dataframes have spelling differences, are in different languages, or cannot be matched exactly, llm-join can automatically generate prompts and utilize LLMs to assist in high-quality joining.

## Installation
You can install the development version of llmjoin from [GitHub](https://github.com/) with:
```{r}
devtools::install_github("evanliu3594/llmjoin")
```

## Usage

1. setup your LLM services.
``` r
library(llmjoin)
set_llm(
  url = "url-to-your-llm-provider",
  key = "your-api-key"
)
```
2. use LLM-JOIN
```{r}
x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

result <- llm_join(x, y, key1 = "id", key2 = "month")
print(result)
```

## License
MIT License

> For further assistance or suggestions, feel free to submit an issue or contact the author.