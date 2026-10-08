# AGENTS.md — R/ 包实现层

本目录是包的全部实现。总体约束（P0–P3）与管线说明见根 `AGENTS.md`，本文只列本目录特有的规则。

## 规则

1. 只用 base R：不引入 tidyverse、magrittr、readr；管道用 `|>`，匿名函数用 `\(x)`（根 P0.5）。
2. 用户可见输出只用 `message()`、`warning()`、`stop()`（根 P0.1）。
3. `stop()` 的错误信息给出修复方法（根 P2.3）。
4. `parse_joint()` 的 `x_keys` / `y_keys` 防伪造过滤保持原样，删除或放宽都不允许（根 P0.3）。
5. 函数名用 snake_case；内部辅助函数加 `.` 前缀或 `@noRd`；改 roxygen 注释后跑 `devtools::document()`（根 P2.1）。
6. 新增 provider 的做法：`.providers` 注册表加一项，四个内部函数各加一个 case（根 §架构）。

## 文件清单

| 文件 | 内容 |
|---|---|
| `connection.R` | `set_llm()` 写配置（含 `key` 入参护栏）；`get_llm()` 读配置（`key` 默认脱敏）；`.read_config()` 校验；`chat_llm()` 唯一的 LLM 调用入口 |
| `providers.R` | provider 注册表（openai、claude、gemini、deepseek）与 headers、body、parse、url 四个函数 |
| `llmjoin.R` | join 管线：`tbl2md()` → `joint_prompt()` → `build_joint()` → `parse_joint()` → `llm_join()` |
| `utils.R` | `%||%`、`globalVariables`、NAMESPACE imports |

## 本目录特有的事实

1. **数字段要数分隔符。** 数 CSV 字段用 `1 + 逗号数`，不要用 `lengths(strsplit())`。`strsplit("02,", ",")` 会丢掉尾随空字段，返回长度 1。`.count_fields()` 首版踩中，把提示词自己要求的 `02,`（无匹配）合法行判成畸形并删掉，被 3 条既有测试抓住（261008）。
2. **归一化恢复只在唯一命中原始键时进行。** `.recover_keys()` 只把大小写、首尾空白、数字形态这类等价写法映射回原值，不引入原表不存在的新值。命中多个原始键就按 ambiguous 丢弃并警告。相似度匹配不算命中（根 P0.3）。
3. **字符串字面量只用 ASCII。** `R CMD check --as-cran` 的 `checking code files for non-ASCII characters` 会把含 `—` 或中文的代码与 NAMESPACE 判成 WARNING；注释里的非 ASCII 允许（261008 实测：`R/llmjoin.R` 注释中的破折号历次 check 均 OK，写进 `stop()` 文案的破折号立刻报 WARNING）。需要非 ASCII 时用 `\uxxxx` 转义，或把面向用户的英文文案改成 ASCII。
4. **`merge()` 把 NA 键当字符串互相匹配。** R 4.6.1 实测（261004）。涉及 NA 键值的代码和测试不依赖"NA 行不连接"这一假设。
5. **白名单里的 NA 例外按条件放行。** 键集含真实 NA 时放行字面 `"NA"` 回显；键集无 NA 时仍按伪造丢弃（根 P0.3，audit #5，2026-10-05 修复）。
6. **`build_joint()` 的校验先于 LLM 调用。** `.validate_key()` 要求 x、y 为 data.frame，key 为单元素、非 NA 且存在于对应列名；错误信息点名参数并附修复方法。
7. **`llm_join()` 第二段 merge 前探测列名后缀。** x 已含名为 `key2` 的列时，merge 会给 joint 键列加 `.x` / `.y` 后缀，必须按实际列名连接。删掉该探测会复活"静默一行都匹配不上"的缺陷。
8. **`provider_body()` 的 openai 分支按模型名分流。** 模型名匹配 `^(o[0-9]|gpt-[5-9])`（GPT-5+/o 系）时发 `max_completion_tokens` 并省略 temperature，`.temperature` 非 0 时警告被忽略；其余模型维持 `max_tokens` 加 temperature。第三方兼容端点（Ollama、代理等）经 `provider = "openai"` 接入，整个分支保持原样。
9. **`provider_parse()` 先报空内容再报截断。** 截断警告（E1）：openai、gemini、deepseek 看 `finish_reason == "length"`，claude 看 `stop_reason == "max_tokens"`；reasoning 模型的空内容报错优先于截断警告。
10. **`.warn_unmatched()` 的计算放在两个白名单过滤块之后。** y 侧删行会改变 x 侧计数（E2）。x、y 各一条 warning，真实 NA 键不计入，只提示不删行。
