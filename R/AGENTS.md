# AGENTS.md — R/ 包实现层(唯一事实源)

> 本目录存放包的全部实现代码。总体约束(P0–P3)与管线概要见根目录 `AGENTS.md` §架构,本文不重复管线细节。

## 规则

- base R only:禁止 tidyverse / magrittr / readr;管道 `|>`,匿名函数 `\(x)`(根 P0.5)。
- 用户可见输出只用 `message()` / `warning()` / `stop()`,禁止 `cat()`(根 P0.1)。
- `stop()` 的错误信息必须包含"如何修复"的指引(根 P2.3)。
- `parse_joint()` 的 `x_keys` / `y_keys` 防伪造过滤是安全特性,不得移除或弱化(根 P0.3)。
- 函数命名 snake_case;内部辅助函数 `.` 前缀或 `@noRd`;roxygen 变更后跑 `devtools::document()`(根 P2.1)。
- 新增 provider = `.providers` 注册表加一项 + 四个内部函数各加一个 case(根 §架构 Provider 层)。

## 当前清单(261006)

| 文件 | 内容(一句话) | 状态 |
|---|---|---|
| `connection.R` | `set_llm()` 写配置(key 入参护栏) / `get_llm()` 读配置(key 默认脱敏) / 私有 `.read_config()` 校验 / `chat_llm()` 唯一 LLM 调用入口(`.message` 强转、经 `.read_config()` 取明文 key) | 现行 |
| `providers.R` | provider 注册表(openai/claude/gemini/deepseek)+ headers/body/parse/url 四函数 | 现行 |
| `llmjoin.R` | join 管线:tbl2md → joint_prompt → build_joint → parse_joint → llm_join | 现行 |
| `utils.R` | `%||%`、globalVariables、NAMESPACE imports | 现行 |

## 本目录特有要点

- **R/ 里的字符串字面量必须纯 ASCII**:`R CMD check --as-cran` 的
  `checking code files for non-ASCII characters` 会把含 `—`/中文等字符的**代码/NAMESPACE**
  判成 WARNING(portable packages 要求);**注释里的非 ASCII 是允许的**(261008 实测:
  `R/llmjoin.R` 注释里的破折号历次 check 均 OK,而新写进 `stop()` 文案的破折号立刻报 WARNING)。
  要非 ASCII 就用 `\uxxxx` 转义,或者把面向用户的英文文案改成 ASCII。

- base R `merge()` 把 NA 键当字符串互相匹配(R 4.6.1 实测,261004 发现):涉及 NA 键值的
  代码与测试不得依赖"NA 行不连接"的假设。
- `parse_joint()` 白名单:键集含真实 NA 时放行字面 "NA" 回显(audit #5,2026-10-05 修复);
  不得无条件放行——键集无 NA 且无字面 "NA" 时仍按伪造丢弃(根 P0.3)。
- `build_joint()` 入口校验(`.validate_key()`):x/y 须为 data.frame,key 须为单元素非 NA
  字符串且存在于对应列名;校验先于 LLM 调用,错误信息点名参数并附修复指引。
- `llm_join()` 第二段 merge 前须探测 joint 键列是否被 merge 加 `.x/.y` 后缀(x 已含名为
  `key2` 的列时会出现),按实际列名连接——移除该探测会复活"静默一行都匹配不上"的旧 bug。
- `provider_body()` openai 分支:模型名匹配 `^(o[0-9]|gpt-[5-9])`(GPT-5+/o 系)发
  `max_completion_tokens`、省略 temperature(`.temperature` 非 0 时警告被忽略);其余
  模型维持 `max_tokens`+temperature——第三方兼容端点(Ollama、代理等)经
  provider="openai" 接入,不得整体切换。
- `provider_parse()` 对截断回复警告(E1):openai/gemini/deepseek 看
  `finish_reason=="length"`,claude 看 `stop_reason=="max_tokens"`;reasoning 模型
  空内容报错优先于截断警告。
- `parse_joint()` 末尾 `.warn_unmatched()` 提示未匹配键(E2):x/y 双侧各一条 warning,
  真实 NA 键不计入,只提示不删行;计算必须放在两个白名单过滤块之后(y 侧删行会
  改变 x 侧计数)。
