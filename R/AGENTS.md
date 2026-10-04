# AGENTS.md — R/ 包实现层(唯一事实源)

> 本目录存放包的全部实现代码。总体约束(P0–P3)与管线概要见根目录 `AGENTS.md` §架构,本文不重复管线细节。

## 规则

- base R only:禁止 tidyverse / magrittr / readr;管道 `|>`,匿名函数 `\(x)`(根 P0.5)。
- 用户可见输出只用 `message()` / `warning()` / `stop()`,禁止 `cat()`(根 P0.1)。
- `stop()` 的错误信息必须包含"如何修复"的指引(根 P2.3)。
- `parse_joint()` 的 `x_keys` / `y_keys` 防伪造过滤是安全特性,不得移除或弱化(根 P0.3)。
- 函数命名 snake_case;内部辅助函数 `.` 前缀或 `@noRd`;roxygen 变更后跑 `devtools::document()`(根 P2.1)。
- 新增 provider = `.providers` 注册表加一项 + 四个内部函数各加一个 case(根 §架构 Provider 层)。

## 当前清单(261004)

| 文件 | 内容(一句话) | 状态 |
|---|---|---|
| `connection.R` | `set_llm()` 配置读写 + `chat_llm()` 唯一 LLM 调用入口 | 现行 |
| `providers.R` | provider 注册表(openai/claude/gemini)+ headers/body/parse/url 四函数 | 现行 |
| `llmjoin.R` | join 管线:tbl2md → joint_prompt → build_joint → parse_joint → llm_join | 现行 |
| `utils.R` | `%||%`、globalVariables、NAMESPACE imports | 现行 |

## 本目录特有要点

- base R `merge()` 把 NA 键当字符串互相匹配(R 4.6.1 实测,261004 发现):涉及 NA 键值的
  代码与测试不得依赖"NA 行不连接"的假设。
- `llm_join()` 第二段 merge 前须探测 joint 键列是否被 merge 加 `.x/.y` 后缀(x 已含名为
  `key2` 的列时会出现),按实际列名连接——移除该探测会复活"静默一行都匹配不上"的旧 bug。
