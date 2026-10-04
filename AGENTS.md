# AGENTS.md — llmjoin 协作约定

本文件是 AI 助手(Agent)与维护者在本仓库协作的最高约定。
约束按效力分四级,冲突时以高级别为准:**P0 > P1 > P2 > P3**。

| 级别 | 含义 | 违反后果 |
|------|------|----------|
| P0 | 硬约束(MUST),无条件遵守 | 变更必须回滚或拒绝 |
| P1 | 流程约束(MUST),实现类任务的强制流程 | 任务视为未完成,不得交付 |
| P2 | 指导约定(SHOULD),默认遵守 | 允许偏离,但须说明理由 |
| P3 | 风格偏好(MAY),参考执行 | 自由裁量 |

## 项目定位与当前状态

llmjoin:用 LLM 做数据框模糊连接(拼写变体、跨语言、精度差异),卖点是**依赖极简**
(Imports: httr、jsonlite、config)。当前 0.3.1,待重提 CRAN(CRAN-SUBMISSION 最后记录
0.3.0,2026-06-08)。测试红线见 P0.2。

## 目录结构(要点;R/ 与 tests/ 明细见各自 AGENTS.md)

| 目录 / 文件 | 职责 | 明细位置 |
|---|---|---|
| `R/` | 包实现(唯一事实源) | `R/AGENTS.md` |
| `tests/` | testthat 测试 | `tests/AGENTS.md` |
| `man/` | roxygen 生成文档,不手改 | R/ 源文件内嵌 roxygen 注释 |
| `handoff/` | 交接记录,兼任项目时间线 | 根 §交接记录约定 |
| `DESCRIPTION` / `NAMESPACE` / `NEWS.md` / `README.md` | 包元数据 / 导出表 / 用户可见变更 / 使用说明 | — |

## 已拍板口径(用户确认,勿再改)

| # | 口径 | 决定 | 日期 |
|---|---|---|---|
| ① | 时间线载体 | `handoff/` 兼任项目时间线,**不建 HISTORY.md**(偏离 project-conventions 框架的单一时间线设计;理由:轮次交接记录已承载留痕职能,避免双写) | 261004 |
| ② | 目录级文档 | 仅建 `R/`、`tests/` 两份 AGENTS.md;其余目录不建(man/ 由工具生成,NEWS.md 承载用户可见变更) | 261004 |
| ③ | 开发辅助工具 | 提交信息、用户可见文档、交接记录**不点名开发辅助工具**;provider 功能语境除外(接入新 LLM 服务时如实描述) | 261004 |
| ④ | 版本管理 | AGENTS 体系(根 + `R/`、`tests/`)与 `handoff/` **纳入 git 追踪**;`.Rbuildignore` 维持排除,不进 R CMD 构建(261005 维护者拍板;历史记录的措辞中性化以维护者 261004 就地修订为先例) | 261005 |
| ⑤ | 历史重写 | 261005 已对全历史执行痕迹清除重写并 force-push:b9b1c4b 起的提交 SHA 均已改变(映射见 handoff/[DONE]-全量清除开发助手痕迹-261005.md),旧记录中的 SHA 引用以该映射为准;归档于仓库外 bundle 与本地 `backup/` 分支,永不推送 | 261005 |

## P0 硬约束

1. **CRAN 合规**
   - 不向用户主目录写任何文件;配置只写 `tools::R_user_dir("llmjoin", "config")`。
   - 新增依赖必须证明必要性并评估传递依赖成本;本包卖点是依赖极简(Imports: httr、jsonlite、config)。
   - 用户可见输出用 `message()` / `warning()` / `stop()`,禁止 `cat()`。
   - 严禁提交 API key、密钥或真实配置文件。
2. **测试红线**:交付前 `testthat::test_local()` 必须全绿(当前基线:121 项断言,0 失败);测试不得依赖真实 LLM 服务,一律用 `local_mocked_bindings()` 打桩。
3. **防伪造校验是安全特性**:`parse_joint()` 的 `x_keys` / `y_keys` 白名单过滤不得移除或弱化;`build_joint()` / `llm_join()` 必须默认传键值集合。
4. **API 兼容**:导出函数的签名或语义变更必须记入 NEWS.md 当前版本段,并说明迁移方式。
5. **base R 优先**:禁止引入 tidyverse / magrittr / readr;管道用 `|>`,匿名函数用 `\(x)`,字符串处理优先 base 函数。

## P1 流程约束(实现类任务)

本仓库遵循 BDD/TDD:

0. 口径不明、存在多个可选方案且取舍影响结果、或需改动"已拍板口径"时,先问后做
   (选项 + 影响 + 推荐,一次问清);纯机械改动不问,但执行前后须声明预期并核对。
1. 先与用户确认 Given-When-Then 场景矩阵(正常 / 异常 / 边界)。
2. 生成测试文件骨架:导入、夹具、mock 桩齐全,测试体留 G-W-T 注释。
3. 场景确认后,按测试函数拆分为独立子任务。
4. 子代理填写测试与实现,自验通过后跑全量回归。
5. 审核标准:全部测试通过 + G-W-T 全覆盖 + 无副作用;不合格则修复或拒绝。
6. 每轮并发子代理不超过 4 个;子代理无活动超时连续两次即由编排者按已商定方案接手,
   不再重试(261004 曾发生:同一任务两个代理先后 600 秒零产出超时)。

## P2 指导约定

1. roxygen 注释变更后运行 `devtools::document()`;`man/` 由工具生成,不手改。
2. 每个用户可见变更(修复 / 新特性 / 破坏性)在 NEWS.md 对应版本段记录。
3. `stop()` 的错误信息必须包含"如何修复"的指引(如 `Use set_llm() to reconfigure.`)。
4. 代码行为与本文件"架构"一节发生漂移时,随该次变更一并更新本文件。
5. 疑难结论、审查结果、不可复现的问题 → 写入 handoff/(见下节约定)。

## P3 风格偏好

- 函数命名 snake_case;内部辅助函数加 `.` 前缀或 `@noRd`。
- 错误 / 警告信息用英文(与 CRAN 包惯例一致),面向开发者的文档用中文。
- 提交信息用英文祈使句,前缀 `feat:` / `fix:` / `docs:` / `chore:`。
- 交接记录与用户沟通:直白朴素、先结论后证据,证据带可核对数值;未验证的结论注明"未验证"。

## 架构(2026-10-04 与 v0.3.1 代码同步)

- **配置层** `R/connection.R`:`set_llm()` 写 YAML 配置(单引号转义为 `''`)到
  `tools::R_user_dir("llmjoin", "config")/LLMJOIN.yml`;`chat_llm(.message, .model,
  .temperature, .max_tokens, .timeout, .verbose)` 是唯一 LLM 调用入口,每次调用读取并校验配置
  (URL/key/provider 必填,无缓存验证)。默认 `.max_tokens = 30000`、`.timeout = 300`、
  `.temperature = 0`(越界自动截断并警告)。thinking / reasoning 模式已在 0.2.2 移除,不再支持。
- **Provider 层** `R/providers.R`:`.providers` 注册表(openai / claude / gemini)→
  base_url、endpoint、default_model、auth_type。四个内部函数:
  `provider_headers()`(openai/gemini 走 Bearer;claude 走 `x-api-key` + `anthropic-version`)、
  `provider_body()`(请求体)、`provider_parse()`(响应解析;claude 过滤 thinking block,
  按序拼接全部 text block)、
  `provider_url()`。新增 provider = 注册表加一项 + 四个函数各加一个 case。
- **Join 层** `R/llmjoin.R`:
  `tbl2md()`(data.frame/向量 → markdown 表)→
  `joint_prompt()`(两键列 → 匹配提示词)→
  `build_joint(x, y, key1, key2, ...)`(造 prompt → `chat_llm()` → `parse_joint()`,自动传
  `x_keys`/`y_keys`)→
  `parse_joint(llm_response, key1, key2, x_keys, y_keys)`(剥 markdown fence → rle 取最长含逗号
  行块 → 表头探测/补齐 → `utils::read.csv` → 防伪造过滤)→
  `llm_join()`(build_joint + 两次显式 `merge()`;x 已含名为 `key2` 的列时靠 merge 后缀
  探测定位 joint 键列)。
- **工具** `R/utils.R`:`%||%`、`globalVariables`、NAMESPACE imports(httr/jsonlite)。
- **测试** `tests/testthat/`:`test-parse_joint.R`、`test-llm_join.R`、`test-tbl2md.R`、
  `test-providers.R`;mock 模式
  `local_mocked_bindings(chat_llm = function(...) "...", .package = "llmjoin")`。

### 已知问题(待修复,详见 handoff/)

2026-10-04 审核的 1-4 项(llm_join 合并键、parse_joint 表头探测、tbl2md factor、
claude 多 text block)已在 0.3.1 修复。剩余待办:

1. 【中低】README `writeClipboard()` 仅 Windows 存在,macOS/Linux 报错,与跨平台承诺不符。
2. 【低】x 键列含 NA 时,LLM 回显的字符串 `"NA"` 被防伪造校验当伪造值丢弃。
3. 【低】`chat_llm()` 收到长度>1 的向量消息时,错误信息不指向 `.message` 参数。
4. 【低】key1/key2 拼错时报 `undefined columns selected`,不指名参数;建议入口显式校验。
5. 【需实测】OpenAI gpt-5.4-mini 的 `max_tokens`/`temperature` 请求体可能被官方 API 拒绝
   (GPT-5/o 系要求 `max_completion_tokens`、拒 temperature),需真实 API 验证。
6. 【可选增强】`provider_parse()` 检查 `finish_reason=="length"` 截断并警告;
   `parse_joint()` 末尾对未匹配的 x_keys 提示"N 个键未匹配"。

## 常用命令

```bash
Rscript -e "testthat::test_local('.')"          # 全量测试(从源码加载,勿用 library(llmjoin))
Rscript -e 'devtools::test_active_file("tests/testthat/test-parse_joint.R")'
Rscript -e 'devtools::load_all(".")'            # 交互开发加载
Rscript -e 'devtools::check()'                  # 完整 R CMD check
Rscript -e 'devtools::document()'               # 生成 man/ 文档
Rscript -e 'devtools::install()'                # 本地安装(保持与源码同步)
```

## 交接记录约定(handoff/)

**每轮任务结束后,必须在 `handoff/` 目录新增一条处理结果记录**(不改写、不删除历史记录;
`handoff/` 兼任项目时间线,不另建 HISTORY.md,见 §已拍板口径①),
文件名格式:

```
[<任务状态>]-<任务内容>-<yymmdd>.md
```

- `<任务状态>` 固定词表(四选一):`DONE` 已完成 / `WIP` 进行中 / `BLOCKED` 受阻待外部输入 /
  `REVIEW` 已完成待复核。
- `<任务内容>`:简短中文短语,不含空格,词间用连字符。
- `<yymmdd>`:六位日期,如 `261004`。
- 示例:`[DONE]-修复llm_join合并键-261011.md`。

记录内容按以下模板填写(可作为交接给下一轮的最小上下文):

```markdown
# [<任务状态>] <任务内容> (<yymmdd>)

## 背景
为什么做这件事。

## 做了什么
关键动作与决定(含取舍理由)。

## 变更文件
- path/to/file:改动摘要

## 验证
验证方式与结果(测试数、失败数等)。

## 遗留与下一步
未完成项、已知问题、给下一轮的提示。
```
