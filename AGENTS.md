# AGENTS.md — llmjoin 协作约定

本文件是 AI 助手(Agent)与维护者在本仓库协作的最高约定。原 旧协作说明文件 已并入本文件。
约束按效力分四级,冲突时以高级别为准:**P0 > P1 > P2 > P3**。

| 级别 | 含义 | 违反后果 |
|------|------|----------|
| P0 | 硬约束(MUST),无条件遵守 | 变更必须回滚或拒绝 |
| P1 | 流程约束(MUST),实现类任务的强制流程 | 任务视为未完成,不得交付 |
| P2 | 指导约定(SHOULD),默认遵守 | 允许偏离,但须说明理由 |
| P3 | 风格偏好(MAY),参考执行 | 自由裁量 |

## P0 硬约束

1. **CRAN 合规**
   - 不向用户主目录写任何文件;配置只写 `tools::R_user_dir("llmjoin", "config")`。
   - 新增依赖必须证明必要性并评估传递依赖成本;本包卖点是依赖极简(Imports: httr、jsonlite、config)。
   - 用户可见输出用 `message()` / `warning()` / `stop()`,禁止 `cat()`。
   - 严禁提交 API key、密钥或真实配置文件。
2. **测试红线**:交付前 `testthat::test_local()` 必须全绿(当前基线:78 个测试,0 失败);测试不得依赖真实 LLM 服务,一律用 `local_mocked_bindings()` 打桩。
3. **防伪造校验是安全特性**:`parse_joint()` 的 `x_keys` / `y_keys` 白名单过滤不得移除或弱化;`build_joint()` / `llm_join()` 必须默认传键值集合。
4. **API 兼容**:导出函数的签名或语义变更必须记入 NEWS.md 当前版本段,并说明迁移方式。
5. **base R 优先**:禁止引入 tidyverse / magrittr / readr;管道用 `|>`,匿名函数用 `\(x)`,字符串处理优先 base 函数。

## P1 流程约束(实现类任务)

本仓库遵循 BDD/TDD:

1. 先与用户确认 Given-When-Then 场景矩阵(正常 / 异常 / 边界)。
2. 生成测试文件骨架:导入、夹具、mock 桩齐全,测试体留 G-W-T 注释。
3. 场景确认后,按测试函数拆分为独立子任务。
4. 子代理填写测试与实现,自验通过后跑全量回归。
5. 审核标准:全部测试通过 + G-W-T 全覆盖 + 无副作用;不合格则修复或拒绝。
6. 每轮并发子代理不超过 4 个。

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

## 架构(2026-10-04 与 v0.3.1 代码同步)

- **配置层** `R/connection.R`:`set_llm()` 写 YAML 配置(单引号转义为 `''`)到
  `tools::R_user_dir("llmjoin", "config")/LLMJOIN.yml`;`chat_llm(.message, .model,
  .temperature, .max_tokens, .timeout, .verbose)` 是唯一 LLM 调用入口,每次调用读取并校验配置
  (URL/key/provider 必填,无缓存验证)。默认 `.max_tokens = 30000`、`.timeout = 300`、
  `.temperature = 0`(越界自动截断并警告)。thinking / reasoning 模式已在 0.2.2 移除,不再支持。
- **Provider 层** `R/providers.R`:`.providers` 注册表(openai / claude / gemini)→
  base_url、endpoint、default_model、auth_type。四个内部函数:
  `provider_headers()`(openai/gemini 走 Bearer;claude 走 `x-api-key` + `anthropic-version`)、
  `provider_body()`(请求体)、`provider_parse()`(响应解析;claude 过滤 thinking block 取 text)、
  `provider_url()`。新增 provider = 注册表加一项 + 四个函数各加一个 case。
- **Join 层** `R/llmjoin.R`:
  `tbl2md()`(data.frame/向量 → markdown 表)→
  `joint_prompt()`(两键列 → 匹配提示词)→
  `build_joint(x, y, key1, key2, ...)`(造 prompt → `chat_llm()` → `parse_joint()`,自动传
  `x_keys`/`y_keys`)→
  `parse_joint(llm_response, key1, key2, x_keys, y_keys)`(剥 markdown fence → rle 取最长含逗号
  行块 → 表头探测/补齐 → `utils::read.csv` → 防伪造过滤)→
  `llm_join()`(build_joint + 两次 `merge()`)。
- **工具** `R/utils.R`:`%||%`、`globalVariables`、NAMESPACE imports(httr/jsonlite)。
- **测试** `tests/testthat/`:目前仅 `test-parse_joint.R`;mock 模式
  `local_mocked_bindings(chat_llm = function(...) "...", .package = "llmjoin")`。

### 已知问题(待修复,详见 handoff/)

1. 【高】`llm_join()` 两次 `merge()` 未显式指定 `by`,按同名列交集连接;x、y 存在同名非键列
   或 x 已含名为 `key2` 的列时会静默错连。应改为 `by = key1` / `by = key2`。
2. 【中】`parse_joint()` 表头探测不识别带引号(`"id","month"`)与通用表头,垃圾行混入结果。
3. 【中】`tbl2md()` 对 factor 向量静默输出空表(`is.vector(factor)` 为 FALSE)。
4. 【中】`provider_parse()` claude 分支只取第一个 text block,长回复会被静默截断。

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

**每轮任务结束后,必须在 `handoff/` 目录新增一条处理结果记录**(不改写、不删除历史记录),
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
