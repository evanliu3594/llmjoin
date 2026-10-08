# AGENTS.md — llmjoin 协作约定

约束效力：P0 > P1 > P2 > P3。冲突时取高级别。

| 级别 | 要求 | 违反后果 |
|------|------|----------|
| P0 | 硬约束，无条件遵守 | 变更回滚或拒绝 |
| P1 | 实现类任务的强制流程 | 任务视为未完成，不得交付 |
| P2 | 指导约定 | 可偏离，须说明理由 |
| P3 | 风格偏好 | 自由裁量 |

## 项目定位与当前状态

llmjoin 用 LLM 做数据框模糊连接：拼写变体、跨语言、精度差异。卖点是依赖极简，Imports 只有 httr、jsonlite、config。

版本 0.3.2，三条状态事实：

- tag `v0.3.2` 已推送；GitHub release 待维护者创建。
- CRAN 未提交；`CRAN-SUBMISSION` 停在 0.3.0。
- 本机 `devtools::check(cran = TRUE)` 曾为 `Status: OK`（0 error / 0 warning / 0 note）。投稿前按 §已知问题 3 重写 `cran-comments.md`。

测试红线见 P0.2。

## 目录结构

| 路径 | 职责 | 细则 |
|---|---|---|
| `R/` | 包实现，唯一事实源 | `R/AGENTS.md` |
| `tests/` | testthat 测试 | `tests/AGENTS.md` |
| `man/` | roxygen 生成的文档 | 由工具生成 |
| `handoff/` | 交接记录，兼任项目时间线 | 本文 §交接记录约定 |
| `DESCRIPTION` / `NAMESPACE` / `NEWS.md` / `README.md` | 元数据 / 导出表 / 用户可见变更 / 使用说明 | — |

## 已拍板口径（用户确认，勿再改）

| # | 主题 | 决定 | 理由 | 日期 |
|---|---|---|---|---|
| ① | 时间线 | `handoff/` 兼任项目时间线，不建 `HISTORY.md` | 一条时间线只留一个载体 | 261007 |
| ② | 目录级文档 | 只给 `R/`、`tests/` 建 AGENTS.md，其余目录不建。计划、场景矩阵、审查记录一律进 `handoff/`，不开新顶层目录 | 顶层目录只放项目内容 | 261004（261008 重申） |
| ③ | 工具痕迹 | 提交信息、用户文档、交接记录不点名开发辅助工具。接入新 LLM 服务的 provider 语境除外 | 协作者身份与产品无关 | 261004 |
| ④ | 版本管理 | AGENTS 体系（根 + `R/`、`tests/`）与 `handoff/` 纳入 git 追踪；`.Rbuildignore` 维持排除 | 协作可见，构建不带 | 261005 |
| ⑤ | 历史重写 | 261005 重写全历史并 force-push。`b9b1c4b` 起的 SHA 已变，映射见 `handoff/261005_全量清除开发助手痕迹.md`。归档 bundle 与本地 `backup/` 分支只留本地，不推送 | 旧记录里的 SHA 引用一律按映射读 | 261005 |
| ⑥ | claude 字样 | provider 注册表与用户文档保留 claude 配置入口 | 属 provider 功能语境，走口径③ 除外条款 | 261007 |
| ⑦ | 免费托管端点 | 不提供包内默认可用的托管转发服务端：不建 `server/`、不持有上游 key、不承诺第三方额度。新用户路径改为 README 指向服务商 key 申请页；需凭据的示例用 `@examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))` 守卫 | 托管端点等于承担域名续费、证书续期、刷屏处置、宕机值守的无限期责任；包内硬编码 URL 发布后几乎不可更改 | 261008 |
| ⑧ | 凭据显示与输入 | `get_llm()` 默认脱敏 key：空串显 `<empty>`，长度 ≤ 8 显 `****`，更长显 `****` 加末 4 位。返回值与打印内容一致，取明文只能显式 `show_key = TRUE`。`chat_llm()` 请求路径始终用明文 key，该行为由测试固定。文档与示例教用户用 `Sys.getenv("LLMJOIN_API_KEY")`。三项收口决定：不加 `ask_key()`；保留 `set_llm(key = )` 参数；配置文件不做 `Sys.chmod(0600)` | 控制台输出会进 `.Rhistory`、截屏、CI 日志，脱敏是安全特性，不得弱化。包管不住用户在控制台敲的字面量，只能改获取渠道。Windows R 4.6.1 的 base 与 utils 都没有 `getPasswd`，`readline()` 不遮蔽；删参数是破坏性变更，`Sys.getenv()` 已覆盖其收益；Windows 的 chmod 只切只读位 | 261008 |
| ⑨⑩⑪ | 工具与编辑器产物 | 外部产物既不入库也不进 tar：`.zcodeignore`、`.wrangler/`、`.positron/`、`.vscode/` 同时写进 `.gitignore` 与 `.Rbuildignore`。`R CMD build` 默认只排除 `.Rproj.user`、`.RData`、`.DS_Store`、`.Rhistory`、`.gitignore`，所以 `^.*\.Rproj$` 一行保留。引用这些条目时写模式名，不写行号或序号 | 未被排除的目录会随 tar 对外发布；行号随清单增删漂移 | 261008 |

## P0 硬约束

1. **CRAN 合规**
   - 配置文件只写 `tools::R_user_dir("llmjoin", "config")`；不向用户主目录写任何其他文件。
   - 新增依赖前先证明必要性，并评估传递依赖成本。
   - 用户可见输出只用 `message()`、`warning()`、`stop()`；`cat()` 不出现在包代码里。
   - API key、密钥、真实配置文件不进提交。
   - 包不把明文凭据推到任何用户可见渠道：`message()` / `warning()` / `stop()` 的文案只含 provider、model、URL、配置路径，不含 `LLMJOIN_key` 的值。该约束由 `tests/testthat/test-no_key_leak.R` 固定，文件内含一条"检测器认得出明文"的正向对照，防止负向断言空过。
2. **测试红线**：交付前 `testthat::test_local()` 全绿。当前基线 416 项断言、0 失败。测试不调用真实 LLM 服务，一律用 `local_mocked_bindings()` 打桩。
3. **防伪造校验是安全特性**：`parse_joint()` 的 `x_keys` / `y_keys` 白名单过滤保持原样，删除或放宽都不允许。`build_joint()` 与 `llm_join()` 默认传入键值集合。归一化恢复只在唯一命中原始键时进行，相似度匹配不算命中。
4. **API 兼容**：导出函数的签名或语义有变，就写进 NEWS.md 当前版本段，并给出迁移方法。
5. **base R 优先**：不引入 tidyverse、magrittr、readr。管道用 `|>`，匿名函数用 `\(x)`，字符串处理优先用 base 函数。
6. **文档风格（覆盖本文与全部 `handoff/`）**：一律用简明技术报告写法——一句一事；陈述句 ≤ 50 字、步骤句 ≤ 30 字；步骤用祈使句；主动语态并写出执行者；条件写成整句；数字、单位、时间具体；事实、判断、建议分列，不确定写"未确认"；结论与风险前置；同一事物全文只用一个名称；删去"进行/加以/予以/作出"类空动词。本文只保留六节：项目定位与当前状态、目录结构、已拍板口径、纪律约束（P0–P3 与 §架构 的不得改动项）、已知问题（只列未决项）、常用命令与交接约定。过程叙述、逐时补记、取证细节、已解决事项一律进 `handoff/`，本文只留一行指针。一份收口记录目标 ≤ 150 行。

## P1 流程约束（实现类任务）

仓库用 BDD/TDD。

0. 三种情形先问后做：口径不明、多个方案且取舍影响结果、需要改动已拍板口径。一次问清，给选项 + 影响 + 推荐。纯机械改动不用问，但执行前后要声明预期并核对结果。
1. 与维护者确认 Given-When-Then 场景矩阵，覆盖正常、异常、边界。
2. 生成测试文件骨架：导入、夹具、mock 桩齐全，测试体留 G-W-T 注释。
3. 场景确认后，按测试函数拆成独立子任务。
4. 子代理填测试与实现，自验通过后跑全量回归。
5. 按三条标准审核：测试全绿、G-W-T 全覆盖、无副作用。不合格就修复或拒绝。
6. 每轮并发子代理不超过 4 个。子代理连续两次无活动超时，就由编排者按已商定方案接手，不再重试。

## P2 指导约定

1. 改 roxygen 注释后运行 `devtools::document()`。
2. 每个用户可见变更（修复、新特性、破坏性变更）写进 NEWS.md 对应版本段。
3. `stop()` 的错误信息给出修复方法，例如 `Use set_llm() to reconfigure.`。
4. 代码行为与本文 §架构 不一致时，随该次变更一起更新本文。
5. 疑难结论、审查结果、不可复现的问题写进 `handoff/`。

## P3 风格偏好

- 函数名用 snake_case。内部辅助函数加 `.` 前缀或 `@noRd`。
- 错误与警告信息用英文（CRAN 惯例）。面向开发者的文档用中文。
- 提交信息用英文祈使句，前缀 `feat:`、`fix:`、`docs:`、`chore:`。
- 汇报先结论后证据，证据带可核对数值。未验证的结论写明"未验证"。

## 架构（2026-10-08 与 0.3.2 代码同步）

**配置层 `R/connection.R`**

- `set_llm()`：把 YAML 写入 `tools::R_user_dir("llmjoin", "config")/LLMJOIN.yml`，单引号转义为 `''`。`key` 参数护栏把 missing、NULL、空串、零长、长度 > 1、NA、非字符统一成一条错误，错误信息点名参数并给检查方法。校验顺序：provider → key → url → model。
- `get_llm(show_key = FALSE)`：读同一份配置，用 `message()` 报 provider、model、URL、配置文件路径，返回不可见 list（`provider` / `url` / `model` / `key` / `config_path`），其中 `key` 已按口径⑧ 脱敏。
- `chat_llm(.message, .model, .temperature, .max_tokens, .timeout, .verbose)`：唯一的 LLM 入口。私有 `.read_config()` 按四类顺序校验：文件缺失 → YAML 非法 → 缺 URL/key → provider 未知，顺序与文案不变。随后单独拦空串 key：`LLM_key: ''` 能过 `is.null`，发出去只会拿到鉴权错误；拦在这里，`get_llm()` 才能显示 `<empty>` 而不报错。请求路径始终用明文 key。`.message` 接受可强转输入：元素 `as.character`、NA 置空、向量按换行拼成单串；missing、NULL、零长、全空白点名报错。默认 `.max_tokens = 30000`、`.timeout = 300`、`.temperature = 0`，越界截断并警告。openai GPT-5+/o 系不发 temperature，`.temperature` 非 0 时警告被忽略。thinking / reasoning 输出模式已在 0.2.2 移除。

**Provider 层 `R/providers.R`**

- `.providers` 注册表（openai、claude、gemini、deepseek）给出 base_url、endpoint、default_model、auth_type。
- `provider_headers()`：claude 用 `x-api-key` 加 `anthropic-version`，其余用 Bearer。
- `provider_body()`：模型名匹配 `^(o[0-9]|gpt-[5-9])` 的 openai 系发 `max_completion_tokens` 并省略 temperature，其余模型与其余 provider 发 `max_tokens` 加 temperature。第三方兼容端点经 `provider = "openai"` 接入，整个分支保持原样。
- `provider_parse()`：claude 过滤 thinking block，按序拼接全部 text block。截断警告：openai、gemini、deepseek 看 `finish_reason == "length"`，claude 看 `stop_reason == "max_tokens"`。
- `provider_url()`：组装请求地址。
- 新增 provider 的做法：注册表加一项，四个函数各加一个 case。

**Join 层 `R/llmjoin.R`**

- `tbl2md()`：data.frame 或向量转 markdown 表。
- `joint_prompt()`：造匹配提示词，`x`、`y` 允许 data.frame 或向量。三条承诺被 `parse_joint()` 依赖：纯 CSV 行（不用 markdown 管道表、不用制表符）、每个列 1 值恰好一行并写明期望行数、值逐字复制含前导零。
- `build_joint(x, y, key1, key2, ...)`：先用 `.validate_key()` 校验 x、y 为 data.frame 且键名存在于列名，再走 prompt → `chat_llm()` → `parse_joint()`，自动传 `x_keys` / `y_keys`。
- `parse_joint(llm_response, key1, key2, x_keys, y_keys)`：依次执行——剥 fence；`rle` 取最长含逗号行块；表头探测与补齐；`.count_fields()` 校验每行 2 字段（不合规的行点名行号并忽略，全部不合规就报 `does not look like a two-column mapping table`）；`utils::read.csv`；去完全重复行并报 message；`.filter_key_column()` 防伪造过滤，过滤前先经 `.canon_key()` / `.recover_keys()` 做唯一命中归一化（大小写、首尾空白、数字形态如 `1`→`01`；缩写展开 `Feb`→`February` 不在恢复范围，命中不了仍按伪造丢弃）；一对多只提示不删行；`.warn_unmatched()` 给 x、y 各一条 warning，真实 NA 不计，返回行数少于键值数时追加"疑似截断，提高 `.max_tokens` 重试"。
- `llm_join()`：`build_joint()` 加两次显式 `merge()`。x 已含名为 `key2` 的列时，靠 merge 后缀探测 joint 键列。

**其他**

- `R/utils.R`：`%||%`、`globalVariables`、NAMESPACE imports。
- `tests/testthat/`：10 个文件，清单与 mock 写法见 `tests/AGENTS.md`。

### 已知问题（只列未决项）

已修复与已关闭的事项不进本表，按时间序见 `handoff/`。

| # | 状态 | 事项 | 下一步 |
|---|---|---|---|
| 1 | 长期未验证 | openai `gpt-6-luna`、gemini `gemini-3.8-flash`、openai reasoning 请求体（`max_completion_tokens` 加省略 temperature），以及这三家对 `joint_prompt()` 三条承诺的遵守情况。deepseek 的端点与模型实名已实测通过 | 本机无这三家凭据，冒烟不做（261008 拍板）。拿到凭据再按 §常用命令 验证。结论保持"未验证" |
| 2 | 已知代价，不改 | 5 个需凭据或需已有配置的示例（`set_llm`、`chat_llm`、`build_joint`、`llm_join`、`get_llm`）在 CRAN 检查机上永不执行。审核人 2026-06 反对 `\dontrun` 的理由（隐藏 bug 不被发现）以此形式回归 | 保持现状。缓解已在位：`joint_prompt()` 与 `parse_joint()` 的普通示例每次 check 都执行；`get_llm()` 另有 testthat 断言覆盖 |
| 3 | 投稿前必做 | `cran-comments.md` 仍是 0.3.0 内容 | 重写并覆盖 0.3.2 七项：DeepSeek provider、移除 `readr`、防伪造校验、示例守卫、`get_llm()`、凭据不回显护栏、joint 解析与提示词加固 |
| 4 | 搁置，等回复 | GitHub 服务端缓存，工单 #4830894。6 个旧提交 SHA 中 5 个已不可达，仅 `b9b1c4b` 仍可直链访问（261008 复测 200；对照 SHA 得 422，检测法有效） | 不重复开票，不主动跟进。Support 回复后复测 6 个 SHA 并关闭；被拒就回退为等待服务端 GC |
| 5 | 待拍板 | 换行策略：`core.autocrlf=true`，仓库无 `.gitattributes`（工作树实测为 LF）。风险是 Windows checkout 得 CRLF、build 出的 tar 带 CRLF，CRAN 有 `CRLF line endings` note 先例 | 二选一：加 `.gitattributes`（`* text=auto eol=lf`）或维持现状。定下来前不改换行配置 |
| 6 | 待拍板 | 思考型模型的 token 预算没有用户侧文档：README 对 `max_tokens`、`thinking`、`reason` 零命中，`chat_llm` 的 `@param .max_tokens` 只写参数发到哪个字段。实测 `deepseek-flash` 在 `.max_tokens = 300` 时预算全部耗尽于 reasoning，`message.content` 返回空串；报错文案已指向原因，默认 30000 不受影响 | 决定是否补进 README 与 `man/chat_llm.Rd`。补就同步 NEWS.md 并重跑 `devtools::document()` |

## 常用命令

```bash
Rscript -e "testthat::test_local('.')"          # 全量测试(从源码加载,勿用 library(llmjoin))
Rscript -e 'devtools::test_active_file("tests/testthat/test-parse_joint.R")'
Rscript -e 'devtools::load_all(".")'            # 交互开发加载
Rscript -e 'devtools::check()'                  # 完整 R CMD check
Rscript -e 'devtools::check(cran = TRUE)'       # 投稿口径(= R CMD check --as-cran)
Rscript -e 'devtools::document()'               # 生成 man/ 文档
Rscript -e 'devtools::install()'                # 本地安装(保持与源码同步)
read -s LLMJOIN_API_KEY && export LLMJOIN_API_KEY && Rscript -e 'devtools::check(cran = TRUE)'   # 带 key 冒烟
```

- 跑 check 前清 locale 变量：`env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript ...`。Git Bash 导出的 `C.UTF-8` 会让 Windows R 启动报错，并被记成假 ERROR/WARNING。
- `Rscript` 可能不在 PATH：PATH 指向 R-4.6.0，实装是 R-4.6.1。改用 `"/c/Program Files/R/R-4.6.1/bin/Rscript.exe"`。多行 R 代码写成 `.R` 文件再执行；`-e '...'` 在本机 Git Bash 下易被引号和编码破坏。
- `--as-cran` 会执行示例，含 `\donttest`（261008 实测）。保持口径⑦ 的 `@examplesIf` 守卫，否则本地 `LLMJOIN.yml` 会被覆盖成占位符，并向已配置的 provider 发出真实请求。
- 冒烟只用 deepseek，它是本机唯一有凭据的一家。5 个受守卫示例会真实执行、产生费用并覆盖 `LLMJOIN.yml`，跑前先备份该文件。key 用 `read -s` 读入；把 key 写进命令行会进 shell history，与 P0.1 同类泄露。

## 交接记录约定（handoff/）

一份交接文档 = 一个工作会话的收口快照，让下一个 agent 不带上下文也能接续。`handoff/` 兼任项目时间线（口径①）。

- **风格与体量**：按 P0.6 撰写。只写四类内容：本轮改动、验证数值、口径变化、未决事项。目标 ≤ 150 行；逐时补记与过程叙述在收口前合并成结论。
- **命名** `<yyMMdd>_<主题>.md`：日期领衔，文件名排序即时间序。主题用简短中文短语，不含空格，词间用连字符。同日多份靠主题区分，仍冲突就加 `-HHmm`。2026-10-06 前的记录已在 261007 批量改为日期领衔命名。
- **时机**：开工即建当次文档，随做随记，会话收口时补全定稿。定稿后只增不改，勘误写进下一份 handoff 的「更正」。
- **结构**：下面 7 节，节名可合并，§2 验证结果不可缺。

```markdown
# Handoff <yyMMdd> — <主题>

## 0. 元信息
- 收口时间:<yyyy-MM-dd HH:mm>
- 上一份交接:handoff/<yyMMdd>_<主题>.md(指针以实际前一份为准)
- 下一会话焦点:<未指定写「未指定」>

## 1. 改动清单(每改动一行)
- **<一句话动作>**:<做了什么>;动机:<为什么>;影响面:<文件/产物清单>。

## 2. 验证结果
- <测试/QC/审计结果,带数值;未验证如实标注「未验证」>。

## 3. 口径变化
- <无则写「无」;有则新旧对照一行,并注明总纲已同步>。

## 4. 遗留问题与下一步
- <缺口/未收口事项/风险/下一步动作>。

## 5. 建议技能(下一会话调用)
- `<skill-name>`:<一句为什么>。

## 6. 指针
- <产物路径、commit 哈希、相关文档——只给指针,不复制内容>。
```

- **内容**：引用而不复制——总纲、其他记录、commit diff 里已有的内容给路径或哈希。API key 与口令不落进文档，凭据只留占位与指针。写不出验证结果就说明改动尚未收口，先验证再定稿。
- **git**：收口的 handoff 必须提交入库，与实施改动同一提交或紧随其后的独立提交。`handoff/` 保持 git 追踪，`.Rbuildignore` 维持排除。收口时同步核对本文 §已知问题 与 `R/`、`tests/` 两份 AGENTS.md 的清单和基线数字。
- **接续**：下一会话先读本文，再读 `handoff/` 文件名排序最后一份（同日多份按元信息「上一份交接」指针回溯）。记载与现实冲突时以现实为准，冲突点记进下一份 handoff。待维护者拍板的事项必须登记进本文 §已知问题 或 §已拍板口径，只留在 handoff 遗留节不算登记。
