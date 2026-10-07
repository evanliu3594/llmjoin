# [DONE] AGENTS与handoff纳入git (261005)

## 背景
维护者拍板(承接 [DONE]-引入project-conventions分层约束-261004.md 的遗留项):AGENTS.md
与 handoff/ 纳入 git 追踪,但不进 R CMD 构建(.Rbuildignore 维持排除)。

## 做了什么
- 核实基线:根 AGENTS.md 与三条 261004 记录(代码审核、协作说明迁移、移除AI协作工具痕迹)
  已由维护者在 d906c2c/95d390b 纳入追踪并完成措辞中性化(含记录文件重命名);.Rbuildignore
  四条排除规则(^AGENTS\.md$、^R/AGENTS\.md$、^tests/AGENTS\.md$、^handoff$)已齐备,无需改动。
- 全量排查工具名痕迹(口径③):grep 确认仅剩一处——本轮 261004 记录中的"原协作说明",
  按维护者就地中性化先例改为"旧协作说明文件";小写 claude 的 9 处命中均为 provider 功能
  语境(注册表/请求体/响应解析),按口径③例外保留。
- 总纲登记拍板为 §已拍板口径④(版本管理)。
- 提交:三份 AGENTS 文件(根、R/、tests/)+ 三条未跟踪 handoff 记录 + 本记录。

## 变更文件
- AGENTS.md:§已拍板口径新增④
- handoff/[DONE]-引入project-conventions分层约束-261004.md:一处工具名中性化
- handoff/ 三条未跟踪记录纳入追踪;R/AGENTS.md、tests/AGENTS.md 首次纳入追踪

## 验证
- grep 复查:AGENTS*.md 与 handoff/*.md 无任何开发辅助工具点名;provider 语境 claude 9 处
  为例外范围。
- .Rbuildignore 与实际文件逐一对照,AGENTS 体系与 handoff/ 均不会进 R CMD 构建产物。

## 遗留与下一步
- git 历史仍有两处旧痕迹(d906c2c 提交信息、更早推送的旧协作说明文件本体),彻底清除需
  filter-repo + force-push 且会使 CRAN-SUBMISSION 中 SHA 失效——仍待维护者单独决策,不在
  本轮范围。
- 后续每轮 handoff 记录随仓库提交,措辞自始遵循口径③。
