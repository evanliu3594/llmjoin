x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

test_that("llmjoin", joint_prompt(unique(x["id"]), unique(y["month"])))