# TDD/BDD: build_joint() — End-to-end fuzzy-join mapping via LLM
# Contract: orchestrates joint_prompt + chat_llm + parse_joint
# Contract: returns a 2-column data.frame mapping key1 to key2.
# Contract: passes extra arguments through to chat_llm.

describe("build_joint", {

  describe("successful joint building", {

    it("should return a 2-column data.frame mapping numeric IDs to month names", {
      skip_if_no_llm()

      x <- data.frame(id = c("01", "02", "04"), value = c(10, 20, 40))
      y <- data.frame(month = c("January", "Feb", "May"), amount = c(100, 200, 400))

      result <- build_joint(x, y, key1 = "id", key2 = "month")

      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
      expect_equal(colnames(result), c("id", "month"))
      expect_gt(nrow(result), 0)
    })

    it("should map input keys to target values, allowing empty for no match", {
      skip_if_no_llm()

      x <- data.frame(x = c("01", "02", "04"))
      y <- data.frame(y = c("January", "Feb", "May"))

      result <- build_joint(x, y, key1 = "x", key2 = "y")

      expect_true(all(result[["x"]] %in% c("01", "02", "04")))
      non_empty <- result[["y"]] != "" & !is.na(result[["y"]])
      if (any(non_empty)) {
        expect_true(all(result[["y"]][non_empty] %in% c("January", "Feb", "May")))
      }
    })

    it("should handle single-row mappings", {
      skip_if_no_llm()

      x <- data.frame(key = "Jan")
      y <- data.frame(key = "January")

      result <- build_joint(x, y, key1 = "key", key2 = "key")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
    })

    it("should pass extra arguments to chat_llm via ...", {
      skip_if_no_llm()

      x <- data.frame(id = c("01", "02", "04"))
      y <- data.frame(month = c("January", "Feb", "May"))

      result <- build_joint(x, y, key1 = "id", key2 = "month",
                            .temperature = 0)

      expect_s3_class(result, "data.frame")
      expect_equal(ncol(result), 2)
    })

  })

})
