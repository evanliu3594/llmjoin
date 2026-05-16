# TDD/BDD: check_joint() — Ask LLM to validate a built joint
# Contract: returns joint unchanged when LLM finds no issues
# Contract: filters out problematic rows identified by LLM
# Contract: handles malformed LLM response gracefully by returning joint unchanged

describe("check_joint", {

  describe("LLM finds no issues", {

    it("should return joint unchanged when LLM returns empty response", {
      # Given: a joint with valid mappings
      joint <- data.frame(
        id = c("01", "02", "04"),
        month = c("January", "Feb", "May"),
        stringsAsFactors = FALSE
      )

      # Given: LLM says no problems (returns empty/whitespace)
      local_mocked_bindings(
        chat_llm = function(...) "  ",
        .package = "llmjoin"
      )

      # When: checking the joint
      result <- check_joint(joint)

      # Then: joint is returned unchanged
      expect_equal(result, joint)
    })

    it("should return joint unchanged when LLM returns generic 'no issues' response", {
      joint <- data.frame(
        name = c("Germany", "France"),
        std = c("Germany", "France"),
        stringsAsFactors = FALSE
      )

      local_mocked_bindings(
        chat_llm = function(...) "All looks good.",
        .package = "llmjoin"
      )

      result <- check_joint(joint)

      expect_equal(result, joint)
    })

  })

  describe("LLM finds problematic rows", {

    it("should filter out rows flagged as problematic by LLM", {
      # Given: a joint with one suspicious mapping
      joint <- data.frame(
        a = c("France", "Narnia", "Germany"),
        b = c("France", "Atlantis", "Germany"),
        stringsAsFactors = FALSE
      )

      # Given: LLM returns the problematic phrase in its expected format
      local_mocked_bindings(
        chat_llm = function(...) "Narnia is equal to Atlantis,",
        .package = "llmjoin"
      )

      # When: checking the joint
      result <- check_joint(joint)

      # Then: the problematic row is removed
      expect_equal(nrow(result), 2)
      expect_false("Narnia" %in% result[["a"]])
      expect_equal(result[["a"]], c("France", "Germany"))
    })

    it("should filter out only the first problematic row from LLM response", {
      joint <- data.frame(
        code = c("CHN", "CN", "NAR", "WAK"),
        name = c("China", "China", "Narnia", "Wakanda"),
        stringsAsFactors = FALSE
      )

      # parser extracts first `strsplit` token as filter key; only one row removed
      local_mocked_bindings(
        chat_llm = function(...) paste0(
          "NAR is equal to Narnia,\n",
          "WAK is equal to Wakanda,"
        ),
        .package = "llmjoin"
      )

      result <- check_joint(joint)

      # First problematic row (NAR) is filtered; WAK remains
      expect_equal(nrow(result), 3)
      expect_false("NAR" %in% result[["code"]])
      expect_true("CHN" %in% result[["code"]])
    })

    it("should return joint unchanged when LLM response has no matchable format", {
      joint <- data.frame(
        x = c("A", "B"),
        y = c("C", "D"),
        stringsAsFactors = FALSE
      )

      # Given: LLM response doesn't contain " is equal to " pattern
      local_mocked_bindings(
        chat_llm = function(...) "Problem: some mappings are wrong!!!",
        .package = "llmjoin"
      )

      result <- check_joint(joint)

      expect_equal(result, joint)
    })

  })

})
