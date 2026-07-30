test_that("inputs module returns default chemistry values", {
  shiny::testServer(
    mod_gamblegram_inputs_server,
    {
      session$setInputs(
        language = "es",
        sodium = 140,
        potassium = 4,
        chloride = 104,
        bicarbonate = 24,
        albumin = 4,
        lactate = 1,
        ionized_calcium = 1.2
      )
      
      result <- chemistry()
      
      expect_equal(result$sodium, 140)
      expect_equal(result$potassium, 4)
      expect_equal(result$chloride, 104)
      expect_equal(result$bicarbonate, 24)
      expect_equal(result$albumin, 4)
      expect_equal(result$lactate, 1)
      expect_equal(result$ionized_calcium, 1.2)
    }
  )
})

test_that("inputs module returns selected language", {
  shiny::testServer(
    mod_gamblegram_inputs_server,
    {
      session$setInputs(language = "en")
      
      expect_equal(language(), "en")
      
      session$setInputs(language = "es")
      
      expect_equal(language(), "es")
    }
  )
})

test_that("inputs module reacts to chemistry changes", {
  shiny::testServer(
    mod_gamblegram_inputs_server,
    {
      session$setInputs(
        language = "es",
        sodium = 140,
        potassium = 4,
        chloride = 104,
        bicarbonate = 24,
        albumin = 4,
        lactate = 1,
        ionized_calcium = 1.2
      )
      
      expect_equal(chemistry()$chloride, 104)
      
      session$setInputs(chloride = 110)
      
      expect_equal(chemistry()$chloride, 110)
    }
  )
})

test_that("inputs module returns entered values for downstream validation", {
  shiny::testServer(
    mod_gamblegram_inputs_server,
    {
      session$setInputs(
        language = "es",
        sodium = 140,
        potassium = 4,
        chloride = -1,
        bicarbonate = 24,
        albumin = 4,
        lactate = 1,
        ionized_calcium = 1.2
      )
      
      expect_equal(
        chemistry()$chloride,
        -1
      )
      
      issues <- validate_chemistry_for_ui(
        values = chemistry(),
        language = language()
      )
      
      expect_equal(
        issues,
        "Los valores no pueden ser negativos."
      )
    }
  )
})

test_that("changing language preserves chemistry values", {
  shiny::testServer(
    mod_gamblegram_inputs_server,
    {
      session$setInputs(
        language = "es",
        sodium = 137,
        potassium = 3.7,
        chloride = 99,
        bicarbonate = 19,
        albumin = 2.8,
        lactate = 4.2,
        ionized_calcium = 1.05
      )
      
      values_before <- chemistry()
      
      session$setInputs(language = "en")
      session$flushReact()
      
      values_after <- chemistry()
      
      expect_equal(values_after, values_before)
      expect_equal(language(), "en")
    }
  )
})

test_that("chemistry remains reactive after changing language", {
  shiny::testServer(
    mod_gamblegram_inputs_server,
    {
      session$setInputs(
        language = "es",
        sodium = 140,
        potassium = 4,
        chloride = 104,
        bicarbonate = 24,
        albumin = 4,
        lactate = 1,
        ionized_calcium = 1.2
      )
      
      session$setInputs(language = "en")
      session$setInputs(chloride = 112)
      
      expect_equal(chemistry()$chloride, 112)
      expect_equal(language(), "en")
    }
  )
})