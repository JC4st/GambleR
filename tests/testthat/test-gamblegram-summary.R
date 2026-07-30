test_that("summary calculates observed and corrected anion gaps", {
  result <- build_gamblegram_summary(
    sodium = 140,
    chloride = 104,
    bicarbonate = 24,
    albumin = 2,
    language = "en"
  )
  
  expect_equal(
    result$values$observed_anion_gap,
    12
  )
  
  expect_equal(
    result$values$corrected_anion_gap,
    17
  )
})

test_that("summary returns Spanish labels", {
  result <- build_gamblegram_summary(
    sodium = 140,
    chloride = 104,
    bicarbonate = 24,
    albumin = 2,
    language = "es"
  )
  
  expect_equal(
    result$title,
    "Resumen de la brecha aniónica"
  )
  
  expect_equal(
    result$labels$observed_anion_gap,
    "Brecha aniónica observada"
  )
  
  expect_equal(
    result$labels$corrected_anion_gap,
    "Brecha aniónica corregida por albúmina"
  )
})

test_that("summary returns English labels", {
  result <- build_gamblegram_summary(
    sodium = 140,
    chloride = 104,
    bicarbonate = 24,
    albumin = 2,
    language = "en"
  )
  
  expect_equal(
    result$title,
    "Anion gap summary"
  )
  
  expect_equal(
    result$labels$observed_anion_gap,
    "Observed anion gap"
  )
})

test_that("summary formats decimal marks according to language", {
  spanish <- build_gamblegram_summary(
    sodium = 139.5,
    chloride = 104,
    bicarbonate = 24,
    albumin = 3,
    language = "es",
    digits = 1
  )
  
  english <- build_gamblegram_summary(
    sodium = 139.5,
    chloride = 104,
    bicarbonate = 24,
    albumin = 3,
    language = "en",
    digits = 1
  )
  
  expect_equal(
    spanish$formatted_values$observed_anion_gap,
    "11,5"
  )
  
  expect_equal(
    english$formatted_values$observed_anion_gap,
    "11.5"
  )
})

test_that("summary display text includes values and units", {
  result <- build_gamblegram_summary(
    sodium = 140,
    chloride = 104,
    bicarbonate = 24,
    albumin = 2,
    language = "es"
  )
  
  expect_equal(
    result$display_text$observed_anion_gap,
    "Brecha aniónica observada: 12,0 mmol/L"
  )
  
  expect_equal(
    result$display_text$corrected_anion_gap,
    paste(
      "Brecha aniónica corregida por albúmina:",
      "17,0 mmol/L"
    )
  )
})

test_that("summary contains educational notes without diagnosis", {
  result <- build_gamblegram_summary(
    sodium = 140,
    chloride = 104,
    bicarbonate = 24,
    albumin = 2,
    language = "es"
  )
  
  expect_named(
    result$notes,
    c(
      "albumin_correction",
      "residual_compartments",
      "disclaimer"
    )
  )
  
  expect_match(
    result$notes$disclaimer,
    "educativa"
  )
  
  expect_match(
    result$notes$residual_compartments,
    "ilustrativos"
  )
})

test_that("normal albumin produces identical observed and corrected AG", {
  result <- build_gamblegram_summary(
    sodium = 140,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    language = "en"
  )
  
  expect_equal(
    result$values$observed_anion_gap,
    result$values$corrected_anion_gap
  )
})

test_that("summary validates language", {
  expect_error(
    build_gamblegram_summary(
      sodium = 140,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      language = "fr"
    ),
    "Unsupported language"
  )
})

test_that("summary validates digits", {
  expect_error(
    build_gamblegram_summary(
      sodium = 140,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      digits = -1
    ),
    "non-negative integer"
  )
  
  expect_error(
    build_gamblegram_summary(
      sodium = 140,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      digits = 1.5
    ),
    "non-negative integer"
  )
})

test_that("summary rejects negative chemistry values", {
  expect_error(
    build_gamblegram_summary(
      sodium = 140,
      chloride = 104,
      bicarbonate = 24,
      albumin = -1
    ),
    "cannot be negative"
  )
})