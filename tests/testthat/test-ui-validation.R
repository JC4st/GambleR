valid_chemistry_values <- function() {
  list(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
}

test_that("valid UI chemistry returns no issues", {
  issues <- validate_chemistry_for_ui(
    values = valid_chemistry_values(),
    language = "es"
  )
  
  expect_length(issues, 0)
})

test_that("UI validation detects missing values", {
  values <- valid_chemistry_values()
  values$chloride <- NULL
  
  issues <- validate_chemistry_for_ui(
    values = values,
    language = "es"
  )
  
  expect_equal(
    issues,
    "Complete todos los campos antes de generar el Gamblegram."
  )
})

test_that("UI validation detects non-finite values", {
  values <- valid_chemistry_values()
  values$lactate <- Inf
  
  issues <- validate_chemistry_for_ui(
    values = values,
    language = "en"
  )
  
  expect_equal(
    issues,
    "All chemistry values must be finite numeric values."
  )
})

test_that("UI validation detects negative values", {
  values <- valid_chemistry_values()
  values$albumin <- -1
  
  issues <- validate_chemistry_for_ui(
    values = values,
    language = "es"
  )
  
  expect_equal(
    issues,
    "Los valores no pueden ser negativos."
  )
})

test_that("UI validation allows extreme finite values", {
  values <- valid_chemistry_values()
  
  values$sodium <- 250
  values$chloride <- 10
  values$bicarbonate <- 80
  values$lactate <- 40
  
  issues <- validate_chemistry_for_ui(
    values = values,
    language = "es"
  )
  
  expect_length(issues, 0)
})

test_that("UI validation messages follow the selected language", {
  values <- valid_chemistry_values()
  values$potassium <- -1
  
  spanish <- validate_chemistry_for_ui(
    values,
    language = "es"
  )
  
  english <- validate_chemistry_for_ui(
    values,
    language = "en"
  )
  
  expect_equal(
    spanish,
    "Los valores no pueden ser negativos."
  )
  
  expect_equal(
    english,
    "Chemistry values cannot be negative."
  )
})