test_that("translate_text returns Spanish text", {
  expect_equal(
    translate_text("sodium", language = "es"),
    "Sodio"
  )

  expect_equal(
    translate_text("observed_ag", language = "es"),
    "Brecha aniónica observada"
  )
})

test_that("translate_text returns English text", {
  expect_equal(
    translate_text("sodium", language = "en"),
    "Sodium"
  )

  expect_equal(
    translate_text("corrected_ag", language = "en"),
    "Albumin-corrected anion gap"
  )
})

test_that("unknown keys return the key by default", {
  expect_equal(
    translate_text("unknown_key", language = "es"),
    "unknown_key"
  )
})

test_that("unknown keys accept a custom fallback", {
  expect_equal(
    translate_text(
      key = "unknown_key",
      language = "es",
      fallback = "Texto no disponible"
    ),
    "Texto no disponible"
  )
})

test_that("unsupported languages are rejected", {
  expect_error(
    translate_text("sodium", language = "fr"),
    "Unsupported language"
  )
})

test_that("component labels are named correctly", {
  labels <- get_component_labels("es")

  expect_type(labels, "character")

  expect_named(
    labels,
    c(
      "sodium",
      "potassium",
      "ionized_calcium",
      "other_cations",
      "chloride",
      "bicarbonate",
      "lactate",
      "albumin",
      "residual_anions"
    )
  )
})

test_that("component labels change with language", {
  spanish <- get_component_labels("es")
  english <- get_component_labels("en")

  expect_equal(spanish[["sodium"]], "Sodio")
  expect_equal(english[["sodium"]], "Sodium")

  expect_false(identical(spanish, english))
})

test_that("all component translation keys exist", {
  spanish <- get_component_labels("es")
  english <- get_component_labels("en")

  expect_false(any(names(spanish) == spanish))
  expect_false(any(names(english) == english))
})

test_that("get_plot_labels returns the complete plot dictionary", {
  labels <- get_plot_labels("es")
  
  expect_named(
    labels,
    c("components", "sides", "y_axis")
  )
  
  expect_equal(labels$sides[["cation"]], "Cationes")
  expect_equal(labels$sides[["anion"]], "Aniones")
  
  expect_equal(
    labels$y_axis,
    "Contribución de carga (mEq/L)"
  )
  
  expect_equal(
    labels$components[["albumin"]],
    "Albúmina"
  )
})

test_that("get_plot_labels supports English", {
  labels <- get_plot_labels("en")
  
  expect_equal(labels$sides[["cation"]], "Cations")
  expect_equal(labels$sides[["anion"]], "Anions")
  
  expect_equal(
    labels$y_axis,
    "Charge contribution (mEq/L)"
  )
})