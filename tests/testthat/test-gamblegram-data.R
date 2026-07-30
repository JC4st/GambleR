test_that("gamblegram data has the expected structure", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_s3_class(result, "tbl_df")
  
  expect_named(
    result,
    c(
      "side",
      "component",
      "translation_key",
      "value",
      "display_order",
      "is_residual"
    )
  )
})

test_that("gamblegram data contains all expected components", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_setequal(
    result$component,
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

test_that("gamblegram data preserves zero-valued segments", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_true("other_cations" %in% result$component)
  expect_true("residual_anions" %in% result$component)
  
  expect_equal(
    sum(result$component == "other_cations"),
    1
  )
  
  expect_equal(
    sum(result$component == "residual_anions"),
    1
  )
})

test_that("cation and anion totals are equal", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  totals <- result |>
    dplyr::group_by(side) |>
    dplyr::summarise(
      total = sum(value),
      .groups = "drop"
    )
  
  expect_equal(
    totals$total[totals$side == "cation"],
    totals$total[totals$side == "anion"]
  )
})

test_that("ionized calcium contains its electrical charge contribution", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  calcium_value <- result |>
    dplyr::filter(component == "ionized_calcium") |>
    dplyr::pull(value)
  
  expect_equal(calcium_value, 2.4)
})

test_that("albumin contains the estimated graphical charge", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  albumin_value <- result |>
    dplyr::filter(component == "albumin") |>
    dplyr::pull(value)
  
  expect_equal(albumin_value, 11.2)
})

test_that("residual components are explicitly identified", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  residual_components <- result |>
    dplyr::filter(is_residual) |>
    dplyr::pull(component)
  
  expect_setequal(
    residual_components,
    c("other_cations", "residual_anions")
  )
})

test_that("translation keys remain language independent", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_equal(
    result$translation_key,
    result$component
  )
})

test_that("display order is sequential within each side", {
  result <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  cation_order <- result |>
    dplyr::filter(side == "cation") |>
    dplyr::pull(display_order)
  
  anion_order <- result |>
    dplyr::filter(side == "anion") |>
    dplyr::pull(display_order)
  
  expect_equal(cation_order, seq_along(cation_order))
  expect_equal(anion_order, seq_along(anion_order))
})