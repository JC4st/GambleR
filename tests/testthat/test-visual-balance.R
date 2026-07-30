test_that("visual balance preserves graphical electroneutrality", {
  result <- calculate_visual_balance(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )

  expect_equal(sum(result$cations), sum(result$anions))
  expect_equal(result$total_charge, sum(result$cations))
})

test_that("visual balance reports observed and corrected AG as values", {
  result <- calculate_visual_balance(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 2,
    lactate = 1,
    ionized_calcium = 1.2
  )

  expect_equal(result$observed_anion_gap, 12)
  expect_equal(result$corrected_anion_gap, 17)
})

test_that("only one residual compartment is populated", {
  result <- calculate_visual_balance(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )

  expect_true(
    result$cations[["other_cations"]] == 0 ||
      result$anions[["residual_anions"]] == 0
  )
})
