test_that("ionized calcium is represented as twice its concentration", {
  result <- cation_components(
    sodium = 140,
    potassium = 4,
    ionized_calcium = 1.2
  )

  expect_equal(unname(result[["ionized_calcium"]]), 2.4)
})

test_that("anion components include lactate and estimated albumin charge", {
  result <- anion_components(
    chloride = 104,
    bicarbonate = 24,
    lactate = 1.5,
    albumin = 4
  )

  expect_equal(unname(result[["lactate"]]), 1.5)
  expect_equal(unname(result[["albumin"]]), 11.2)
})

test_that("component builders reject negative values", {
  expect_error(cation_components(140, -1, 1.2))
  expect_error(anion_components(104, 24, -1, 4))
})
