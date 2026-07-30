test_that("observed anion gap excludes potassium", {
  expect_equal(
    calculate_anion_gap(
      sodium = 140,
      chloride = 104,
      bicarbonate = 24
    ),
    12
  )
})

test_that("albumin correction uses the specified formula", {
  expect_equal(
    calculate_corrected_anion_gap(
      observed_ag = 12,
      albumin = 2
    ),
    17
  )
})

test_that("normal albumin leaves the anion gap unchanged", {
  expect_equal(
    calculate_corrected_anion_gap(
      observed_ag = 12,
      albumin = 4
    ),
    12
  )
})

test_that("anion gap functions reject non-finite input", {
  expect_error(calculate_anion_gap(140, NA_real_, 24))
  expect_error(calculate_corrected_anion_gap(12, Inf))
})
