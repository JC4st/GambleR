test_that("valid chemistry inputs pass", {
  expect_true(
    validate_chemistry_inputs(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      lactate = 1,
      ionized_calcium = 1.2
    )
  )
})

test_that("negative values are rejected", {
  expect_error(
    validate_chemistry_inputs(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = -1,
      lactate = 1,
      ionized_calcium = 1.2
    ),
    "cannot be negative"
  )
})
