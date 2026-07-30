test_that("graphical albumin charge uses factor 2.8", {
  expect_equal(albumin_charge(4), 11.2)
  expect_equal(albumin_charge(2.5), 7)
})

test_that("albumin charge rejects negative and invalid values", {
  expect_error(albumin_charge(-1))
  expect_error(albumin_charge(NA_real_))
  expect_error(albumin_charge(c(3, 4)))
})
