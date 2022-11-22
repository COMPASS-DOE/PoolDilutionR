test_that("catch bad input", {
  expect_error(ap_prediction(1), regexp = "must be zero")
  expect_error(ap_prediction("1"), regexp = "must be numeric")
  expect_error(ap_prediction(c(0,2,1)))
})
