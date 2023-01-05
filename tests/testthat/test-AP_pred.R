test_that("ap_prediction works", {
  # Bad inputs
  expect_error(ap_prediction(1), regexp = "must be zero")
  expect_error(ap_prediction("1"), regexp = "must be numeric")
  expect_error(ap_prediction(c(0, 2, 1)), regexp = "must increase")

  # Basic behavior, P < C
  times <- 0:5
  x <- ap_prediction(time = 0:5, m0 = 10, n0 = 1, P = 0.5, k = 0.3)
  expect_identical(nrow(x), length(times))
  expect_identical(names(x), c("mt", "nt", "AP_pred"))
  expect_true(all(diff(x$mt) < 0)) # decreasing mt
  expect_true(all(diff(x$nt) < 0)) # decreasing nt
  expect_true(all(diff(x$AP_pred) < 0)) # decreasing AP_pred

  # Basic behavior, P > C
  times <- 0:5
  x <- ap_prediction(time = 0:5, m0 = 10, n0 = 1, P = 5, k = 0.3)
  expect_identical(nrow(x), length(times))
  expect_identical(names(x), c("mt", "nt", "AP_pred"))
  expect_true(all(diff(x$mt) > 0)) # increasing mt
  expect_true(all(diff(x$nt) < 0)) # decreasing nt
  expect_true(all(diff(x$AP_pred) < 0)) # decreasing AP_pred

  # Behavior: No production or consumption
  # If inputs P = k = 0.0 , then outputs mt and nt should be constant
  # and equal to m0 and n0 respectively.



  # Behavior: No methane and no production
  # A variant of the previous one. If inputs P = m0 = n0 = 0.0 ,
  # then outputs mt and nt should be constant zero
  x <- ap_prediction(time = times, m0 = 0, n0 = 0, P = 0.0, k = 0.1)
  expect_true(all(x$mt == 0))
  expect_true(all(x$nt == 0))

  # Behavior: No discrimination
  # If inputs frac_P = frac_k = 1.0 , then output AP_pred should be constant.


})
