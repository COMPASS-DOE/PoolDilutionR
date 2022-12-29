test_that("set_default_params works", {

  # Errors is P0 not given
  expect_error(set_default_params(params = list()), regexp = "Not implemented")

  p <- list(P0 = 1)
  times <- 0:5
  # convenience function
  sdp <- function(p) {
    set_default_params(p, times, n = runif(length(times)), frac_k = 0.1)
  }

  # Basic behavior: returns a list
  x <- sdp(p)
  expect_type(x, "list")

  # k0 not supplied
  expect_type(x$k0, "double")
  # k0 supplied
  p$k0 <- 0.1
  x <- sdp(p)
  expect_identical(x$k0, 0.1)

  # method not supplied
  expect_type(x$method, "character")
  # method supplied
  p$method <- "test"
  x <- sdp(p)
  expect_identical(x$method, "test")

  # lower not supplied
  expect_type(x$lower, "double")
  expect_identical(sort(names(x$lower)), sort(c("P", "k")))

  # lower supplied
  p$lower <- "test"
  x <- sdp(p)
  expect_identical(x$lower, "test")

  # upper not supplied
  expect_type(x$upper, "double")
  expect_identical(sort(names(x$upper)), sort(c("P", "k")))

  # upper supplied
  p$upper <- "test"
  x <- sdp(p)
  expect_identical(x$upper, "test")

  # control not supplied
  expect_type(x$control, "list")

  # control supplied
  p$control <- "test"
  x <- sdp(p)
  expect_identical(x$control, "test")

})


test_that("estimate_k0 works", {

  # bad input
  expect_error(estimate_k0(c(0, 2, 1)), regexp = "must increase")

  # negative trend
  x <- estimate_k0(1:5, c(1, 0.9, 0.7, 0.65, 0.4), frac_k = 0.98)
  expect_type(x, "double")
  # positive trend
  x <- estimate_k0(1:5, c(0.4, 0.65, 0.7, 0.9, 1), frac_k = 0.98)
  expect_type(x, "double")
  # what else?
})
