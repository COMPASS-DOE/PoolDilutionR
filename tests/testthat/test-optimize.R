test_that("set_default_params works", {

  p <- list(P0 = 1)
  times <- 0:5
  # convenience function
  sdp <- function(p) {
    set_default_params(p)
  }

  # Basic behavior: returns a list
  x <- sdp(p)
  expect_type(x, "list")

  # method not supplied
  expect_type(x$method, "character")
  # method supplied
  p$method <- "test"
  x <- sdp(p)
  expect_identical(x$method, "test")

  # lower not supplied
  expect_type(x$lower, "double")
  exp_names <- sort(c("P", "k", "frac_P", "frac_k"))
  expect_identical(sort(names(x$lower)), exp_names)

  # lower supplied
  p$lower <- "test"
  x <- sdp(p)
  expect_identical(x$lower, "test")

  # upper not supplied
  expect_type(x$upper, "double")
  expect_identical(sort(names(x$upper)), exp_names)

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
