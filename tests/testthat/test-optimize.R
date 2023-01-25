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


test_that("pdr_estimate_k0 works", {

  # bad input
  expect_error(pdr_estimate_k0(c(0, 2, 1)), regexp = "must increase")

  # output
  expect_message(pdr_estimate_k0(1:3, c(1, 0.9, 0.7), frac_k = 0.98), regexp = "Estimated k0")
  expect_silent(pdr_estimate_k0(1:3, c(1, 0.9, 0.7), frac_k = 0.98, quiet = TRUE))

  # negative trend
  x <- pdr_estimate_k0(1:3, c(1, 0.9, 0.7), frac_k = 0.98, quiet = TRUE)
  expect_type(x, "double")
  # positive trend
  x <- pdr_estimate_k0(1:3, c(1, 0.9, 0.7), frac_k = 0.98, quiet = TRUE)
  expect_type(x, "double")
})

test_that("pdr_optimize works", {

  # input data (from the pdr_optimize examples)
  tm <- 0:5
  m <- c(10, 8, 6, 5, 4, 3)
  n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
  m_prec <- 0.001
  ap_prec = 1

  # bad input
  expect_error(pdr_optimize(c(0, 2, 1)), regexp = "must increase")
  expect_error(pdr_optimize(tm, m, n, m_prec, ap_prec, P = 0.5, k = 0.3,
                            params_to_optimize = "bad_param"),
               regexp = "params_to_optimize must be")

  # try all possible combinations of params_to_optimize: P, k, P and k, etc.
  params <- c("P", "k", "frac_P", "frac_k")
  param_sets <- c()
  for(i in seq_along(params)) {
    param_sets <- c(param_sets, combn(params, i, simplify = FALSE))
  }
  for(p in param_sets) {
    expect_type(pdr_optimize(tm, m, n, m_prec, ap_prec, P = 0.5, k = 0.3, params_to_optimize = p), "list")
  }

  # estimates k if not given
  expect_message(pdr_optimize(tm, m, n, m_prec, ap_prec, P = 0.5), regexp = "Estimated k0")

  # initial parameters respected
  P <- 0.5
  k <- 0.3
  default <- pdr_optimize(tm, m, n, m_prec, ap_prec, P = P, k = k)
  expect_equal(default$initial_par["P"], P, ignore_attr = TRUE)
  expect_equal(default$initial_par["k"], k, ignore_attr = TRUE)

  # upper and lower bounds respected
  bounds <- c(default$par["k"] * 1.1, default$par["k"] * 1.3)
  bounds <- unname(bounds)
  bounded <- pdr_optimize(tm, m, n, m_prec, ap_prec, P = P, k = k,
                          other_params = list(lower = c("k" = bounds[1]),
                                              upper = c("k" = bounds[2])))
  expect_true(bounded$par["k"] >= bounds[1])
  expect_true(bounded$par["k"] <= bounds[2])

  # include_progress works
  x <- pdr_optimize(tm, m, n, m_prec, ap_prec, P = P, k = k)
  expect_null(x$progress)
  y <- pdr_optimize(tm, m, n, m_prec, ap_prec, P = P, k = k, include_progress = TRUE)
  expect_s3_class(y$progress, "data.frame")
})

test_that("pdr_optimize_tidy works", {

  # input data (from the pdr_optimize examples)
  tm <- 0:5
  m <- c(10, 8, 6, 5, 4, 3)
  n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
  m_prec <- 0.001
  ap_prec = 1

  # Should return a data frame with one row per parameter estimated
  x <- pdr_optimize_tidy(tm, m, n, m_prec, ap_prec, P = 0.5, k = 0.3,
                         params_to_optimize = c("P", "k"))
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 2L)

  x <- pdr_optimize_tidy(tm, m, n, m_prec, ap_prec, P = 0.5, k = 0.3,
                         params_to_optimize = c("P"))
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 1L)
})
