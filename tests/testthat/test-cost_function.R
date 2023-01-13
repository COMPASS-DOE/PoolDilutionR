test_that("cost function works", {

  times <- 0:5
  m <- c(10, 8, 6, 5, 4, 3)
  n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
  Nm = m / 10
  Nd = n / 10

  x <- cost_function(params = list(P = 0.5, k = 0.3),
                     time = times,
                     m = m, n = n,  m_prec = 0.001, ap_prec = 1)
  expect_type(x, "double")
  expect_identical(length(x), 1L)

  # TODO: not sure what else to test

  # Log function called if supplied
  logfn_called <- FALSE
  logfn <- function(df) {
    # Cost functions should pass a data frame to the log function
    expect_s3_class(df, "data.frame")
    logfn_called <<- TRUE
  }
  cost_function(params = list(P = 0.5, k = 0.3),
                time = times,
                m = m, n = n,  m_prec = 0.001, ap_prec = 1,
                log_progress = logfn)
  expect_true(logfn_called)
})
