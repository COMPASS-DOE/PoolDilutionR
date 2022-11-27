
#' Optimize P (production) and k (consumption) for a pool dilution experiment
#'
#' @param time Time vector of time values, numeric (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param Nm Normalization factor for pool size, see Eq. 12
#' @param Nd Normalization factor for isotopic signature, see Eq. 13
#' @param params Parameters TODO
#' @param cost_fn Cost function to use; the default is \code{\link{cost_function}}
#' @param prediction_fn Prediction function that the cost function will use;
#' the default is \code{\link{ap_prediction}}
#' @param control Optional list of control parameters; see \code{\link{optim}}
#'
#' @return The output of \code{\link{optim}}.
#' @export
#'
#' @examples
optimize_pk <- function(time, m, n, Nm, Nd,
                        params,
                        cost_fn = cost_function,
                        prediction_fn = ap_prediction) {

  # If P0 not given, guess
  if(is.null(params[["P0"]])) {
    stopifnot("Not implemented!")
  }
  # If k0 not given, guess
  if(is.null(params[["k0"]])) {
    params[["k0"]] <- estimate_k0(time, n, FRAC_K)
  }
  # If method not given, set default
  if(is.null(params[["method"]])) {
    params[["method"]] <- "L-BFGS-B"
  }
  # If bounds not given, provide some so that the optimizer
  # can't produce <0 values for P, nor <=0 for k
  if(is.null(params[["lower"]])) {
    params[["lower"]] <- c("P" = 0.0, "k"= 0.0001)
  }
  if(is.null(params[["upper"]])) {
    params[["upper"]] <- c("P" = Inf, "k"= Inf)
  }
  # If control not given, use an empty list
  if(is.null(params[["control"]])) {
    params[["control"]] <- list()
  }

  # Create a closure for logging progress
  log_msgs <- list()
  step <- 1
  plog <- function(x) {
    log_msgs[[step]] <<- as.data.frame(x)
    step <<- step + 1
  }

  # Call optim()
  out <- optim(par = c("P" = params[["P0"]], "k"= params[["k0"]]),
        fn = cost_fn,
        method = params[["method"]],
        lower = params[["lower"]],
        upper = params[["upper"]],
        control = params[["control"]],

        # "..." that the optimizer will pass to cost_fn:
        time = time,
        m = m,
        n = n,
        Nm = Nm,
        Nd = Nd,
        log_progress = plog)

  out$initial_par <- c("P0" = params[["P0"]], "k0" = params[["k0"]])
  out$progress <- do.call(rbind, c(log_msgs, make.row.names = FALSE))
  out
}


#' Estimate initial k from heavy isotope concentration data
#'
#' @param time Time vector of time values, numeric (e.g. days); first should be zero
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param frac_k Fractionation: 13C consumption as a fraction of 12C consumption
#'
#' @return Initial estimate of k0 (consumption rate constant)
#' @export
#'
#' @examples
#' estimate_k0(1:5, c(1, 0.9, 0.7, 0.65, 0.4), frac_k = 0.98)
estimate_k0 <- function(time, n, frac_k) {
  # Estimate starting k by slope of 13C following para. 21 in VfH2002:
  mod <- lm(log(n) ~ time)
  m_slope <- unname(mod$coefficients["time"])
  # Generally, this slope is negative (net 13CH4 consumption)
  # If not, our k0 estimate below won't work
  # For now, ensure this is true; there's probably a more sophisticated
  # way to estimate k0 in this case but save that for the future
  m_slope <- -abs(m_slope)

  # "...multiplied by 1/a to correct for fractionation against the
  # labeled methane." (BBL: this should be "1/-a"; see equation 8)
  k0 = m_slope / -frac_k
  message("Estimated k0 = ", k0, " from m_slope = ", m_slope)
  k0
}
