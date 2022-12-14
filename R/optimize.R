
#' Optimize P (production) and k (consumption) for a pool dilution experiment
#'
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param Nm Normalization factor for pool size, see Eq. 12
#' @param Nd Normalization factor for isotopic signature, see Eq. 13
#' @param params Parameters TODO
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#' @param cost_fn Cost function to use; the default is \code{\link{cost_function}}
#' @param prediction_fn Prediction function that the cost function will use;
#' the default is \code{\link{ap_prediction}}
#'
#' @importFrom stats optim
#' @return The output of \code{\link{optim}}.
#' @export
#'
#' @examples
#' m <- c(10, 8, 6, 5, 4, 3)
#' n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
#' Nm = m / 10
#' Nd = n / 10
#' optimize_pk(time = 0:5, m, n, Nm, Nd, params = list(P0 = 0.5))
optimize_pk <- function(time, m, n, Nm, Nd,
                        params,
                        pool = "CH4",
                        frac_P = P_default(pool),
                        frac_k = k_default(pool),
                        cost_fn = cost_function,
                        prediction_fn = ap_prediction) {

  # Set defaults if not given by user
  params <- set_default_params(params, time, n, frac_k)

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
        frac_P = frac_P,
        frac_k = frac_k,
        log_progress = plog)

  out$initial_par <- c("P0" = params[["P0"]], "k0" = params[["k0"]])
  out$progress <- do.call(rbind, c(log_msgs, make.row.names = FALSE))
  out
}


#' Set default parameters for optimization
#'
#' @param params A named list of parameters
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#'
#' @return The \code{params} list with entries filled in as needed.
set_default_params <- function(params, time, n, frac_k) {

  # If P0 not given, guess
  if(is.null(params[["P0"]])) {
    stop("Not implemented!")
  }
  # If k0 not given, guess
  if(is.null(params[["k0"]])) {
    params[["k0"]] <- estimate_k0(time, n, frac_k)
  }
  # If method not given, set default
  if(is.null(params[["method"]])) {
    params[["method"]] <- "L-BFGS-B"
  }
  # If bounds not given, provide some so that the optimizer
  # isn't allowed to produce <0 values for P, nor <=0 for k
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
  params
}


#' Estimate initial k from heavy isotope concentration data
#'
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param frac_k Fractionation: 13C consumption as a fraction of 12C consumption
#'
#' @importFrom stats lm
#' @return Initial estimate of k0 (consumption rate constant)
#' @export
#'
#' @examples
#' estimate_k0(1:5, c(1, 0.9, 0.7, 0.65, 0.4), frac_k = 0.98)
estimate_k0 <- function(time, n, frac_k) {
  if(!all(diff(time) > 0)) stop("Time values must increase.")

  # Estimate starting k by slope of 13C following para. 21 in VfH2002:
  mod <- lm(log(n) ~ time)
  n_slope <- unname(mod$coefficients["time"])
  # Generally, this slope is negative (net 13CH4 consumption)
  # If not, our k0 estimate below won't work
  # For now, ensure this is true; there's probably a more sophisticated
  # way to estimate k0 in this case but save that for the future
  n_slope <- -abs(n_slope)

  # "...multiplied by 1/a to correct for fractionation against the
  # labeled methane." (BBL: this should be "1/-a"; see equation 8)
  k0 = n_slope / -frac_k
  #message("Estimated k0 = ", k0, " from n_slope = ", n_slope)
  k0
}
