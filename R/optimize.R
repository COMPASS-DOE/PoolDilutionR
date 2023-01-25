
#' Optimize P (production) and k (consumption) for a pool dilution experiment
#'
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param m_prec Instrument precision for pool size, expressed as a standard deviation
#' @param ap_prec Instrument precision for atom percent, expressed as a standard deviation
#' @param P production rate, unit gas/unit time
#' @param k first-order rate constant for consumption, 1/unit time
#' @param params_to_optimize Named vector of parameters ("P", "k", "frac_P",
#' and/or "frac_k") to optimize against observations
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#' @param other_params Other parameters pass on to \code{\link{optim}}
#' @param cost_fn Cost function to use; the default is \code{\link{cost_function}}
#' @param prediction_fn Prediction function that the cost function will use;
#' the default is \code{\link{ap_prediction}}
#' @param include_progress Include detailed optimizer progress data in output?
#' @param quiet Suppress output messages, logical
#'
#' @importFrom stats optim
#' @return The output of \code{\link{optim}}.
#' @export
#'
#' @examples
#' tm <- 0:5
#' m <- c(10, 8, 6, 5, 4, 3)
#' n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
#' m_prec <- 0.001
#' ap_prec <- 1
#' # Optimize values for P (production) and k (consumption)
#' pdr_optimize(time = tm, m, n, m_prec, ap_prec, P = 0.5, k = 0.3)
#' # If we don't provide a value for k, it can be estimated from the data
#' pdr_optimize(tm, m, n, m_prec, ap_prec, P = 0.5)
#' # Hold k and frac_k constant, optimize P and frac_P
#' pdr_optimize(tm, m, n, m_prec, ap_prec, P = 0.5, params_to_optimize = c("P", "frac_P"))
#' # Optimize only k
#' pdr_optimize(tm, m, n, m_prec, ap_prec, P = 0.5, params_to_optimize = "k")
#' # Optimize only k, bounding its possible values
#' op <- list(lower = c("k" = 0.2), upper = c("k" = 0.3))
#' pdr_optimize(tm, m, n, m_prec, ap_prec, 0.5, 0.27, params_to_optimize = "k", other_params = op)
pdr_optimize <- function(time, m, n, m_prec, ap_prec,
                         P,
                         k,
                         params_to_optimize = c("P", "k"),
                         pool = "CH4",
                         frac_P = NULL,
                         frac_k = NULL,
                         other_params = list(),
                         cost_fn = cost_function,
                         prediction_fn = ap_prediction,
                         include_progress = FALSE,
                         quiet = FALSE) {

  if(is.null(frac_P)) {
    if(!quiet) message("No frac_P provided; looking up from pdr_fractionation table")
    frac_P <- frac_P_default(pool)
  }
  if(is.null(frac_k)) {
    if(!quiet) message("No frac_k provided; looking up from pdr_fractionation table")
    frac_k <- frac_k_default(pool)
  }

  # Set defaults if not given by user
  other_params <- set_default_params(other_params)

  # Create a closure for logging progress
  log_msgs <- list()
  step <- 1
  plog <- function(x) {
    log_msgs[[step]] <<- as.data.frame(x)
    step <<- step + 1
  }

  # Estimate k starting value if not given
  if(missing(k)) {
    k <- pdr_estimate_k0(time, n, frac_k, quiet = quiet)
  }

  # Create the optim's 'par' vector that controls which parameters to optimize
  all_params <- c("P" = P, "k" = k, "frac_P" = frac_P, "frac_k" = frac_k)
  if(any(!params_to_optimize %in% names(all_params))) {
    stop("params_to_optimize must be P, k, frac_P, and/or frac_k")
  }
  params <- all_params[params_to_optimize]

  # Make sure 'lower' and 'upper' only have the variables being optimized;
  # otherwise bad things happen inside of optim()
  other_params$lower <- other_params$lower[intersect(names(params),
                                                     names(other_params$lower))]
  other_params$upper <- other_params$upper[intersect(names(params),
                                                     names(other_params$upper))]

  # Call optim()
  out <- optim(par = params,
               fn = cost_fn,
               method = other_params[["method"]],
               lower = other_params[["lower"]],
               upper = other_params[["upper"]],
               control = other_params[["control"]],

               # "..." that the optimizer will pass to cost_fn:
               time = time,
               m = m,
               n = n,
               m_prec = m_prec,
               ap_prec = ap_prec,
               P = P,
               k = k,
               frac_P = frac_P,
               frac_k = frac_k,
               log_progress = plog)

  out$initial_par <- all_params
  out$initial_other <- other_params
  if(include_progress) {
    out$progress <- do.call(rbind, c(log_msgs, make.row.names = FALSE))
  }
  out
}


# Set default parameters for optimization
#
# @param other_params A named list of parameters
#
# @return The \code{params} list with entries filled in as needed.
set_default_params <- function(other_params) {

  # If method not given, set default
  if(is.null(other_params[["method"]])) {
    other_params[["method"]] <- "L-BFGS-B"
  }
  # If bounds not given, provide some so that the optimizer
  # isn't allowed to produce <0 values for P, nor <=0 for k
  if(is.null(other_params[["lower"]])) {
    other_params[["lower"]] <- c("P" = 0.0, "k"= 0.0001, "frac_P" = 0, "frac_k" = 0)
  }
  if(is.null(other_params[["upper"]])) {
    other_params[["upper"]] <- c("P" = Inf, "k"= Inf, "frac_P" = 1, "frac_k" = 1)
  }
  # If control not given, use an empty list
  if(is.null(other_params[["control"]])) {
    other_params[["control"]] <- list()
  }
  other_params
}


#' Estimate initial k from heavy isotope concentration data
#'
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param frac_k Fractionation: 13C consumption as a fraction of 12C consumption
#' @param quiet Suppress output message, logical
#'
#' @importFrom stats lm
#' @return Initial estimate of k0 (consumption rate constant)
#' @export
#'
#' @examples
#' pdr_estimate_k0(1:5, c(1, 0.9, 0.7, 0.65, 0.4), frac_k = 0.98)
pdr_estimate_k0 <- function(time, n, frac_k, quiet = FALSE) {
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
  if(!quiet) message("Estimated k0 = ", round(k0, 3), " from n_slope = ", round(n_slope, 3))

  k0
}
