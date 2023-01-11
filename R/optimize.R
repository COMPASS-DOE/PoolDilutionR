
#' Optimize P (production) and k (consumption) for a pool dilution experiment
#'
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param Nm Normalization factor for pool size, see Eq. 12
#' @param Nd Normalization factor for isotopic signature, see Eq. 13
#' @param P pee
#' @param k kay
#' @param params_to_optimize Named vector of parameters ("P", "k", "frac_P",
#' and/or "frac_k") to optimize against observations
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#' @param other_params Other parameters pass on to \code{\link{optim}}
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
#' pdr_optimize(time = 0:5, m, n, Nm, Nd, P = 0.5)
pdr_optimize <- function(time, m, n, Nm, Nd,
                         P,
                         k,
                         params_to_optimize = c("P", "k"),
                         pool = "CH4",
                         frac_P = P_default(pool),
                         frac_k = k_default(pool),
                         other_params = list(),
                         cost_fn = cost_function,
                         prediction_fn = ap_prediction) {

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
    k <- estimate_k0(time, n, frac_k)
  }

  # Create the optim's 'par' vector that controls which parameters to optimize
  params <- rep(NA_real_, length(params_to_optimize))
  all_params <- c("P" = P, "k" = k, "frac_P" = frac_P, "frac_k" = frac_k)
  if(any(!params_to_optimize %in% names(all_params))) {
    stop("`params_to_optimize` must be P, k, frac_P, and/or frac_k")
  }
  params <- all_params[params_to_optimize]

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
               Nm = Nm,
               Nd = Nd,
               frac_P = frac_P,
               frac_k = frac_k,
               log_progress = plog)

  out$initial_par <- params
  out$progress <- do.call(rbind, c(log_msgs, make.row.names = FALSE))
  out
}


#' Set default parameters for optimization
#'
#' @param other_params A named list of parameters
#'
#' @return The \code{params} list with entries filled in as needed.
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
  message("Estimated k0 = ", round(k0, 3), " from n_slope = ", round(n_slope, 3))
  k0
}
