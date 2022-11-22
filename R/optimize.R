
#' Optimize P (production) and k (consumption) for a pool dilution experiment
#'
#' @param time Time vector of time values, numeric (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param Nm Normalization factor for pool size, see Eq. 12
#' @param Nd Normalization factor for isotopic signature, see Eq. 13
#' @param params Parameters TODO
#' @param cost_fn Cost function to use, passed to \code{\link{optim}}
#' @param prediction_fn Prediction function that the cost function will use
#' @param control Optional list of control parameters; see \code{\link{optim}}
#'
#' @return The output of \code{\link{optim}}.
#' @export
#'
#' @examples
optimize_pk <- function(time, m, n, Nm, Nd,
                        params,
                        cost_fn = cost_function,
                        prediction_fn = ap_prediction,
                        control = list()) {

  # If P0 not given, guess
  if(is.null(params[["P0"]])) {

  }
  # If k0 not given, guess
  if(is.null(params[["k0"]])) {

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

  optim(par = c("P" = params[["P0"]], "k"= params[["k0"]]),
        fn = cost_fn,
        method = params[["method"]],
        lower = params[["lower"]],
        upper = params[["upper"]],
        control = control,

        # "..." that the optimizer will pass to cost_fn:
        time = time,
        m = m,
        n = n,
        Nm = Nm,
        Nd = Nd)
}
