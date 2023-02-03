#' Predict total pool, heavy isotope pool, and atom percent
#'
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param m0 total pool size at time zero, as a volume
#' @param n0 pool size of heavy isotope at time zero, as a volume
#' @param P production rate, unit gas/unit time
#' @param k first-order rate constant for consumption, 1/unit time
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#'
#' @return Returns a data frame with \code{mt}, \code{nt}, and \code{AP_pred} (atom percent) for each time step
#' @export
#'
#' @note This is Eq. 11 from von Fischer and Hedin 2002 with a few modifications.
#' @author K.A. Morris & B. Bond-Lamberty
#' @examples
#' pdr_predict(time = 0:5, m0 = 10, n0 = 1, P = 0.5, k = 0.3)
pdr_predict <- function(time, m0, n0, P, k,
                        pool = "CH4",
                        frac_P = frac_P_default(pool),
                        frac_k = frac_k_default(pool)) {
  if (!is.numeric(time)) stop("Time must be numeric.")
  if (!all(diff(time) > 0)) stop("Time values must increase.")
  if (time[1] != 0.0) stop("First time value must be zero.")

  kfrac <- k * frac_k
  pfrac <- P * frac_P
  nt <- pfrac / kfrac - (pfrac / kfrac - n0) * exp(-kfrac * time)

  # Equation 5 (and denominator in Eq. 11 of von Fischer and Hedin 2002):
  mt <- P / k - (P / k - m0) * exp(-k * time)
  ap <- (nt / mt) * 100
  # If any nt values are zero, we want the corresponding atom percent values
  # also to be zero, not Inf or Nan
  ap[nt == 0] <- 0.0

  data.frame(mt = mt, nt = nt, AP_pred = ap)
}
