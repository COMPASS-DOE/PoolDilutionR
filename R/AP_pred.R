
#' Atom Percent Prediction
#'
#' @param time vector of time values, numeric (e.g. days); first should be zero
#' @param m0 pool size at time zero, as a volume
#' @param n0 amount of heavy isotope at time zero, as a volume
#' @param P production rate, unit gas/unit time
#' @param k first-order rate constant for consumption, 1/unit time
#'
#' @return Returns a data frame with mt, nt, and AP (atom percent) predictions for each time step
#' @export
#'
#' @note All combined, this is Eq. 11 from vFH2002 with a few modifications…
#' @author K.A. Morris & B. B-L
#' @examples
ap_prediction <- function(time, m0, n0, P, k) {
  kfrac <- k * FRAC_K
  pfrac <- P * FRAC_P
  nt <- pfrac / kfrac - (pfrac / kfrac - n0) * exp(-kfrac * time)
  # Equation 5 (and denominator in Eq. 11):
  mt <- P / k - (P / k - m0) * exp(-k * time)
  ap <- nt / mt * 100

  data.frame(mt = mt, nt = nt, AP_pred =  ap)
}
