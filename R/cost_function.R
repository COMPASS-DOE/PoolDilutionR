
#' Cost Function
#'
#' @param params named vector holding optimizer-assigned values for P and k
#' @param time vector of time values, numeric (e.g. days); first should be zero
#' @param m observed pool size (as a volume), same length as time
#' @param n observed heavy isotope (as a volume), same length as time
#' @param Nm normalization factor for pool size, see Eq. 12
#' @param Nd normalization factor for isotopic signature, see Eq. 13
#' @param gas Name of gas; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#' @param log_progress An optional logging function
#'
#' @return Returns the sum of squares between predicted and observed m and AP
#' @export
#'
#' @note This is Eq. 14 from vFH2002 with a few modifications…
#' @author K.A. Morris & B. B-L
#' @examples
cost_function <- function(params, time, m, n, Nm, Nd,
                          gas = "CH4",
                          frac_P = P_default(gas),
                          frac_k = k_default(gas),
                          log_progress = NULL) {
  #message(params["P"], ",", params["k"])
  pred <- ap_prediction(time = time,
                        m0 = m[1],
                        n0 = n[1],
                        P = params["P"],
                        k = params["k"])

  #vFH eq 14
  cost <- sum((abs(m - pred$mt) / sd(m)) * Nm + (abs(n - pred$nt) / sd(n)) * Nd)
  # Log progress and return to optimizer
  if(!is.null(log_progress)) {
    log_progress(data.frame(P = params["P"], k = params["k"], cost = cost))
  }
  cost
}
