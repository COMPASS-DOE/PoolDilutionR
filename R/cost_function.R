
#' Cost Function
#'
#' @param params Named vector holding optimizer-assigned values for P and k
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param m_prec Instrument precision for pool size, expressed as a standard deviation
#' @param ap_prec Instrument precision for atom percent, expressed as a standard deviation
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#' @param log_progress An optional logging function
#'
#' @importFrom stats sd
#' @return Returns the sum of squares between predicted and observed m and AP
#' @export
#'
#' @note This is Eq. 14 from vFH2002 with a few modifications…
#' @author K.A. Morris & B. Bond-Lamberty
#' @examples
#' m <- c(10, 8, 6, 5, 4, 3)
#' n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
#' cost_function(params = list(P = 0.5, k = 0.3), time = 0:5, m, n, m_prec = 0.001, ap_prec = 1)
cost_function <- function(params, time, m, n, m_prec, ap_prec,
                          pool = "CH4",
                          frac_P = P_default(pool),
                          frac_k = k_default(pool),
                          log_progress = NULL) {
  #message(params["P"], ",", params["k"])
  pred <- ap_prediction(time = time,
                        m0 = m[1],
                        n0 = n[1],
                        P = params[["P"]],
                        k = params[["k"]],
                        frac_P = frac_P,
                        frac_k = frac_k)

  pool_weight = sd(m) / m_prec # Normalization factor for pool size, see Eq. 12
  ap_weight = sd((n / n + m) * 100) / ap_prec # Normalization factor for isotopic signature, see Eq. 13

  #vFH eq 14
  cost <- sum((abs(m - pred$mt) / sd(m)) * pool_weight + (abs(n - pred$nt) / sd(n)) * ap_weight)
  # Log progress and return to optimizer
  if(!is.null(log_progress)) {
    log_progress(data.frame(P = params[["P"]], k = params[["k"]], cost = cost))
  }
  cost
}
