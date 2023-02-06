#' Cost function between observed and predicted pools
#'
#' @param params Named list holding optimizer-assigned values for parameters
#' @param time Vector of numeric time values; first should be zero
#' @param m Observed total pool size, same length as time
#' @param n Observed pool size of heavy isotope, same length as time
#' @param m_prec Instrument precision for pool size, expressed as a standard deviation
#' @param ap_prec Instrument precision for atom percent, expressed as a standard deviation
#' @param P production rate, unit pool size/unit time
#' @param k first-order rate constant for consumption, 1/unit time
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param frac_P Fractionation value for production; see \code{\link{pdr_fractionation}}
#' @param frac_k Fractionation value for consumption; see \code{\link{pdr_fractionation}}
#' @param log_progress An optional logging function
#'
#' @importFrom stats sd
#' @return Returns a cost metric summarizing the difference between the
#' predicted and observed \code{m} (total pool size) and \code{AP} (atom percent).
#' @export
#'
#' @note This implements Equations 12-14 from von Fischer and Hedin (2002).
#' @author K.A. Morris & B. Bond-Lamberty
#' @examples
#' m <- c(10, 8, 6, 5, 4, 3)
#' n <- c(1, 0.7, 0.6, 0.4, 0.3, 0.2)
#' pdr_cost(params = list(P = 0.5, k = 0.3), time = 0:5, m, n, m_prec = 0.001, ap_prec = 0.01)
pdr_cost <- function(params, # values are set by optim()
                     time, m, n, m_prec, ap_prec,
                     P,
                     k,
                     pool = "CH4",
                     frac_P = frac_P_default(pool),
                     frac_k = frac_k_default(pool),
                     log_progress = NULL) {
  # If the optimizer passes in values for P/k/frac_P/frac_k, they
  # override the defaults (named parameters)
  if ("P" %in% names(params)) P <- params[["P"]]
  if ("k" %in% names(params)) k <- params[["k"]]
  if ("frac_P" %in% names(params)) frac_P <- params[["frac_P"]]
  if ("frac_k" %in% names(params)) frac_k <- params[["frac_k"]]

  pred <- pdr_predict(
    time = time,
    m0 = m[1],
    n0 = n[1],
    P = P,
    k = k,
    frac_P = frac_P,
    frac_k = frac_k
  )

  ap_obs <- (n / m) * 100
  pool_weight <- sd(m) / m_prec # Normalization factor for pool size, see Eq. 12
  ap_weight <- sd(ap_obs) / ap_prec # Normalization factor for isotopic signature, see Eq. 13

  # von Fischer and Hedin (2002) equation 14
  cost <- sum(abs(ap_obs - pred$AP_pred) / sd(ap_obs)) * ap_weight +
    sum(abs(m - pred$mt) / sd(m)) * pool_weight

  # Log progress and return to optimizer
  if (!is.null(log_progress)) {
    log_progress(data.frame(P = P, k = k, frac_P = frac_P, frac_k = frac_k, cost = cost))
  }

  cost
}
