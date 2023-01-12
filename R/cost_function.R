
#' Cost Function
#'
#' @param params Named list holding optimizer-assigned values for parameters
#' @param time Vector of numeric time values (e.g. days); first should be zero
#' @param m Observed pool size (as a volume), same length as time
#' @param n Observed heavy isotope (as a volume), same length as time
#' @param Nm Normalization factor for pool size, see Eq. 12
#' @param Nd Normalization factor for isotopic signature, see Eq. 13
#' @param pool Name of pool; see \code{\link{pdr_fractionation}}
#' @param P production rate, unit gas/unit time
#' @param k first-order rate constant for consumption, 1/unit time
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
#' Nm = m / 10
#' Nd = n / 10
#' cost_function(params = list(P = 0.5, k = 0.3), time = 0:5, m, n, Nm, Nd)
cost_function <- function(params, # values are set by optim()
                          time, m, n, Nm, Nd,
                          pool = "CH4",
                          P,
                          k,
                          frac_P = P_default(pool),
                          frac_k = k_default(pool),
                          log_progress = NULL) {

  # If the optimizer passes in values for P/k/frac_P/frac_k, they
  # override the defaults (named parameters)
  if("P" %in% names(params)) P <- params[["P"]]
  if("k" %in% names(params)) k <- params[["k"]]
  if("frac_P" %in% names(params)) frac_P <- params[["frac_P"]]
  if("frac_k" %in% names(params)) frac_k <- params[["frac_k"]]

  pred <- ap_prediction(time = time,
                        m0 = m[1],
                        n0 = n[1],
                        P = P,
                        k = k,
                        frac_P = frac_P,
                        frac_k = frac_k)

  # von Fischer and Hedin (2002) equation 14
  cost <- sum((abs(m - pred$mt) / sd(m)) * Nm + (abs(n - pred$nt) / sd(n)) * Nd)
  # Log progress and return to optimizer
  if(!is.null(log_progress)) {
    log_progress(data.frame(P = P, k = k, frac_P = frac_P, frac_k = frac_k, cost = cost))
  }
  if(!is.finite(cost)) {
    cat("whoops")
  }
  cost
}
