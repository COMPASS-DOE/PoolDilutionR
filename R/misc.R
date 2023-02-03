utils::globalVariables("pdr_fractionation")

#' Retrieve default P fractionation value for a pool
#'
#' @param pool Name of pool, character
#'
#' @return The default entry for \code{pool} listed
#' in \code{\link{pdr_fractionation}}.
#' @export
#'
#' @examples
#' frac_P_default("CH4")
frac_P_default <- function(pool) {
  if (!pool %in% pdr_fractionation$Pool) {
    stop("This pool is not listed in pdr_fractionation")
  }
  pdr_fractionation[
    pdr_fractionation$Pool == pool &
      pdr_fractionation$Default,
    "frac_P"
  ]
}


#' Retrieve default k fractionation value for a pool
#'
#' @param pool Name of pool, character
#'
#' @return The default entry for \code{pool} listed
#' in \code{\link{pdr_fractionation}}.
#' @export
#'
#' @examples
#' frac_k_default("CH4")
frac_k_default <- function(pool) {
  if (!pool %in% pdr_fractionation$Pool) {
    stop("This pool is not listed in pdr_fractionation")
  }
  pdr_fractionation[
    pdr_fractionation$Pool == pool &
      pdr_fractionation$Default,
    "frac_k"
  ]
}
