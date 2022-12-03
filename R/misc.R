
#' Retrieve default P fractionation value for a gas
#'
#' @param gas Name of gas, character
#'
#' @return The default entry for \code{gas} listed
#' in \code{\link{pdr_fractionation}}.
#' @export
#'
#' @examples
#' P_default("CH4")
P_default <- function(gas) {
  if(!gas %in% pdr_fractionation$Gas) {
    stop("This gas is not listed in pdr_fractionation")
  }
  pdr_fractionation[pdr_fractionation$Gas == gas,
                    pdr_fractionation$Default,
                    "frac_P"]
}


#' Retrieve default k fractionation value for a gas
#'
#' @param gas Name of gas, character
#'
#' @return The default entry for \code{gas} listed
#' in \code{\link{pdr_fractionation}}.
#' @export
#'
#' @examples
#' k_default("CH4")
k_default <- function(gas) {
  if(!gas %in% pdr_fractionation$Gas) {
    stop("This gas is not listed in pdr_fractionation")
  }
  pdr_fractionation[pdr_fractionation$Gas == gas,
                    pdr_fractionation$Default,
                    "frac_k"]
}
