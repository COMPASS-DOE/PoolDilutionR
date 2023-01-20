#' P and k fractionation values
#'
#' A compendium of possible production (P) and consumption (k) fractionation
#' values, by pool.
#'
#' @format
#' \describe{
#'   \item{Pool}{Name of pool (gas or solid)}
#'   \item{frac_P}{Fractionation value of production (P)}
#'   \item{frac_k}{Fractionation value of consumption (k)}
#'   \item{Default}{Default for this pool? Logical}
#'   \item{Source}{Source paper or URL}
#' }
"pdr_fractionation"

#' Example isotopic time series data from a methane pool experiment.
#'
#' Add description.
#'
#' @format
#' \describe{
#'   \item{id}{Name of pool (gas or solid)}
#'   \item{time_days}{Fractionation value of production (P)}
#'   \item{cal12CH4ml}{Fractionation value of consumption (k)}
#'   \item{cal13CH4ml}{Default for this pool? Logical}
#'   \item{AP_obs}{Source paper or URL}
#' }
"Morris2023"
