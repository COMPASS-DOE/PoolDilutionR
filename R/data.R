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

#' Example time series data from a methane dilution pool experiment.
#'
#' Sequential measurements of methane concentration and isotopic signature were taken using a Picarro G2920 with a Small Sample Introduction module.
#' This instrument provides gas concentrations in ppm and signatures in delta-13C, here we provide those data converted into volume of methane and atom percent.
#'
#' @format
#' \describe{
#'   \item{id}{Sample ID, a factor}
#'   \item{time_days}{time in days between measurements, starting at 0}
#'   \item{cal12CH4ml}{ml of 12C-CH4 at each timestep}
#'   \item{cal13CH4ml}{ml of 13C-CH4 at each timestep}
#'   \item{AP_obs}{atom percent 13C-CH4 at each timestep}
#' }
"Morris2023"
