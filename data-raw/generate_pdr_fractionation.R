# Since this script will only be run occasionally, use
# devtools::load_all() instead of loading the built package.
devtools::load_all()

# Read in the input parameter csv file
pdr_fractionation <- read.csv("./data-raw/pdr_fractionation.csv")

# Save as an object in the package
usethis::use_data(pdr_fractionation, internal = FALSE, overwrite = TRUE)
