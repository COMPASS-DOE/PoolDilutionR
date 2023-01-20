# Since this script will only be run occasionally, use
# devtools::load_all() instead of loading the built package.
devtools::load_all()

# Read in the input parameter csv file
Morris2023 <- read.csv("./data-raw/Morris2023.csv")

# Save as an object in the package
usethis::use_data(Morris2023, internal = FALSE, overwrite = TRUE)
