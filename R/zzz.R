
.onAttach <- function(...) {
  if(interactive() && stats::runif(1) < 0.2) {
    packageStartupMessage("Citation: Morris et al. (2023) xxxxx")
  }
}
