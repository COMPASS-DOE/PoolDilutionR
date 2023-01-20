## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(PoolDilutionR)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(tidyr)
library(kableExtra)

## ----Example Data, echo = FALSE, message = TRUE-------------------------------
facet_labels <- c(
    AP_obs = "Atom% Observed",
    cal12CH4ml = "12C Methane (mL)",
    cal13CH4ml = "13C Methane (mL)")

Morris2023 %>%
    mutate(id_numeric = as.factor(id)) %>%
    pivot_longer(cols = c(cal12CH4ml, cal13CH4ml, AP_obs)) %>%
    ggplot(aes(round, value, group = id, color = id)) +
    scale_color_discrete("Sample ID") +
    geom_point() + geom_line() + ylab("") + xlab("\n Timestep") +
    ggtitle("Example Input Data") +
    facet_wrap( ~ name, scales = "free",
                labeller = labeller(name = facet_labels)) -> p

