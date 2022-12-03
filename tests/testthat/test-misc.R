
test_that("Default P and k functions work", {

  expect_error(P_default("bad gas"), "gas is not listed")
  expect_error(k_default("bad gas"), "gas is not listed")

  p <- P_default("CH4")
  expect_type(p, "double")
  expect_length(p, 1L)

  k <- k_default("CH4")
  expect_type(k, "double")
  expect_length(k, 1L)
})

test_that("pdr_fractionation table is valid", {

  # There should be a maximum of one default value for each gas
  for(g in unique(pdr_fractionation$Gas)) {
    pdr_f <- subset(pdr_fractionation, pdr_fractionation$Gas == g)
    expect_lt(sum(pdr_f$Default), 2)
  }

})
