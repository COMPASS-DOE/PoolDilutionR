
test_that("Default P and k functions work", {

  expect_error(frac_P_default("bad pool"), "pool is not listed")
  expect_error(frac_k_default("bad pool"), "pool is not listed")

  p <- frac_P_default("CH4")
  expect_type(p, "double")
  expect_length(p, 1L)

  k <- frac_k_default("CH4")
  expect_type(k, "double")
  expect_length(k, 1L)
})

test_that("pdr_fractionation table is valid", {

  # There should be a maximum of one default value for each pool
  for(p in unique(pdr_fractionation$Pool)) {
    pdr_f <- subset(pdr_fractionation, pdr_fractionation$Pool == p)
    expect_lt(sum(pdr_f$Default), 2)
  }

})
