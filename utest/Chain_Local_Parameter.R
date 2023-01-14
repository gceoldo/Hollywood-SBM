library(testthat)

source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Chain_Local_Parameter.R")

message("initialization")
test_that("initialization", {
  expect_s4_class(ChainParameter(5), "ChainParameter")
  expect_error(ChainParameter())
  expect_error(ChainParameter(-1))
})

message("store parameter")
test_that("update state", {
  length_chain <- 10
  parameters_to_be_stored <- rnorm(length_chain)
  clp <- ChainParameter(length_chain)
  for(i in 1:length_chain) store_parameter(clp, parameters_to_be_stored[i])
  expect_equal(clp@state$chain, parameters_to_be_stored)
}) 

message("store parameter")
test_that("update state", {
  start_parameter <- 3
  parameters_to_be_stored <- c(4,3,2,2,3,4,3,4,4,4,4,2,2,3,1,1,3,3,4,5,2,3,4,4,3)
  clp <- ChainParameter(length(parameters_to_be_stored))
  for(i in 1:length(parameters_to_be_stored)) store_parameter(clp, parameters_to_be_stored[i])
  expect_true( is.na(proposed_is_accepted(clp)[1]))
  expect_false(is.na(proposed_is_accepted(clp, 2)[1]))
  expect_equal(proposed_is_accepted(clp)[-1], abs(diff(parameters_to_be_stored)) > 0)
  expect_equal(proposed_is_accepted(clp, start_parameter), c(T,abs(diff(parameters_to_be_stored)) > 0))
  expect_equal(proposed_is_accepted(clp, parameters_to_be_stored[1]), c(F,abs(diff(parameters_to_be_stored)) > 0))
}) 

