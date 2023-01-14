library(testthat)

source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Parametric_Space.R")

setClass("Model", slots = c(model_for_networks = "character", model_for_union = "character"))

setGeneric("model_for_networks", function(object) {"model_for_networks"})
setMethod("model_for_networks", "Model", function(object) object@model_for_networks)
setGeneric("model_for_union", function(object) {"model_for_union"})
setMethod("model_for_union", "Model", function(object) object@model_for_union)



message("test successful initialization:")
test_that("test successful initialization", {
  model <- new("Model", model_for_networks = "PitmannYor", model_for_union = "uniform")
  expect_s4_class(ParametricSpace(model), class = "ParametricSpace")
  
  expect_error(ParametricSpace(c(1,2,3), FALSE, TRUE))
  expect_error(ParametricSpace(model, NA, TRUE))
  expect_error(ParametricSpace(model, 1L, TRUE))
  expect_error(ParametricSpace(model, logical(0), TRUE))
  expect_error(ParametricSpace(model, c(T, T), TRUE))
  expect_error(ParametricSpace(model, TRUE, NA))
})

message("test read only functions:")
test_that("test read only functions", {
  model <- new("Model", model_for_networks = "PitmannYor", model_for_union = "uniform")
  
  expect_equal(global_parameters(ParametricSpace(model)), "overlap")
  expect_equal(local_parameters(ParametricSpace(model)), c("strength", "discount"))
  expect_equal(global_parameters(ParametricSpace(model,T)), c("strength", "overlap"))
  expect_equal(local_parameters(ParametricSpace(model,T)), "discount")
  expect_equal(global_parameters(ParametricSpace(model,F,T)), c("discount", "overlap"))
  expect_equal(local_parameters(ParametricSpace(model,F,T)), "strength")
  expect_equal(global_parameters(ParametricSpace(model,T,T)), c("strength", "discount", "overlap"))
  expect_length(local_parameters(ParametricSpace(model,T,T)), 0L)
  
  expect_equal(type_parameter(ParametricSpace(model), "strength"), "double")
  expect_equal(type_parameter(ParametricSpace(model), "discount"), "double")
  expect_equal(type_parameter(ParametricSpace(model), "overlap"), "integer")
  expect_error(type_parameter(ParametricSpace(model), "rlap"))
})
