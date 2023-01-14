library(testthat)

source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Model.R")

message("test successful/unsuccesful initialization:")
test_that("test successful/unsuccesful initialization", {
  expect_is(Model(), "Model")
  
  # check default values:
  expect_equal(c(model_for_networks(Model()),model_for_union(Model())),c("PitmannYor", "uniform"))
  
  # check wrong input
  expect_error(Model("adfsd"))
  expect_error(Model("PitmannYor", "dfgs"))
})  
