library(testthat)
library(rlang)
library(hash)

setGeneric(name = "label_active_communities", def = function(object) standardGeneric("label_active_communities"))
setGeneric("local_parameters",  function(object) standardGeneric("local_parameters"))
setGeneric("global_parameters", function(object) standardGeneric("global_parameters"))


setClass("CommunityAssignment", slots = c(state = "environment", update_state = "list"))
setClass("ParametricSpace", slots = c(name_parameter = "character", parameter_is_global = "logical", type_parameter = "character"))


# load source file for class CommunityAssignment
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Parameter.R")

setGeneric(name = "number_communities", def = function(object) standardGeneric("number_communities"))
setMethod("number_communities", "CommunityAssignment", function(object) object@state$number_communities)
setMethod("label_active_communities", "CommunityAssignment", function(object) object@state$label_active_communities)

setMethod("local_parameters",  "ParametricSpace", function(object) object@name_parameter[which(object@parameter_is_global == FALSE)])
setMethod("global_parameters", "ParametricSpace", function(object) object@name_parameter[which(object@parameter_is_global == TRUE)])
setGeneric("type_parameter", function(object, which_parameter) standardGeneric("type_parameter"))
setMethod("type_parameter", "ParametricSpace", function(object, which_parameter) object@type_parameter[[which(object@name_parameter == which_parameter)]])






message("test successful initialization:")
test_that("test successful initialization", {
  community_assignment <- new("CommunityAssignment", state = rlang::new_environment(list(number_communities = 3L, label_active_communities = c(1L, 3L, 4L))), update_state = list())
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(FALSE, FALSE, TRUE), type_parameter = c("double", "double", "integer")) 
  parameter <- Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = c(`1`= 1,`3`= 1,`4`=1), overlap = 6L))
  
  expect_s4_class(object = parameter, class = "Parameter")
  
  expect_true(is.integer(parameter@state$global_parameters$overlap))
  expect_true(is.double(parameter@state$local_parameters[["1"]]$strength))
  expect_true(is.double(parameter@state$local_parameters[["1"]]$discount))
  
  expect_equal(names(create_empty_parameter(community_assignment, parametric_space)), c("strength", "discount", "overlap"))
  expect_equal(unname(unlist(lapply(create_empty_parameter(community_assignment, parametric_space), typeof))), c("double", "double", "integer"))
  
  expect_error(Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = c(`1`= -.8,`3`= 1,`4`=1), overlap = 6L)))
  expect_error(Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = c(`1`= 1,         `4`=1), overlap = 6L)))
  expect_error(Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = c(`1`= 1,  `2`= 1,`4`=1), overlap = 6L)))
  expect_error(Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), overlap = 6L)))
  
  expect_equal(Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = c(`1`= 1,`3`= 1,`4`=1), 
                 overlap = 6.5))@state$global_parameters$overlap, 6L)
})

message("test read only methods:")
test_that("test read only methods", {
  community_assignment <- new("CommunityAssignment", state = rlang::new_environment(list(number_communities = 3L, label_active_communities = c(1L, 3L, 4L))), update_state = list())
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(FALSE, TRUE, TRUE), type_parameter = c("double", "double", "integer"))
  parameter <- Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = 1, overlap = 6L))
  
  expect_equal(sapply(c(1L, 3L, 4L), function(x) strength(parameter, as.character(x))), c(.7, .8, .9))
  expect_equal(discount(parameter), 1)
  expect_type(sapply(c(1L, 3L, 4L), function(x) strength(parameter, as.character(x))), "double")
  expect_type(discount(parameter), "double")
  
  expect_equal(overlap(parameter), 6L)
  expect_type(overlap(parameter), "integer")
})

message("test updates:")
test_that("test updates", {
  community_assignment <- new("CommunityAssignment", state = rlang::new_environment(list(number_communities = 3L, label_active_communities = c(1L, 3L, 4L))), update_state = list())
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(FALSE, TRUE, TRUE), type_parameter = c("double", "double", "integer"))
  parameter <- Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = 1, overlap = 6L))
  
  expect_equal({set_strength(parameter, .9, "3"); strength(parameter, "3")}, .9)
  expect_error(set_strength(parameter, -.1, "3")) # strength can't be negative
  expect_error(set_strength(parameter, .9)) # strength is local but no label is given
  expect_error(set_strength(parameter, .9, "2")) # community "2" is not active
  
  expect_equal({set_discount(parameter, -.05, "3"); discount(parameter, "3")}, -.05)
  expect_error(set_discount(parameter, -.8)) # discount too low
  
  expect_equal({set_overlap(parameter, 10L); overlap(parameter)}, 10L)
  expect_error(set_overlap(parameter, -1L)) # overlap too low
  
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(FALSE, FALSE, TRUE), type_parameter = c("double", "double", "integer"))
  parameter <- Parameter(community_assignment, parametric_space, list(strength = c(`1`= .7,`3`= .8,`4`= .9), discount = c(`1`= 0,`3`= -.5,`4`= 1), overlap = 6L))
  
  expect_error(set_strength(parameter, .4, "3")) # must be:  discount > - strength
  
  expect_equal({set_discount(parameter, .5, "1"); discount(parameter, "1")}, .5)
  expect_error(set_discount(parameter, .5)) # community not given
  
})

message("test names parameters and active communities")
test_that("test names parameters and active communities", {
  community_assignment <- new("CommunityAssignment", state = rlang::new_environment(list(number_communities = 3L, label_active_communities = c(1L, 3L, 4L))), update_state = list())
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(FALSE, FALSE, TRUE), type_parameter = c("double", "double", "integer")) 
  parameter <- Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = c(`1`= 1,`3`= 1,`4`=1), overlap = 6L))
  
  expect_equal(local_parameters(parameter), local_parameters(parametric_space))
  expect_equal(global_parameters(parameter), global_parameters(parametric_space))
  expect_equal(label_active_communities(parameter), as.character(label_active_communities(community_assignment)))
  
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(FALSE, TRUE, TRUE), type_parameter = c("double", "double", "integer")) 
  parameter <- Parameter(community_assignment, parametric_space, list(strength = c(`1`=.7,`3`=.8,`4`=.9), discount = 1, overlap = 6L))
  
  expect_equal(local_parameters(parameter), local_parameters(parametric_space))
  expect_equal(global_parameters(parameter), global_parameters(parametric_space))
  expect_equal(label_active_communities(parameter), as.character(label_active_communities(community_assignment)))
  
  parametric_space <- new("ParametricSpace", name_parameter = c("strength", "discount", "overlap"), parameter_is_global = c(TRUE, TRUE, TRUE), type_parameter = c("double", "double", "integer")) 
  parameter <- Parameter(community_assignment, parametric_space, list(strength = .8, discount = 1, overlap = 6L))
  
  expect_equal(global_parameters(parameter), global_parameters(parametric_space))
  expect_equal(label_active_communities(parameter), as.character(label_active_communities(community_assignment)))
})

