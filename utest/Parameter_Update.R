library(testthat)

setGeneric("local_parameters",  function(object) standardGeneric("local_parameters"))
setGeneric("global_parameters", function(object) standardGeneric("global_parameters"))
setGeneric("size_population", function(object, label_community) standardGeneric("size_population"))
setGeneric("size_largest_population", function(object) standardGeneric("size_largest_population"))
setGeneric("label_active_communities", function(object) standardGeneric("label_active_communities"))
setGeneric("total_arity", function(object, label_community) standardGeneric("total_arity"))

setClass("Network", slots = c(individual_statistics = "list", global_statistics = "list"))
setClass("Multinetwork", slots = c(state = "environment", update_state = "list"))
setClass("Parameter", slots = c(state = "environment", update_state = "list", name_local_parameters  = "character"))

source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Parameter_Update.R")
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/generative_model.R")

create_hashmultiset <- function(initial_sequence) {
  hashmultiset <- hash::hash()
  hashmultiset[sort(unique(initial_sequence))] <- as.integer(table(initial_sequence))
  return(hashmultiset)
}
resample <- function(x, ...) x[sample.int(length(x), ...)]
null_to_zero <- function(value) {
  if(!is.null(value)) return(value)
  else return(0L)
}
zero_to_null <- function(value) {
  if(value != 0L) return(value)
  else return(NULL)
}


message("test boundary flip")
test_that("test boundary flip", {
  expect_equal(boundary_reflection(.5, 0, 1), .5)
  expect_equal(boundary_reflection( 0, 0, 1),  0)
  expect_equal(boundary_reflection( 1, 0, 1),  1)
  
  expect_equal(boundary_reflection( 1, 0, Inf), 1)
  expect_equal(boundary_reflection(-1, 0, Inf), 1)
  
  expect_equal(boundary_reflection(-2.5, 0, 1), .5)
})

message("test proposal")
test_that("test proposal", {
  setMethod("size_population", "Network", function(object, label_community) object@global_statistics$size_population)
  setMethod("size_largest_population", "Multinetwork", function(object) object@state$global_statistics$size_largest_population)
  setGeneric("strength", function(object, label_community) standardGeneric("strength"))
  setMethod( "strength", "Parameter", function(object, label_community) {
    if("strength" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$strength)
    else return(object@state$global_parameters$strength)
  })
  setGeneric("discount", function(object, label_community) standardGeneric("discount"))
  setMethod( "discount", "Parameter", function(object, label_community) {
    if("discount" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$discount)
    else return(object@state$global_parameters$discount)
  })
  setGeneric("overlap", function(object) standardGeneric("overlap"))
  setMethod( "overlap", "Parameter", function(object) { return(object@state$global_parameters$overlap) })
  
  network <- new("Network")
  network@global_statistics$size_population <- 13L
  
  multinetwork <- new("Multinetwork")
  multinetwork@state <- rlang::new_environment()
  multinetwork@state$global_statistics$size_largest_population <- 9L
  
  parameter <- new("Parameter")
  parameter@state <- rlang::new_environment()
  parameter@state$local_parameters  <- hash::hash()
  parameter@state$global_parameters <- vector("list", 3)
  names(parameter@state$global_parameters) <- c("strength", "discount", "overlap")
  parameter@state$global_parameters$strength <- .2
  parameter@state$global_parameters$discount <- 0.1
  parameter@state$global_parameters$overlap <- 3L
  
  strength_samples <- replicate(100, set_defaultPU("proposal")$strength(parameter = parameter))
  expect_true(all(strength_samples < 1))
  expect_true(all(strength_samples > 0))
  
  discount_samples <- replicate(100, set_defaultPU("proposal")$discount(parameter = parameter))
  expect_true(all(discount_samples > (-parameter@state$global_parameters$strength)))
  
  overlap_samples <- replicate(100, set_defaultPU("proposal")$overlap(parameter, multinetwork, network))
  expect_true(all(overlap_samples >= (network@global_statistics$size_population - multinetwork@state$global_statistics$size_largest_population)))
})

message("test logratio")
test_that("test logratio", {
  setMethod("size_population", "Network", function(object, label_community) object@global_statistics$size_population)
  setMethod("size_population", "Multinetwork", function(object, label_community) object@state$statistics_communities[[as.character(label_community)]]$size_population)
  setGeneric("pair_degrees_with_frequencies", function(object, label_community) standardGeneric("pair_degrees_with_frequencies"))
  setMethod("pair_degrees_with_frequencies", "Multinetwork", function(object, label_community) {
    list(
      degrees = as.integer(hash::keys(  object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies)),
      degree_frequencies = hash::values(object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies)
    )
  })
  setMethod("size_largest_population", "Multinetwork", function(object) object@state$global_statistics$size_largest_population)
  setMethod("label_active_communities", "Multinetwork", function(object) { hash::keys(object@state$statistics_communities) })
  setMethod("total_arity", "Multinetwork", function(object, label_community) object@state$statistics_communities[[as.character(label_community)]]$total_arity)
  
  setGeneric("strength", function(object, label_community) standardGeneric("strength"))
  setMethod( "strength", "Parameter", function(object, label_community) {
    if("strength" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$strength)
    else return(object@state$global_parameters$strength)
  })
  setGeneric("discount", function(object, label_community) standardGeneric("discount"))
  setMethod( "discount", "Parameter", function(object, label_community) {
    if("discount" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$discount)
    else return(object@state$global_parameters$discount)
  })
  setGeneric("overlap", function(object) standardGeneric("overlap"))
  setMethod( "overlap", "Parameter", function(object) { return(object@state$global_parameters$overlap) })
  
  
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  } 
  compute_degrees_combining_degrees_individual_relations <- function(degrees_individual_relations) {
    degrees <- hash::hash()
    for(i in 1:length(degrees_individual_relations)) {
      hashmultiset_degrees_in_relation_i <- degrees_individual_relations[[i]]
      keys_relation_i <- hash::keys(hashmultiset_degrees_in_relation_i)
      for(j in seq_along(keys_relation_i)) {
        degrees[[keys_relation_i[j]]] <- null_to_zero(degrees[[keys_relation_i[j]]]) + hashmultiset_degrees_in_relation_i[[keys_relation_i[j]]] 
      }
    } 
    return(degrees)
  }

  multinetwork <- new("Multinetwork")
  multinetwork@state <- rlang::new_environment()
  multinetwork@state$statistics_communities <- hash::hash()
  multinetwork@state$global_statistics <- rlang::new_environment()
  
  lac <- c(2L,4L,5L)
  assignment <- rep(lac, 4)[sample.int(length(lac) * 4, length(lac) * 4)]
  assignment2 <- rep(NA_integer_, length(assignment))
  for(i in seq_along(lac)) assignment2[which(assignment == lac[i])] <- i
  set_relations <- sample_HBM(assignment2, .8, .1, 2L, function() 2L+rpois(1,3))
  rm(assignment2)
  
  multinetwork@state <- rlang::new_environment()
  multinetwork@state$statistics_communities <- hash::hash()
  multinetwork@state$global_statistics <- rlang::new_environment()
  
  maxpop <- 0L
  for(i in lac) {
    multinetwork@state$statistics_communities[[as.character(i)]] <- rlang::new_environment()
    
    multinetwork@state$statistics_communities[[as.character(i)]]$degrees <- compute_degrees_combining_degrees_individual_relations(lapply(set_relations[assignment == i], compute_hashmultiset_degrees_from_relation))
    multinetwork@state$statistics_communities[[as.character(i)]]$degree_frequencies <- create_hashmultiset(hash::values(multinetwork@state$statistics_communities[[as.character(i)]]$degrees))
    multinetwork@state$statistics_communities[[as.character(i)]]$size_population <- length(multinetwork@state$statistics_communities[[as.character(i)]]$degrees)
    
    multinetwork@state$statistics_communities[[as.character(i)]]$arity_frequencies  <- create_hashmultiset(unlist(lapply(set_relations[assignment == i], length)))
    multinetwork@state$statistics_communities[[as.character(i)]]$total_arity <- sum(as.integer(hash::keys(multinetwork@state$statistics_communities[[as.character(i)]]$arity_frequencies)) 
                                                                                    * hash::values(multinetwork@state$statistics_communities[[as.character(i)]]$arity_frequencies))
    
    maxpop <- max(maxpop, multinetwork@state$statistics_communities[[as.character(i)]]$size_population)
  }
  multinetwork@state$global_statistics$size_largest_population <- maxpop
  
  parameter <- new("Parameter")
  parameter@state <- rlang::new_environment()
  parameter@state$local_parameters  <- hash::hash()
  parameter@state$global_parameters <- vector("list", 3)
  names(parameter@state$global_parameters) <- c("strength", "discount", "overlap")
  parameter@state$global_parameters$strength <- .8
  parameter@state$global_parameters$discount <- 0.1
  parameter@state$global_parameters$overlap <- 11L
  
  logdhollywood <- function(set_relations, alpha, theta) {
    v <- length(unique(unlist(set_relations)))
    m <- sum(sapply(set_relations, length))
    deg <- compute_degrees_combining_degrees_individual_relations(lapply(set_relations, compute_hashmultiset_degrees_from_relation))
    unique_deg <- as.integer(sort(unique(hash::values(deg))))
    deg_freq <- as.integer(table(hash::values(deg)))
    return(
      v*log(alpha)  
      + lgamma(theta/alpha + v) - lgamma(theta/alpha) 
      - lgamma(theta + m) + lgamma(theta)
      + sum( deg_freq * (lgamma(unique_deg - alpha) - lgamma(1 - alpha)))
    )
  }
  for(i in lac) {
    proposed_strength <- .7
    expect_equal(
      set_defaultPU("evaluation")$strength(proposed_strength, as.character(i), parameter, multinetwork),
      logdhollywood(set_relations[which(assignment == i)], proposed_strength, parameter@state$global_parameters$discount)
      - logdhollywood(set_relations[which(assignment == i)], parameter@state$global_parameters$strength, parameter@state$global_parameters$discount)
    )
    proposed_discount <- 1.5
    expect_equal(
      set_defaultPU("evaluation")$discount(proposed_discount, as.character(i), parameter, multinetwork),
      ( logdhollywood(set_relations[which(assignment == i)], parameter@state$global_parameters$strength, proposed_discount) 
      - logdhollywood(set_relations[which(assignment == i)], parameter@state$global_parameters$strength, parameter@state$global_parameters$discount))
    )
  }
  
  network <- new("Network", global_statistics = list(size_population = length(unique(unlist(set_relations)))))
  
  logdmerge <- function(list_networks, overlap) {
    population_sizes <- as.integer(lapply(list_networks, function(x) length(unique(unlist(x)))))
    maximum_size_population <- max(population_sizes)
    total_size_population <- length(unique(unlist(list_networks)))
    product <- prod(as.double(lapply(population_sizes, function(x) 1-x/(maximum_size_population + overlap))))
    
    return(log(dbinom(maximum_size_population + overlap - total_size_population, maximum_size_population + overlap, product)))
  }
  proposed_overlap <- (size_population(network) - size_largest_population(multinetwork)):100L
  expect_equal(
    sapply(proposed_overlap, function(x) set_defaultPU("evaluation")$overlap(x, parameter, multinetwork, network)),
    sapply(proposed_overlap, function(x) logdmerge(split(set_relations, assignment), x)) - logdmerge(split(set_relations, assignment), parameter@state$global_parameters$overlap)
  )

})

message("test update")
test_that("test update", {
  setMethod("size_population", "Network", function(object, label_community) object@global_statistics$size_population)
  setMethod("size_population", "Multinetwork", function(object, label_community) object@state$statistics_communities[[as.character(label_community)]]$size_population)
  setGeneric("pair_degrees_with_frequencies", function(object, label_community) standardGeneric("pair_degrees_with_frequencies"))
  setMethod("pair_degrees_with_frequencies", "Multinetwork", function(object, label_community) {
    list(
      degrees = as.integer(hash::keys(  object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies)),
      degree_frequencies = hash::values(object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies)
    )
  })
  setMethod("size_largest_population", "Multinetwork", function(object) object@state$global_statistics$size_largest_population)
  setMethod("label_active_communities", "Multinetwork", function(object) { hash::keys(object@state$statistics_communities) })
  setMethod("total_arity", "Multinetwork", function(object, label_community) object@state$statistics_communities[[as.character(label_community)]]$total_arity)
  
  setMethod("local_parameters",  "Parameter", function(object) {
    key_first_active_community <- hash::keys(object@state$local_parameters)[1]
    return(names(object@state$local_parameters[[key_first_active_community]]))
  })
  setMethod("global_parameters", "Parameter", function(object) names(object@state$global_parameters))
  
  setGeneric("strength", function(object, label_community) standardGeneric("strength"))
  setMethod( "strength", "Parameter", function(object, label_community) {
    if("strength" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$strength)
    else return(object@state$global_parameters$strength)
  })
  setGeneric("discount", function(object, label_community) standardGeneric("discount"))
  setMethod( "discount", "Parameter", function(object, label_community) {
    if("discount" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$discount)
    else return(object@state$global_parameters$discount)
  })
  setGeneric("overlap", function(object) standardGeneric("overlap"))
  setMethod( "overlap", "Parameter", function(object) { return(object@state$global_parameters$overlap) })
  
  
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  } 
  compute_degrees_combining_degrees_individual_relations <- function(degrees_individual_relations) {
    degrees <- hash::hash()
    for(i in 1:length(degrees_individual_relations)) {
      hashmultiset_degrees_in_relation_i <- degrees_individual_relations[[i]]
      keys_relation_i <- hash::keys(hashmultiset_degrees_in_relation_i)
      for(j in seq_along(keys_relation_i)) {
        degrees[[keys_relation_i[j]]] <- null_to_zero(degrees[[keys_relation_i[j]]]) + hashmultiset_degrees_in_relation_i[[keys_relation_i[j]]] 
      }
    } 
    return(degrees)
  }
  
  multinetwork <- new("Multinetwork")
  multinetwork@state <- rlang::new_environment()
  multinetwork@state$statistics_communities <- hash::hash()
  multinetwork@state$global_statistics <- rlang::new_environment()
  
  lac <- c(2L,4L,5L)
  assignment <- rep(lac, 4)[sample.int(length(lac) * 4, length(lac) * 4)]
  assignment2 <- rep(NA_integer_, length(assignment))
  for(i in seq_along(lac)) assignment2[which(assignment == lac[i])] <- i
  set_relations <- sample_HBM(assignment2, .8, .1, 2L, function() 2L+rpois(1,3))
  rm(assignment2)
  
  multinetwork@state <- rlang::new_environment()
  multinetwork@state$statistics_communities <- hash::hash()
  multinetwork@state$global_statistics <- rlang::new_environment()
  
  maxpop <- 0L
  for(i in lac) {
    multinetwork@state$statistics_communities[[as.character(i)]] <- rlang::new_environment()
    
    multinetwork@state$statistics_communities[[as.character(i)]]$degrees <- compute_degrees_combining_degrees_individual_relations(lapply(set_relations[assignment == i], compute_hashmultiset_degrees_from_relation))
    multinetwork@state$statistics_communities[[as.character(i)]]$degree_frequencies <- create_hashmultiset(hash::values(multinetwork@state$statistics_communities[[as.character(i)]]$degrees))
    multinetwork@state$statistics_communities[[as.character(i)]]$size_population <- length(multinetwork@state$statistics_communities[[as.character(i)]]$degrees)
    
    multinetwork@state$statistics_communities[[as.character(i)]]$arity_frequencies  <- create_hashmultiset(unlist(lapply(set_relations[assignment == i], length)))
    multinetwork@state$statistics_communities[[as.character(i)]]$total_arity <- sum(as.integer(hash::keys(multinetwork@state$statistics_communities[[as.character(i)]]$arity_frequencies)) 
                                                                                    * hash::values(multinetwork@state$statistics_communities[[as.character(i)]]$arity_frequencies))
    
    maxpop <- max(maxpop, multinetwork@state$statistics_communities[[as.character(i)]]$size_population)
  }
  multinetwork@state$global_statistics$size_largest_population <- maxpop
  
  parameter <- new("Parameter")
  parameter@state <- rlang::new_environment()
  parameter@state$local_parameters  <- hash::hash()
  parameter@state$global_parameters <- vector("list", 3)
  names(parameter@state$global_parameters) <- c("strength", "discount" ,"overlap")
  parameter@state$global_parameters$strength <- .8
  parameter@state$global_parameters$discount <- .1
  parameter@state$global_parameters$overlap <- 3L
  
  parameter_update <- ParameterUpdate(parameter)
  
  expect_type(replicate(50, metropolis_hastings_strength(parameter_update, parameter, multinetwork)), "double")
  expect_type(replicate(50, metropolis_hastings_discount(parameter_update, parameter, multinetwork)), "double")
  
  # strength and discount are global
  expect_error(metropolis_hastings_strength(parameter_update, parameter, multinetwork, "2")) 
  expect_error(metropolis_hastings_discount(parameter_update, parameter, multinetwork, "2")) 
  
  
  network <- new("Network", global_statistics = list(size_population = length(unique(unlist(set_relations)))))
  expect_type(replicate(50, metropolis_hastings_overlap(parameter_update, parameter, multinetwork, network)), "integer")
  
  parameter <- new("Parameter")
  parameter@state <- rlang::new_environment()
  parameter@name_local_parameters <- c("strength", "discount")
  parameter@state$local_parameters  <- hash::hash()
  parameter@state$global_parameters <- vector("list", 1)
  names(parameter@state$global_parameters) <- c("overlap")
  for(i in lac) {
    parameter@state$local_parameters[[as.character(i)]] <- vector("list", 2)
    names(parameter@state$local_parameters[[as.character(i)]]) <- parameter@name_local_parameters
    for(j in seq_along(c("strength", "discount"))) parameter@state$local_parameters[[as.character(i)]][[c("strength", "discount")[j]]] <- c(strength = .8, discount = .1)[[c("strength", "discount")[j]]]
  }
  parameter@state$global_parameters$overlap <- 3L
  
  parameter_update <- ParameterUpdate(parameter)
  
  for(i in lac) {
    expect_type(replicate(50, metropolis_hastings_strength(parameter_update, parameter, multinetwork, as.character(i))), "double")
    expect_type(replicate(50, metropolis_hastings_discount(parameter_update, parameter, multinetwork, as.character(i))), "double")
  }
  
  # strength and discount are local
  expect_error(metropolis_hastings_strength(parameter_update, parameter, multinetwork)) 
  expect_error(metropolis_hastings_discount(parameter_update, parameter, multinetwork)) 
  
  # community 1 is not active
  expect_error(metropolis_hastings_strength(parameter_update, parameter, multinetwork, "1")) 
  expect_error(metropolis_hastings_discount(parameter_update, parameter, multinetwork, "1")) 
  
  
})








