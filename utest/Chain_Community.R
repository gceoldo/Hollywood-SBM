library(testthat)

source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/ChainCommunity.R")

test_that("initialization", {
  expect_s4_class(ChainCommunity(5), "ChainCommunity")
  expect_error(ChainCommunity())
  expect_error(ChainCommunity(-1))
})

test_that("store values", {
  length_chain <- 5
  chain_community <- ChainCommunity(length_chain)
  
  expect_error(store_proposed_flip(chain_community)) # missing info flip
  expect_error(store_proposed_flip(chain_community, c(1,2,3))) # not a list
  expect_error(store_proposed_flip(chain_community, list()))
  # flipped input:
  expect_error(store_proposed_flip(chain_community, list(relation = 1L, type_update = "flip", old_community = 2L, new_community = 3L, accepted = T)))
  # relation should be integer:
  expect_error(store_proposed_flip(chain_community, list(type_update = "flip", relation = 1, old_community = 2L, new_community = 3L, accepted = T)))
  # accepted should be boolean:
  expect_error(store_proposed_flip(chain_community, list(type_update = "flip", relation = 1, old_community = 2L, new_community = 3L, accepted = 1)))
  # there are duplicates:
  expect_error(store_proposed_flip(chain_community, list(type_update = "flip", relation = 1L, old_community = 2L, new_community = 3L, accepted = TRUE, accepted = TRUE)))
  
  valid_update <- list(type_update = "flip", relation = 1L, old_community = 2L, new_community = 3L, accepted = TRUE)
  for(i in 1:length_chain) store_proposed_flip(chain_community, valid_update)
  
  expect_equal(chain_community@state$first_empty_observation, length_chain + 1L)
  expect_equal(chain_community@state$chain, data.frame(type_update = rep("flip", length_chain), relation = rep(1L, length_chain), old_community = rep(2L, length_chain), new_community = rep(3L, length_chain), accepted = rep(TRUE, length_chain)))
})

test_that("recover assignment", {
  sample_flip <- function(assignment_vector) {
    sizes <- as.integer(table(assignment_vector))
    relation <- sample.int(length(assignment_vector), 1)
    old_community <- assignment_vector[relation]
    skip_flip <- sizes[old_community] == 1L
    if(skip_flip) return(list(type_update = "flip", relation = relation, old_community = old_community, new_community = old_community, accepted = FALSE))
    else return(list(type_update = "flip", relation = relation, old_community = old_community, 
                     new_community = sample((1:length(sizes))[(1:length(sizes)) != old_community], 1), 
                     accepted = as.logical(trunc(runif(1,0,2)))))
  }
  update_assignment <- function(assignment_vector, info_flip) {
    if(info_flip$accepted) assignment_vector[info_flip$relation] <- info_flip$new_community
    return(assignment_vector)
  }
  
  length_chain <- 50L
  chain_community <- ChainCommunity(length_chain)
  
  initial_assignment <- c(rep(1L, 4), rep(2L, 4), rep(3L, 4), rep(4L, 4))
  initial_assignment <- initial_assignment[sample(length(initial_assignment), length(initial_assignment))]
  
  vector_assignment <- initial_assignment
  number_iterations <- 40L
  assignments <- recovered_assignments <- matrix(NA_integer_, number_iterations, length(initial_assignment))
  
  for(i in 1:number_iterations) {
    info_proposed_flip <- sample_flip(vector_assignment)
    store_proposed_flip(chain_community, info_proposed_flip)
    vector_assignment <- update_assignment(vector_assignment, info_proposed_flip)
    assignments[i,] <- vector_assignment
  } 
  
  expect_equal(vector_assignment, assignments[number_iterations,])
  expect_equal(apply(assignments, 1, function(x) length(unique(x))), rep(4L, number_iterations))
  expect_true(all(sapply(2:number_iterations, function(x) sum(assignments[x,] != assignments[x-1,]) <= 1L)))
  
  expect_equal(initial_assignment, recover_assignment(chain_community, vector_assignment, 40L))
  expect_true(all(sapply(1:(number_iterations-1), function(x) all(assignments[number_iterations-x,] == recover_assignment(chain_community, vector_assignment, x)))))
  
  expect_equal(vector_assignment, recover_assignment(chain_community, initial_assignment, 40L, FALSE))
  expect_true(all(sapply(1:number_iterations, function(x) all(assignments[x,] == recover_assignment(chain_community, initial_assignment, x, FALSE)))))
  
})


