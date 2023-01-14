library(testthat)
library(sets)

# initialize generic not defined in Community_Assignment.R
setGeneric(name = "number_relations", def = function(object) standardGeneric("number_relations"))

# load source file for class CommunityAssignment
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Network.R")

# prototypes classes, methods and other function to whom CommunityAssignment depends on
setClass("LabelRelations", slots = c(number_relations = "integer"))
setMethod("number_relations", "LabelRelations", function(object) object@number_relations)


get_first_free_label <- function(ordered_vector) {
  for(i in 1:length(ordered_vector)) if(i != ordered_vector[i]) return(i)
  return(length(ordered_vector) + 1)
}
add_value_to_ordered_vector <- function(new_value, ordered_vector) {
  return(append(ordered_vector, new_value, after = tail(which(ordered_vector < new_value),1)))
} 
rm_value_from_ordered_vector <- function(rm_value, ordered_vector) {
  return(ordered_vector[which(rm_value != ordered_vector)])
} 
copy_hash_object <- function(object) {
  new_object <- hash::hash()
  for(i in names(object)) new_object[i] <- object[[i]]
  return(new_object)
}
copy_environment <- function(envir) {
  return(rlang::new_environment(as.list(envir), parent = empty_env()))
} 
create_hashset <- function(initial_sequence) {
  hashset <- hash::hash()
  hashset[initial_sequence] <- NA_integer_
  return(hashset)
}
remove_elements_from_hashset <- function(hashset, elements_to_be_removed) {
  for(i in seq_along(elements_to_be_removed)) hashset[[as.character(elements_to_be_removed[i])]] <- NULL
  return(invisible())
}
include_elements_to_hashset <- function(hashset, elements_to_be_included) {
  for(i in seq_along(elements_to_be_included)) hashset[[as.character(elements_to_be_included[i])]] <- NA_integer_
  return(invisible())
}
create_hashmultiset <- function(initial_sequence) {
  hashmultiset <- hash::hash()
  hashmultiset[sort(unique(initial_sequence))] <- as.integer(table(initial_sequence))
  return(hashmultiset)
}
extract_frequencies_hashmultiset <- function(hashmultiset, keys, mode_returned_vector) {
  frequencies <- vector(mode_returned_vector, length(keys))
  for(i in seq_along(keys)) frequencies[[i]] <- null_to_zero(hashmultiset[[as.character(keys[i])]])
  return(frequencies)
}
modify_frequencies_hashmultiset <- function(hashmultiset, keys, new_frequencies_or_variation, inputed_variation = FALSE) {
  if(!inputed_variation) for(i in seq_along(keys)) hashmultiset[[as.character(keys[i])]] <- zero_to_null(new_frequencies_or_variation[[i]])
  else for(i in seq_along(keys)) hashmultiset[[as.character(keys[i])]] <- zero_to_null(null_to_zero(hashmultiset[[as.character(keys[i])]]) + new_frequencies_or_variation[[i]])
  return(invisible())
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



message("test successful initialization:")
test_that("test successful initialization", {
  size_set_relations <- 10L
  label_relations <- new("LabelRelations", number_relations = size_set_relations)
  
  set_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) {
    arity <- 2+rpois(1, 2)
    set_relations[[i]] <- as.integer(runif(arity, 1, 10))
  }
  network <- Network(label_relations, set_relations)
  
  # check correct class:
  expect_s4_class(object = network, class = "Network")
  # wrong length set_relations:
  expect_error(Network(label_relations, set_relations[-5]))
  # set_relations contains a relation with length 1
  expect_error(Network(label_relations, c(set_relations[-size_set_relations],c(2L))))
  # set_relations contains a relation with length 0
  expect_error(Network(label_relations, c(set_relations[-size_set_relations],integer(0))))
  # set_relations contains a relation with an actor with label 0
  expect_error(Network(label_relations, c(set_relations[-size_set_relations],c(2L,0L,2L))))
  # set_relations contains a relation with an actor with a missing value
  expect_error(Network(label_relations, c(set_relations[-size_set_relations],c(2L,NA_integer_,2L))))
  
  
})


message("check computation degrees:")
test_that("check computation degrees", {
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
  
  
  # the functions  get_degrees_from_relation  and  compute_degrees_combining_degrees_individual_relations
  # are tested in comparision to the computation of the degrees with the adjacency matrix
  
  # sample set of relations
  size_set_relations <- 10L
  set_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) {
    arity <- 2+rpois(1,2)
    set_relations[[i]] <- as.integer(runif(arity, 1, 25))
  }
  
  # compute degrees individual relations using  get_degrees_from_relation()
  degrees_individual_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) {
    #degrees_individual_relations[[i]] <- hash::hash()
    #degrees_individual_relations[[i]]$degrees <- degrees_individual_relations(set_relations[[i]])
    degrees_individual_relations[[i]] <- compute_hashmultiset_degrees_from_relation(set_relations[[i]])
  }
  # compute degrees network using  compute_degrees_combining_degrees_individual_relations()
  degrees_multiset <- compute_degrees_combining_degrees_individual_relations(degrees_individual_relations)
  degrees <- gset(support = as.integer(hash::keys(degrees_multiset)), memberships = hash::values(degrees_multiset))
  
  expect_equal(unname(gset_memberships(degrees)), as.integer(table(unlist(set_relations))))
  
  
  
  # compute adjacency matrix for multinetwork
  #actors <- sort(unique(unlist(set_relations)))
  #adjacency_matrix <- matrix(0L, length(actors), length(actors))
  #rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- actors
  #for(i in 1:size_set_relations) {
  #  for(j in 1:(length(set_relations[[i]])-1)) {
  #    actor1 <- set_relations[[i]][ j ]
  #    actor2 <- set_relations[[i]][j+1]
  #    adjacency_matrix[as.character(actor1), as.character(actor2)] <- adjacency_matrix[as.character(actor1), as.character(actor2)] + 1L
  #  }
  #}
  
  # compute out-degrees and in-degrees
  #out_degrees <- rowSums(adjacency_matrix)
  #in_degrees <- colSums(adjacency_matrix)
  
  # compare degrees computed with the two functions
  #expect_equal(gset_memberships(degrees), out_degrees + in_degrees)
  
})


