library(testthat)
library(rlang)
library(hash)

message("generic functions defined in other units:")

setGeneric("size_population", function(object, label_community) standardGeneric("size_population"))
setGeneric("community_is_active", function(object, community) standardGeneric("community_is_active"))
setGeneric("set_state_after_flip", function(object, proposed_statistics) standardGeneric("set_state_after_flip"))
setGeneric(name = "label_active_communities", def = function(object) standardGeneric("label_active_communities"))


# load source file for class Multinetwork
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Multinetwork.R")

# prototypes classes and methods to whom Multinetwork depends on
setClass("CommunityAssignment", slots = c(state = "environment"))
CommunityAssignment <- function(assignment) {
  assignment <- as.integer(assignment)
  label_active_communities <- sort(unique(assignment))
  table_sizes <- table(assignment)
  location_communities_in_assignment <- hash::hash()
  for(i in label_active_communities) location_communities_in_assignment[i] <- create_hashset(which(assignment == i))
  new("CommunityAssignment", state = rlang::new_environment(list(assignment = assignment, label_active_communities = label_active_communities, location_communities_in_assignment = location_communities_in_assignment)))
}
setClass("Network", slots = c(individual_statistics = "list"))
Network <- function(set_relations) {
  compute_individual_statistics <- function(relation) { stats <- vector("list", 2); names(stats) <- c("degrees", "arity"); stats$degrees <- compute_hashmultiset_degrees_from_relation(relation); stats$arity <- length(relation); return(stats) } 
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  } 
  
  individual_statistics <- vector("list", length(set_relations))
  for(i in 1:length(set_relations)) individual_statistics[[i]] <- compute_individual_statistics(set_relations[[i]])
  names(individual_statistics) <- as.character(1:length(set_relations))
  new("Network", individual_statistics = individual_statistics)
}

# methods defined in other units
setGeneric(name = "assignment", def = function(object) standardGeneric("assignment"))
setMethod("assignment", "CommunityAssignment", function(object) object@state$assignment)
setMethod("label_active_communities", "CommunityAssignment", function(object) object@state$label_active_communities)
setGeneric(name = "location_selected_communities_in_assignment", def = function(object, which_communities) standardGeneric("location_selected_communities_in_assignment"))
setMethod("location_selected_communities_in_assignment", "CommunityAssignment", function(object, which_communities) {
  locations <- vector(mode="list", length = length(which_communities))
  for(i in seq_along(which_communities)) {
    if(community_is_active(object, which_communities[i])) locations[[i]] <- hash::keys(object@state$location_communities_in_assignment[[as.character(which_communities[i])]])
    else locations[[i]] <- character()
  } 
  if(length(which_communities) > 1L) return(locations)
  else return(locations[[1]])
})
setMethod("community_is_active", "CommunityAssignment", function(object,community) community %in% object@state$label_active_communities)

setGeneric(name = "number_relations", def = function(object) standardGeneric("number_relations"))
setMethod("number_relations", "Network", function(object) length(object@individual_statistics))
setGeneric("degrees_individual_relations",      def = function(object, which_relations) standardGeneric("degrees_individual_relations"))
setMethod( "degrees_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$degrees))
setGeneric("arities_individual_relations",      def = function(object, which_relations) standardGeneric("arities_individual_relations"))
setMethod( "arities_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$arity))


# functions from Utils.R
get_first_free_label <- function(ordered_vector) { for(i in 1:length(ordered_vector)) if(i != ordered_vector[i]) return(i); return(length(ordered_vector) + 1) }
create_hashset <- function(initial_sequence) { hashset <- hash::hash(); hashset[initial_sequence] <- NA_integer_; return(hashset) }
create_hashmultiset <- function(initial_sequence) { hashmultiset <- hash::hash(); hashmultiset[sort(unique(initial_sequence))] <- as.integer(table(initial_sequence)); return(hashmultiset) }
null_to_zero <- function(value) { if(!is.null(value)) { return(value) } else { return(0L) } }
zero_to_null <- function(value) { if(value != 0L) { return(value) } else { return(NULL) } }
#copy_hash_object <- function(object) { new_object <- hash::hash(); for(i in names(object)) new_object[i] <- object[[i]]; return(new_object) }
#remove_elements_from_hashset <- function(hashset, elements_to_be_removed) { for(i in seq_along(elements_to_be_removed))  hashset[[as.character(elements_to_be_removed [i])]] <- NULL;        return(invisible()) }
#include_elements_to_hashset <- function(hashset, elements_to_be_included) { for(i in seq_along(elements_to_be_included)) hashset[[as.character(elements_to_be_included[i])]] <- NA_integer_; return(invisible()) }
extract_frequencies_hashmultiset <- function(hashmultiset, keys, mode_returned_vector) { frequencies <- vector(mode_returned_vector, length(keys)); for(i in seq_along(keys)) frequencies[[i]] <- null_to_zero(hashmultiset[[as.character(keys[i])]]); return(frequencies) }
resample <- function(x, ...) x[sample.int(length(x), ...)]




message("test successful initialization:")
test_that("test successful initialization", {
  size_set_relations <- 10L
  set_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  vector_assignment <- as.integer(runif(size_set_relations, 1, 4))
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  # check correct class:
  expect_s4_class(object = multinetwork, class = "Multinetwork")
  # incoherent length assignment and network:
  expect_error(Multinetwork(CommunityAssignment(vector_assignment[-1]), network))
  expect_error(Multinetwork(community_assignment, Network(set_relations[-6])))
  
  expect_equal(label_active_communities(multinetwork), as.character(label_active_communities(community_assignment)))
})

message("test extract pair degrees - degree_frequencies, arities - arity_frequencies:")
test_that("test extract pair degrees - degree_frequencies, arities - arity_frequencies", {
  size_set_relations <- 10L
  set_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  vector_assignment <- as.integer(runif(size_set_relations, 1, 4))
  label_communities <- sort(unique(vector_assignment))
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  for(i in label_communities) {
    actors <- unique(unlist(set_relations[which(vector_assignment == i)]))
    sorted_keys_actors <- sort(as.character(actors))
    
    degrees_extracted_with_keys <- sapply(sorted_keys_actors, function(x) degrees(multinetwork, as.character(i), x))
    degrees_extracted_directly <- pair_degrees_with_frequencies(multinetwork, as.character(i))$degrees
    
    expect_equal(sort(unique(degrees_extracted_with_keys)), degrees_extracted_directly)
    # keys are sorted
    
    frequencies_extracted_with_keys <- sapply(sort(as.character(unique(degrees_extracted_with_keys))), function(x) degree_frequencies(multinetwork, as.character(i), x))
    frequencies_extracted_directly <- pair_degrees_with_frequencies(multinetwork, as.character(i))$degree_frequencies
    
    expect_equal(frequencies_extracted_with_keys, frequencies_extracted_directly)
    # values are not sorted
    
    arities <- sapply(set_relations[which(vector_assignment == i)], length)
    sorted_keys_arities <- sort(as.character(unique(arities)))
    
    arities_extracted_directly <- pair_arities_with_frequencies(multinetwork, as.character(i))$arities
    
    expect_equal(as.integer(sorted_keys_arities), arities_extracted_directly)
    
    frequencies_extracted_with_keys <- sapply(sorted_keys_arities, function(x) arity_frequencies(multinetwork, as.character(i), x))
    frequencies_extracted_directly <- pair_arities_with_frequencies(multinetwork, as.character(i))$arity_frequencies
    
    expect_equal(frequencies_extracted_with_keys, frequencies_extracted_directly)
    # values are not sorted
    
  } 
})

message("test correct computation degrees:")
test_that("test correct computation degrees", {
  size_set_relations <- 10L
  set_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  vector_assignment <- as.integer(runif(size_set_relations, 1, 4))
  label_communities <- sort(unique(vector_assignment))
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  for(i in label_communities) {
    expect_equal(as.integer(table(unlist(set_relations[which(vector_assignment == i)]))),
                 degrees(multinetwork, as.character(i), as.character(sort(unique(unlist(set_relations[which(vector_assignment == i)])))),"integer"))
    expect_equal(length(table(unlist(set_relations[which(vector_assignment == i)]))),
                 size_population(multinetwork, as.character(i)))
  }
})

message("test computation statistics:")
test_that("test computation statistics", {
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  }
  
  # sample dataset
  size_set_relations <- 20L
  set_relations <- vector("list", size_set_relations)
  largest_possible_community <- 15L
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  vector_assignment <- as.integer(runif(size_set_relations, 1, largest_possible_community))
  sampled_communities <- sort(unique(vector_assignment))
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  for(i in sampled_communities) {
    # manually compute statistics 
    keys_degrees <- as.character(sort(unique(unlist(set_relations[which(vector_assignment == i)]))))
    deg <- extract_frequencies_hashmultiset(compute_degrees_combining_degrees_individual_relations(lapply(set_relations[which(vector_assignment == i)], compute_hashmultiset_degrees_from_relation)), keys_degrees, "integer")
    deg_fr <- as.integer(table(deg))
    keys_deg_fr <- as.character(sort(unique(deg)))
    ar <- sapply(set_relations[which(vector_assignment == i)], length)
    ar_fr <- as.integer(table(ar))
    keys_ar <- as.character(sort(unique(ar)))
    
    expect_equal(deg, degrees(multinetwork, as.character(i), keys_degrees))
    expect_equal(deg_fr, degree_frequencies(multinetwork, as.character(i), keys_deg_fr))
    expect_equal(length(keys_degrees), size_population(multinetwork, as.character(i)))
    expect_equal(ar_fr, arity_frequencies(multinetwork, as.character(i), keys_ar))
    expect_equal(sum(ar), total_arity(multinetwork, as.character(i)))
  }
  expect_equal(size_largest_population(multinetwork), max(unlist(lapply(sampled_communities, function(x) length(unique(unlist(set_relations[which(vector_assignment == x)])))))))
})

message("test proposed statistics:")
test_that("test proposed statistics", {
  remove_value_from_vector <- function(vector, value_to_be_removed) { positions_value_to_be_kept <- which(vector != value_to_be_removed); return(vector[positions_value_to_be_kept]) }
  
  # sample dataset
  size_set_relations <- 10L
  set_relations <- vector("list", size_set_relations)
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  
  vector_assignment <- as.integer(runif(size_set_relations, 1, 10))
  sampled_communities <- sort(unique(vector_assignment))
  first_inactive_community <- get_first_free_label(sampled_communities)
  
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  number_trials <- 50L; counter <- 0L
  for(i in 1:number_trials) {
    # sample community flip
    random_relation  <- as.integer(trunc(runif(1,1,1+size_set_relations)))
    random_community <- resample(remove_value_from_vector(sampled_communities, vector_assignment[random_relation]), 1)
    if((sum(vector_assignment == vector_assignment[random_relation]) >= 2L) ) {
      # community flip is valid
      proposed_statistics <- compute_proposed_statistics_after_flip(multinetwork, random_relation, vector_assignment[random_relation], random_community, network)
      vector_assignment2 <- vector_assignment
      vector_assignment2[random_relation] <- random_community
      # compute statistics with new community assignment
      multinetwork2 <- Multinetwork(CommunityAssignment(vector_assignment2), network)
      
      # test equality proposed statistics and statistics with new assignment vector
      
         expect_equal(proposed_statistics$degrees_old_community, degrees(multinetwork2, as.character(vector_assignment[random_relation]), proposed_statistics$keys_degrees_relation))
      expect_equal(proposed_statistics$degrees_new_community, degrees(multinetwork2, as.character(random_community), proposed_statistics$keys_degrees_relation))
      
      expect_equal(degree_frequencies(multinetwork2, as.character(vector_assignment[random_relation]), proposed_statistics$keys_degree_frequencies_old_community),
                   degree_frequencies(multinetwork,  as.character(vector_assignment[random_relation]), proposed_statistics$keys_degree_frequencies_old_community) + proposed_statistics$variation_degree_frequencies_old_community)
      expect_equal(degree_frequencies(multinetwork2, as.character(random_community),                   proposed_statistics$keys_degree_frequencies_new_community),
                   degree_frequencies(multinetwork,  as.character(random_community),                   proposed_statistics$keys_degree_frequencies_new_community) + proposed_statistics$variation_degree_frequencies_new_community)
      
      expect_equal(size_population(multinetwork2, as.character(vector_assignment[random_relation])), proposed_statistics$size_population_old_community)
      expect_equal(size_population(multinetwork2, as.character(random_community)),                   proposed_statistics$size_population_new_community)
      
      expect_equal(arity_frequencies(multinetwork2, as.character(vector_assignment[random_relation]), as.character(proposed_statistics$arity_relation)), proposed_statistics$arity_frequencies_old_community)
      expect_equal(arity_frequencies(multinetwork2, as.character(random_community),                   as.character(proposed_statistics$arity_relation)), proposed_statistics$arity_frequencies_new_community)
      
      expect_equal(total_arity(multinetwork2, as.character(vector_assignment[random_relation])), proposed_statistics$total_arity_old_community)
      expect_equal(total_arity(multinetwork2, as.character(random_community)),                   proposed_statistics$total_arity_new_community)
      
      expect_equal(size_largest_population(multinetwork2), proposed_statistics$size_largest_population)
      
      counter <- counter + 1L
    }
  }
  print(sprintf("proposed statistics after flip tested for %i/%i tests", counter, number_trials))
  
  
  
  vector_assignment <- as.integer(runif(size_set_relations, 1, 8))
  sampled_communities <- sort(unique(vector_assignment))
  first_inactive_community <- get_first_free_label(sampled_communities)
  
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  number_trials <- 50L; probability_sample_new_community <- 0.5
  counter1 <- counter2 <- counter3 <- 0L
  for(i in 1:number_trials) {
    # sample community flip
    random_relation  <- as.integer(trunc(runif(1,1,1+size_set_relations)))
    random_community <- resample(c(first_inactive_community, remove_value_from_vector(sampled_communities, vector_assignment[random_relation])), 1, 
                                 prob = c(probability_sample_new_community, (1-probability_sample_new_community)*rep(1, length(sampled_communities)-1)/(length(sampled_communities)-1)))
    proposed_statistics <- compute_proposed_statistics_after_flip(multinetwork, random_relation, vector_assignment[random_relation], random_community, network)
    vector_assignment2 <- vector_assignment
    vector_assignment2[random_relation] <- random_community
    # compute statistics with new community assignment
    multinetwork2 <- Multinetwork(CommunityAssignment(vector_assignment2), network)
      
    # test equality proposed statistics and statistics with new assignment vector
      
    expect_equal(proposed_statistics$degrees_old_community, degrees(multinetwork2, as.character(vector_assignment[random_relation]), proposed_statistics$keys_degrees_relation))
    expect_equal(proposed_statistics$degrees_new_community, degrees(multinetwork2, as.character(random_community), proposed_statistics$keys_degrees_relation))
      
    expect_equal(degree_frequencies(multinetwork2, as.character(vector_assignment[random_relation]), proposed_statistics$keys_degree_frequencies_old_community),
                 degree_frequencies(multinetwork,  as.character(vector_assignment[random_relation]), proposed_statistics$keys_degree_frequencies_old_community) + proposed_statistics$variation_degree_frequencies_old_community)
    expect_equal(degree_frequencies(multinetwork2, as.character(random_community),                   proposed_statistics$keys_degree_frequencies_new_community),
                 degree_frequencies(multinetwork,  as.character(random_community),                   proposed_statistics$keys_degree_frequencies_new_community) + proposed_statistics$variation_degree_frequencies_new_community)
      
    expect_equal(null_to_zero(size_population(multinetwork2, as.character(vector_assignment[random_relation]))), proposed_statistics$size_population_old_community)
    expect_equal(size_population(multinetwork2, as.character(random_community)), proposed_statistics$size_population_new_community)
      
    expect_equal(arity_frequencies(multinetwork2, as.character(vector_assignment[random_relation]), as.character(proposed_statistics$arity_relation)), proposed_statistics$arity_frequencies_old_community)
    expect_equal(arity_frequencies(multinetwork2, as.character(random_community),                   as.character(proposed_statistics$arity_relation)), proposed_statistics$arity_frequencies_new_community)
      
    expect_equal(null_to_zero(total_arity(multinetwork2, as.character(vector_assignment[random_relation]))), proposed_statistics$total_arity_old_community)
    expect_equal(total_arity(multinetwork2, as.character(random_community)), proposed_statistics$total_arity_new_community)
      
    expect_equal(size_largest_population(multinetwork2), proposed_statistics$size_largest_population)
    
    if((random_community == first_inactive_community) & (sum(vector_assignment == vector_assignment[random_relation]) == 1)) counter1 <- counter1 + 1L
    if((random_community != first_inactive_community) & (sum(vector_assignment == vector_assignment[random_relation]) == 1)) counter2 <- counter2 + 1L
    if((random_community == first_inactive_community) & (sum(vector_assignment == vector_assignment[random_relation]) >  1)) counter3 <- counter3 + 1L
    
  }
  print(sprintf("relabelled one dimensional community in %i/%i tests", counter1, number_trials))
  print(sprintf("merge to an existing community %i/%i tests", counter2, number_trials))
  print(sprintf("creation of a new community in %i/%i tests", counter3, number_trials))
})

message("test restricted community flip:")
test_that("test restricted community flip", {
  remove_value_from_vector <- function(vector, value_to_be_removed) {
    positions_value_to_be_kept <- which(vector != value_to_be_removed)
    return(vector[positions_value_to_be_kept])
  }
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  }
  
  # sample dataset
  size_set_relations <- 20L
  set_relations <- vector("list", size_set_relations)
  largest_possible_community <- 15L
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  vector_assignment <- as.integer(runif(size_set_relations, 1, largest_possible_community))
  sampled_communities <- sort(unique(vector_assignment))
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  # flip community many times
  number_flips <- 30L; counter <- 0L
  for(i in 1:number_flips) {
    # sample community flip
    random_relation  <- as.integer(trunc(runif(1,1,1+size_set_relations)))
    random_community <- resample(remove_value_from_vector(sampled_communities, vector_assignment[random_relation]), 1)
    if((sum(vector_assignment == vector_assignment[random_relation]) >= 2L) ) {
      # community flip is valid
      proposed_statistics <- compute_proposed_statistics_after_flip(multinetwork, random_relation, vector_assignment[random_relation], random_community, network)
      # update state after community flip
      set_state_after_flip(multinetwork, proposed_statistics = proposed_statistics)
      # update assignment vector
      vector_assignment[random_relation] <- random_community
      
      counter <- counter + 1L
    }    
  }
  
  # compute communities in final assignment vector
  sampled_communities <- sort(unique(vector_assignment))
  # compute statistics with final assignment vector
  multinetwork2 <- Multinetwork(CommunityAssignment(vector_assignment), network)
  for(i in sampled_communities) {
    expect_equal(degrees(multinetwork, i, 1:30), degrees(multinetwork2, i, 1:30))
    expect_equal(degree_frequencies(multinetwork, i, 1:30), degree_frequencies(multinetwork2, i, 1:30))
    expect_equal(arity_frequencies(multinetwork, i, 1:10), arity_frequencies(multinetwork2, i, 1:10))
    expect_equal(total_arity(multinetwork, i), total_arity(multinetwork2, i))
    expect_equal(size_population(multinetwork, i), size_population(multinetwork2, i))
  }
  expect_equal(size_largest_population(multinetwork), size_largest_population(multinetwork2))
  
  print(sprintf("communities are flipped in %i/%i iterations", counter, number_flips))
}) 

message("test community flip:")
test_that("test community flip", {
  remove_value_from_vector <- function(vector, value_to_be_removed) {
    positions_value_to_be_kept <- which(vector != value_to_be_removed)
    return(vector[positions_value_to_be_kept])
  }
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  }
  
  size_set_relations <- 20L
  set_relations <- vector("list", size_set_relations)
  largest_possible_community <- 15L
  for(i in 1:size_set_relations) { arity <- 2+rpois(1, 2); set_relations[[i]] <- as.integer(runif(arity, 1, 10)) }
  network <- Network(set_relations)
  
  
  vector_assignment <- as.integer(runif(size_set_relations, 1, size_set_relations - 8L))
  sampled_communities <- sort(unique(vector_assignment))
  first_inactive_community <- get_first_free_label(sampled_communities)
  
  community_assignment <- CommunityAssignment(vector_assignment)
  multinetwork <- Multinetwork(community_assignment, network)
  
  number_trials <- 50L; probability_sample_new_community <- 0.5
  counter1 <- counter2 <- counter3 <- 0L
  for(i in 1:number_trials) {
    sampled_communities <- sort(unique(vector_assignment))
    first_inactive_community <- get_first_free_label(sampled_communities)
    
    random_relation  <- as.integer(trunc(runif(1,1,1+size_set_relations)))
    random_community <- resample(c(first_inactive_community, remove_value_from_vector(sampled_communities, vector_assignment[random_relation])), 1, 
                                 prob = c(probability_sample_new_community, (1-probability_sample_new_community)*rep(1, length(sampled_communities)-1)/(length(sampled_communities)-1)))
    
    if((random_community == first_inactive_community) & (sum(vector_assignment == vector_assignment[random_relation]) == 1)) counter1 <- counter1 + 1L
    if((random_community != first_inactive_community) & (sum(vector_assignment == vector_assignment[random_relation]) == 1)) counter2 <- counter2 + 1L
    if((random_community != first_inactive_community) & (sum(vector_assignment == vector_assignment[random_relation]) >  1)) counter3 <- counter3 + 1L
    
    proposed_statistics <- compute_proposed_statistics_after_flip(multinetwork, random_relation, vector_assignment[random_relation], random_community, network)
    set_state_after_flip(multinetwork, proposed_statistics = proposed_statistics)
    vector_assignment[random_relation] <- random_community
  }
  
  print(sprintf("relabelled one dimensional community in %i/%i tests", counter1, number_trials))
  print(sprintf("removal one dimensional community %i/%i tests", counter2, number_trials))
  print(sprintf("standard community flip in %i/%i tests", counter3, number_trials))
  print(sprintf("creation of a new community in %i/%i tests", number_trials-counter3-counter2-counter1, number_trials))
  
  sampled_communities <- sort(unique(vector_assignment))
  # compute statistics with final assignment vector
  multinetwork2 <- Multinetwork(CommunityAssignment(vector_assignment), network)
  for(i in sampled_communities) {
    expect_equal(degrees(multinetwork, i, 1:30), degrees(multinetwork2, i, 1:30))
    expect_equal(degree_frequencies(multinetwork, i, 1:30), degree_frequencies(multinetwork2, i, 1:30))
    expect_equal(arity_frequencies(multinetwork, i, 1:10), arity_frequencies(multinetwork2, i, 1:10))
    expect_equal(total_arity(multinetwork, i), total_arity(multinetwork2, i))
    expect_equal(size_population(multinetwork, i), size_population(multinetwork2, i))
  }
  expect_equal(size_largest_population(multinetwork), size_largest_population(multinetwork2))
  
  
})



