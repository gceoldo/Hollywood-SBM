library(testthat)
library(rlang)
library(hash)

message("initialize generic not defined in Community_Assignment.R:")
setGeneric(name = "number_relations", def = function(object) standardGeneric("number_relations"))
setGeneric(name = "number_communities_is_fixed", def = function(object) standardGeneric("number_communities_is_fixed"))
setGeneric(name = "maximum_allowed_number_communities", def = function(object) standardGeneric("maximum_allowed_number_communities"))
setGeneric(name = "minimum_allowed_number_communities", def = function(object) standardGeneric("minimum_allowed_number_communities"))

# load source file for class CommunityAssignment
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Community_Assignment.R")

# prototypes classes, methods and other function to whom CommunityAssignment depends on
setClass("LabelRelations", slots = c(number_relations = "integer"))
setClass("CommunityStructure", slots = c(number_communities_is_fixed = "logical", maximum_allowed_number_communities = "integer", minimum_allowed_number_communities = "integer"))
setMethod("number_relations", "LabelRelations", function(object) object@number_relations)
setMethod(f = "number_communities_is_fixed", signature = "CommunityStructure", definition = function(object) object@number_communities_is_fixed)
setMethod("maximum_allowed_number_communities", "CommunityStructure", function(object) object@maximum_allowed_number_communities)
setMethod("minimum_allowed_number_communities", "CommunityStructure", function(object) object@minimum_allowed_number_communities)

# functions from Utils.R
get_first_free_label <- function(ordered_vector) { for(i in 1:length(ordered_vector)) if(i != ordered_vector[i]) return(i); return(length(ordered_vector) + 1) }
create_hashset <- function(initial_sequence) { hashset <- hash::hash(); hashset[initial_sequence] <- NA_integer_; return(hashset) }
null_to_zero <- function(value) { if(!is.null(value)) { return(value) } else { return(0L) } }
zero_to_null <- function(value) { if(value != 0L) { return(value) } else { return(NULL) } }
copy_hash_object <- function(object) { new_object <- hash::hash(); for(i in names(object)) new_object[i] <- object[[i]]; return(new_object) }
remove_elements_from_hashset <- function(hashset, elements_to_be_removed) { for(i in seq_along(elements_to_be_removed))  hashset[[as.character(elements_to_be_removed [i])]] <- NULL;        return(invisible()) }
include_elements_to_hashset <- function(hashset, elements_to_be_included) { for(i in seq_along(elements_to_be_included)) hashset[[as.character(elements_to_be_included[i])]] <- NA_integer_; return(invisible()) }



message("test successful initialization:")
test_that("test successful initialization", {
  label_relations <- new("LabelRelations", number_relations = 10L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 10L, minimum_allowed_number_communities = 1L)
  vector_assignment <- 2+rbinom(10L, 3, .5)
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  # check correct class:
  expect_s4_class(object = community_assignment, class = "CommunityAssignment")
  # wrong length assignment:
  expect_error(CommunityAssignment(label_relations, community_structure, 2+rbinom(11, 3, .5)))
  # wrong label assignment
  expect_error(CommunityAssignment(new("LabelRelations", number_relations = 3L), community_structure, c(2,2,0)))
  
  # number of communities in assignment smallest than minimum
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 10L, 
                             minimum_allowed_number_communities = 3L)
  expect_error(CommunityAssignment(label_relations, community_structure, 1+rbinom(10L, 1, .5)))
  # number of communities in assignment bigger than minimum
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, 
                             maximum_allowed_number_communities = 3L, minimum_allowed_number_communities = 1L)
  expect_error(CommunityAssignment(label_relations, community_structure, c(1,rep(c(2,3,4),3))))
})



message("test read-only methods:")
test_that("test read-only methods", {
  label_relations <- new("LabelRelations", number_relations = 10L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 10L, minimum_allowed_number_communities = 1L)
  vector_assignment <- 2+rbinom(10L, 3, .5)
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  
  # method to extract information current state
  expect_equal(number_communities(community_assignment), length(unique(vector_assignment)))
  expect_equal(assignment(community_assignment),vector_assignment)
  expect_equal(size_selected_communities(community_assignment, sort(unique(vector_assignment))),
               as.integer(table(vector_assignment)))
  expect_equal(lapply(location_selected_communities_in_assignment(community_assignment, sort(unique(vector_assignment))), function(x) sort(as.integer(x))),
               lapply(sort(unique(vector_assignment)), function(x) which(vector_assignment == x)))
  expect_equal(label_active_communities(community_assignment), sort(unique(vector_assignment)))
  expect_equal(label_first_inactive_community(community_assignment), 
               min((1:(number_communities(community_assignment)+1))[!(1:(number_communities(community_assignment)+1) %in% sort(unique(vector_assignment)))]))
  
  # size of inactive communities is 0
  expect_equal(size_selected_communities(community_assignment,label_first_inactive_community(community_assignment)), 0L)
  
  # check impossible to select size of specific communities without pass them as reference
  expect_error(size_selected_communities(community_assignment))
  # check no relations in first inactive community
  expect_length(location_selected_communities_in_assignment(community_assignment,label_first_inactive_community(community_assignment)), 0L)
  
  
})  



message("test semantic of copied and original environments:")
test_that("test semantic of copied and original environments", {
  label_relations <- new("LabelRelations", number_relations = 10L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 10L, minimum_allowed_number_communities = 1L)
  vector_assignment <- 2+rbinom(10L, 3, .5)
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  # check copy-and-move and reference semantic of copied and orginal current state
  ccs <- get_copy_state(community_assignment)
  comm <- as.character(sample(unique(vector_assignment),1))
  ccs$size_communities[[comm]] <- ccs$size_communities[[comm]] + 1L
  expect_equal(get_copy_state(community_assignment)$size_communities[[comm]], ccs$size_communities[[comm]] - 1L)
  
})  



message("test validity community flips:")
test_that("test validity community flips", {
  label_relations <- new("LabelRelations", number_relations = 5L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 5L, minimum_allowed_number_communities = 1L)
  vector_assignment <- c(1,3,2,2,1)
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  expect_true(proposed_flip_is_valid(community_assignment, 1L, 2L))
  expect_true(proposed_flip_is_valid(community_assignment, 3L, 1L))
  # not required to flip to another community
  expect_true(proposed_flip_is_valid(community_assignment, 1L, 1L))
  
  # community in relation 2 has size 1
  expect_false(proposed_flip_is_valid(community_assignment, relation=2L, 2L))
  # community 4 is inactive
  expect_false(proposed_flip_is_valid(community_assignment, 1L, 4L))
  
})  



message("test coherence current state after community flips:")
test_that("test coherence current state after community flips", {
  label_relations <- new("LabelRelations", number_relations = 10L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 10L, minimum_allowed_number_communities = 1L)
  vector_assignment <- as.integer(runif(10L, 1, 4))
  sampled_communities <- sort(unique(vector_assignment))
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  # update community_assignment after random flips, 
  number_flips <- 100L
  for(i in 1:number_flips) {
    random_relation  <- as.integer(trunc(runif(1,1,11)))
    random_community <- sample(sampled_communities, 1)
    if(proposed_flip_is_valid(community_assignment, random_relation, random_community)) {
      set_state_after_flip(community_assignment, 
                           list(info_flip=list(relation = random_relation, 
                                               old_community = select_assignment_from_relations(community_assignment, random_relation), 
                                               new_community = random_community)))
    }
  }
  
  # check sizes are coherent with assignment
  expect_equal(size_selected_communities(community_assignment, sampled_communities), 
               as.integer(lapply(sampled_communities, function(x) sum(assignment(community_assignment)==x))))
  # checks location selected communities are are coherent with assignment
  expect_equal(lapply(location_selected_communities_in_assignment(community_assignment,sampled_communities), function(x) sort(as.integer(x))),
               lapply(sampled_communities, function(x) which(assignment(community_assignment)==x)))
  
  # create new CommunityAssignment object with current state, after the community flip
  community_assignment2 <- CommunityAssignment(label_relations, community_structure, assignment(community_assignment))
  # check that assignment, sizes, and locations selected communities are correct
  expect_equal(assignment(community_assignment2), assignment(community_assignment))
  expect_equal(size_selected_communities(community_assignment2, sampled_communities),size_selected_communities(community_assignment, sampled_communities))
  expect_equal(lapply(location_selected_communities_in_assignment(community_assignment2,sampled_communities), function(x) sort(as.integer(x))),
               lapply(location_selected_communities_in_assignment(community_assignment, sampled_communities), function(x) sort(as.integer(x))))
  
  
})

get_first_free_label <- function(ordered_vector) {
  for(i in 1:length(ordered_vector)) if(i != ordered_vector[i]) return(i)
  return(length(ordered_vector) + 1L)
}
add_value_to_ordered_vector <- function(new_value, ordered_vector) {
  return(append(ordered_vector, new_value, after = tail(which(ordered_vector < new_value),1)))
} 
rm_value_from_ordered_vector <- function(rm_value, ordered_vector) {
  return(ordered_vector[which(rm_value != ordered_vector)])
} 

message("test community flip with adding and removal communities:")
test_that("test community flip with adding and removal communities", {
  nrel <- 10L
  label_relations <- new("LabelRelations", number_relations = nrel)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = FALSE, maximum_allowed_number_communities = nrel, minimum_allowed_number_communities = 1L)
  vector_assignment <- as.integer(c(3,10,1,10,3,9,5,4,3,6))
  sampled_communities <- sort(unique(vector_assignment))
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  # update community_assignment after random flips, 
  number_flips <- 100L; counter1 <- counter2 <- 0L 
  for(i in 1:number_flips) {
    random_relation  <- as.integer(trunc(runif(1,1,11)))
    if(sum(assignment(community_assignment) == select_assignment_from_relations(community_assignment, random_relation)) == 1L) {
      random_community <- sample(label_active_communities(community_assignment), 1)
      counter1 <- counter1 + 1L
    } else {
      random_community <- sample(c(label_active_communities(community_assignment), label_first_inactive_community(community_assignment)), 1, prob = c(rep(1, number_communities(community_assignment)),6))
      counter2 <- counter2 + 1L
    }
    set_state_after_flip(
      community_assignment, 
      list(info_flip=list(relation = random_relation, 
                          old_community = select_assignment_from_relations(community_assignment, random_relation), 
                          new_community = random_community)))
    
    
  }
  
  # check sizes are coherent with assignment
  expect_equal(number_communities(community_assignment), length(unique(assignment(community_assignment))))
  expect_equal(label_active_communities(community_assignment), sort(unique(assignment(community_assignment))))
  expect_equal(label_first_inactive_community(community_assignment), 
               which(!((1:(max(assignment(community_assignment))+1)) %in% label_active_communities(community_assignment)))[1])
  expect_equal(size_selected_communities(community_assignment, label_active_communities(community_assignment)), 
               as.integer(lapply(label_active_communities(community_assignment), function(x) sum(assignment(community_assignment)==x))))
  
  # create new CommunityAssignment object with current state, after the community flip
  community_assignment2 <- CommunityAssignment(label_relations, community_structure, assignment(community_assignment))
  # check that assignment, sizes, and locations selected communities are correct
  expect_equal(assignment(community_assignment2), assignment(community_assignment))
  
  
})

