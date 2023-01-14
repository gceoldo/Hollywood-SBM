library(testthat)

invisible(setGeneric(name = "number_relations", def = function(object) standardGeneric("number_relations")))
invisible(setGeneric(name = "number_communities_is_fixed", def = function(object) standardGeneric("number_communities_is_fixed")))
invisible(setGeneric(name = "maximum_allowed_number_communities", def = function(object) standardGeneric("maximum_allowed_number_communities")))
invisible(setGeneric(name = "minimum_allowed_number_communities", def = function(object) standardGeneric("minimum_allowed_number_communities")))

source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Community_Assignment.R")

setClass("LabelRelations", slots = c(number_relations = "integer"))
setClass("CommunityStructure", slots = c(number_communities_is_fixed = "logical", maximum_allowed_number_communities = "integer", minimum_allowed_number_communities = "integer"))
setMethod("number_relations", "LabelRelations", function(object) object@number_relations)
setMethod(f = "number_communities_is_fixed", signature = "CommunityStructure", definition = function(object) object@number_communities_is_fixed)
setMethod("maximum_allowed_number_communities", "CommunityStructure", function(object) object@maximum_allowed_number_communities)
setMethod("minimum_allowed_number_communities", "CommunityStructure", function(object) object@minimum_allowed_number_communities)

get_first_free_label <- function(ordered_vector) { for(i in 1:length(ordered_vector)) if(i != ordered_vector[i]) return(i); return(length(ordered_vector) + 1) }

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

correct <- list(
  number_communities = function(community_assignment) {length(unique(assignment(community_assignment))) == number_communities(community_assignment)},
  active_communities = function(community_assignment) {all(sort(unique(assignment(community_assignment))) == label_active_communities(community_assignment))},
  first_inactive_community = function(community_assignment) {which(!( (1:(max(assignment(community_assignment)) + 1)) %in% unique(assignment(community_assignment)) ))[1] == label_first_inactive_community(community_assignment)},
  size_communities = function(community_assignment) {hash_are_equal(hash::hash(table(assignment(community_assignment))), size_communities(community_assignment))}
)

hash_are_equal <- function(object1, object2) all(hash::keys(object1) == hash::keys(object2)) & all(hash::values(object1) == hash::values(object2))
copy_hash_object <- function(object) {
  new_object <- hash::hash()
  for(i in names(object)) new_object[i] <- object[[i]]
  return(new_object)
}

message("test coherence class and read-only methods:")
test_that("test coherence class and read-only methods", {
  label_relations <- new("LabelRelations", number_relations = 10L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = TRUE, maximum_allowed_number_communities = 10L, minimum_allowed_number_communities = 1L)
  vector_assignment <- 2+rbinom(10L, 3, .5)
  community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
  
  expect_true(correct$number_communities(community_assignment))
  expect_true(correct$active_communities(community_assignment))
  expect_true(correct$first_inactive_community(community_assignment))
  expect_true(correct$size_communities(community_assignment))
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

null_to_zero <- function(value) {
  if(!is.null(value)) return(value)
  else return(0L)
}

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

message("test coherence current state after restricted community flips:")
test_that("test coherence current state after restricted community flips", {
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
  
  expect_true(correct$number_communities(community_assignment))
  expect_true(correct$active_communities(community_assignment))
  expect_true(correct$first_inactive_community(community_assignment))
  expect_true(correct$size_communities(community_assignment))
})

add_value_to_ordered_vector <- function(new_value, ordered_vector) {
  return(c(ordered_vector[which(ordered_vector < new_value)], new_value, ordered_vector[which(ordered_vector > new_value)]))
} 
rm_value_from_ordered_vector <- function(rm_value, ordered_vector) {
  return(ordered_vector[which(rm_value != ordered_vector)])
} 

message("test adding and removal communities")
test_that("test adding and removal communities", {
  label_relations <- new("LabelRelations", number_relations = 10L)
  community_structure <- new("CommunityStructure", number_communities_is_fixed = FALSE, maximum_allowed_number_communities = 10L, minimum_allowed_number_communities = 1L)
  number_tests <- 10
  for(i in 1:number_tests) {
    vector_assignment <- as.integer(runif(10L, 1, 9))
    community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
    new_community <- which(!( (1:(max(vector_assignment) + 1)) %in% unique(vector_assignment) ))[1]
    
    community_assignment@update_state$add_empty_community(community_assignment)
    
    expect_equal(number_communities(community_assignment), length(unique(vector_assignment)) + 1L)
    expect_equal(size_communities(community_assignment)[[as.character(new_community)]], 0L)
    expect_equal(label_active_communities(community_assignment), sort(unique(c(vector_assignment, new_community))))
    expect_equal(label_first_inactive_community(community_assignment), which(!( (1:(max(c(vector_assignment, new_community)) + 1)) %in% unique(c(vector_assignment, new_community)) ))[1])
  }
  number_tests <- 100
  for(i in 1:number_tests) {
    vector_assignment <- as.integer(runif(10L, 1, 10))
    community_assignment <- CommunityAssignment(label_relations, community_structure, vector_assignment)
    random_relation <- sample.int(10, 1)
    sampled_community <- sample(unique(vector_assignment)[which(unique(vector_assignment) != vector_assignment[random_relation])], 1)
    
    community_assignment@state$assignment[random_relation] <- sampled_community
    community_assignment@state$size_communities[[as.character(vector_assignment[random_relation])]] <- community_assignment@state$size_communities[[as.character(vector_assignment[random_relation])]] - 1L
    community_assignment@state$size_communities[[as.character(sampled_community)]] <- community_assignment@state$size_communities[[as.character(sampled_community)]] + 1L
    
    if(sum(vector_assignment == vector_assignment[random_relation]) == 1L) {
      community_assignment@update_state$rm_empty_community(as.character(vector_assignment[random_relation]), community_assignment)
      
      expect_equal(number_communities(community_assignment), length(unique(vector_assignment)) - 1L)
      expect_equal(size_communities(community_assignment)[[as.character(sampled_community)]], sum(vector_assignment == sampled_community) + 1L)
      expect_equal(label_active_communities(community_assignment), sort(unique(c(vector_assignment[-random_relation]))))
      expect_equal(label_first_inactive_community(community_assignment), which(!( (1:(max(vector_assignment) + 1)) %in% unique(vector_assignment[-random_relation]) ))[1])
    }
    else expect_error(community_assignment@update_state$rm_empty_community(as.character(vector_assignment[random_relation]), community_assignment))
  }
})

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
  
  expect_true(correct$number_communities(community_assignment))
  expect_true(correct$active_communities(community_assignment))
  expect_true(correct$first_inactive_community(community_assignment))
  expect_true(correct$size_communities(community_assignment))
})



