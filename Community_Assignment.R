# constructor and methods for class  CommumityAssignment 
# object of this class contains a state that is updated with reference semantics

setClass("CommunityAssignment", slots = c(
  state = "environment", 
  update_state = "list"
))

CommunityAssignment <- function(label_relations, community_structure, assignment) {
  if(!is(label_relations, "LabelRelations")) stop("wrong class")
  if(!is(community_structure, "CommunityStructure")) stop("wrong class")
  if(!is(assignment, "numeric")) stop("wrong class")
  
  new("CommunityAssignment",
      state = initialize_state_CA(label_relations, community_structure, assignment),
      update_state = initialize_update_state_CA(community_structure)
  )
  
}


initialize_state_CA <- function(label_relations, community_structure, assignment) {
  if(length(assignment) != number_relations(label_relations)) stop("wrong length assignment")
  
  assignment <- as.integer(assignment)
  label_active_communities <- sort(unique(assignment))
  if(label_active_communities[1] <= 0) stop("components of assignment must be positive")
  number_communities <- length(label_active_communities)
  label_first_inactive_community <- get_first_free_label(label_active_communities)
  
  if(number_communities < minimum_allowed_number_communities(community_structure)) stop("error")
  if(number_communities > maximum_allowed_number_communities(community_structure)) stop("error")
  
  size_communities <- hash::hash(table(assignment))
  
  return(rlang::new_environment(list(
    number_communities = number_communities, 
    size_communities = size_communities, 
    assignment = assignment, 
    label_active_communities = label_active_communities, 
    label_first_inactive_community = label_first_inactive_community
  )))
}

initialize_update_state_CA <- function(community_structure) {
  if(!number_communities_is_fixed(community_structure)) {
    add_empty_community <- function(object) {
      current_label_first_inactive_community <- object@state$label_first_inactive_community
      current_label_active_communities <- object@state$label_active_communities
      new_label_active_communities <- add_value_to_ordered_vector(current_label_first_inactive_community, current_label_active_communities)
      new_label_first_inactive_community <- get_first_free_label(new_label_active_communities)
      
      object@state$number_communities <- object@state$number_communities + 1L
      object@state$size_communities[[as.character(current_label_first_inactive_community)]] <- 0L
      object@state$label_active_communities <- new_label_active_communities
      object@state$label_first_inactive_community <- new_label_first_inactive_community
      
      return(invisible(NULL))
    }
    rm_empty_community <- function(label_removed_community, object) {
      if(object@state$size_communities[[as.character(label_removed_community)]] > 0L) stop("error")
      current_label_active_communities <- object@state$label_active_communities
      new_label_active_communities <- rm_value_from_ordered_vector(label_removed_community, current_label_active_communities)
      new_label_first_inactive_community <- get_first_free_label(new_label_active_communities)
      
      object@state$number_communities <- object@state$number_communities - 1L
      object@state$size_communities[[as.character(label_removed_community)]] <- NULL
      object@state$label_active_communities <- new_label_active_communities
      object@state$label_first_inactive_community <- new_label_first_inactive_community
      
      return(invisible(NULL))
    }
  } else {
    add_empty_community <- function(object) stop("number of community is fixed")
    rm_empty_community <- function(object) stop("number of community is fixed")
  }
  update_size_communities <- function(new_sizes, communities, object) { object@state$size_communities[as.character(communities)] <- new_sizes ; return(invisible()) }
  set_assignment <- function(new_assignment, relations, object) { object@state$assignment[relations] <- new_assignment ; return(invisible()) }
  return(list(
    update_size_communities = update_size_communities,
    set_assignment = set_assignment,
    add_empty_community = add_empty_community,
    rm_empty_community = rm_empty_community
  ))
}


setMethod("number_relations", "CommunityAssignment", function(object) length(object@state$assignment))


setGeneric(name = "number_communities", def = function(object) standardGeneric("number_communities"))
setMethod("number_communities", "CommunityAssignment", function(object) object@state$number_communities)
setGeneric(name = "label_active_communities", def = function(object) standardGeneric("label_active_communities"))
setMethod("label_active_communities", "CommunityAssignment", function(object) object@state$label_active_communities)
setGeneric(name = "label_first_inactive_community", def = function(object) standardGeneric("label_first_inactive_community"))
setMethod("label_first_inactive_community", "CommunityAssignment", function(object) object@state$label_first_inactive_community)
setGeneric(name = "assignment", def = function(object) standardGeneric("assignment"))
setMethod("assignment", "CommunityAssignment", function(object) object@state$assignment)
setGeneric(name = "size_communities", def = function(object) standardGeneric("size_communities"))
setMethod("size_communities", "CommunityAssignment", function(object) copy_hash_object(object@state$size_communities))

setGeneric(name = "select_assignment_from_relations", def = function(object, relations) standardGeneric("select_assignment_from_relations"))
setMethod("select_assignment_from_relations", "CommunityAssignment", function(object, relations) object@state$assignment[relations])
setGeneric(name = "size_selected_communities", def = function(object, which_communities, mode_returned_vector) standardGeneric("size_selected_communities"))
setMethod("size_selected_communities", "CommunityAssignment", function(object, which_communities, mode_returned_vector) {
  if(missing(mode_returned_vector)) mode_returned_vector <- "integer"
  sizes <- vector(mode=mode_returned_vector, length = length(which_communities))
  for(i in seq_along(which_communities)) sizes[[i]] <- null_to_zero(object@state$size_communities[[as.character(which_communities[i])]])
  return(sizes)
})

setGeneric(name = "location_selected_communities_in_assignment", def = function(object, relations) standardGeneric("location_selected_communities_in_assignment"))
setMethod("location_selected_communities_in_assignment", "CommunityAssignment", function(object, relations) which(object@state$assignment == as.integer(relations)))


setGeneric(name = "get_copy_state", def = function(object) standardGeneric("get_copy_state"))
setMethod("get_copy_state", "CommunityAssignment", function(object) {
  return(rlang::new_environment(list(
    number_communities = number_communities(object), 
    size_communities = size_communities(object), 
    assignment = assignment(object), 
    label_active_communities = label_active_communities(object), 
    label_first_inactive_community = label_first_inactive_community(object)
  )))
})

setGeneric("community_is_active", function(object, community) standardGeneric("community_is_active"))
setMethod("community_is_active", "CommunityAssignment", function(object,community) community %in% object@state$label_active_communities)
setGeneric("community_is_first_inactive", function(object, community) standardGeneric("community_is_first_inactive"))
setMethod("community_is_first_inactive", "CommunityAssignment", function(object,community) community == object@state$label_first_inactive_community)

setGeneric("proposed_flip_is_valid", function(object, relation, new_community) standardGeneric("proposed_flip_is_valid"))
setMethod("proposed_flip_is_valid", "CommunityAssignment", function(object, relation, new_community) {
  old_community <- select_assignment_from_relations(object, relation)
  if(size_selected_communities(object, old_community) <= 1) return(FALSE)
  if(!community_is_active(object, new_community)) return(FALSE)
  return(TRUE)
})

setGeneric("set_state_after_flip", function(object, proposed_statistics) standardGeneric("set_state_after_flip"))
setMethod("set_state_after_flip", "CommunityAssignment", function(object, proposed_statistics) {
  relation <- as.integer(proposed_statistics$info_flip$relation)
  new_community <- as.integer(proposed_statistics$info_flip$new_community)
  old_community <- as.integer(proposed_statistics$info_flip$old_community)
  
  if(new_community == label_first_inactive_community(object)) object@update_state$add_empty_community(object)
  new_sizes <- size_selected_communities(object, c(old_community, new_community))  + c(-1L, 1L)*(old_community != new_community)
  object@update_state$update_size_communities(new_sizes, c(old_community, new_community), object)
  if(size_selected_communities(object, old_community) == 0L) object@update_state$rm_empty_community(old_community, object)
  object@update_state$set_assignment(new_community, relation, object)
  return(invisible())
})

setGeneric("entropy", function(object) standardGeneric("entropy"))
setMethod("entropy", "CommunityAssignment", function(object) {
  size_probabilities <- hash::values(size_communities(object))/number_relations(object)
  return(-sum(size_probabilities * log(size_probabilities)))
})





