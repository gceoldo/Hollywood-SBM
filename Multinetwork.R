setClass("Multinetwork", slots = c(
  state = "environment", 
  update_state = "list"
))

Multinetwork <- function(community_assignment, network) {
  if(!is(community_assignment, "CommunityAssignment")) stop("wrong class")
  if(!is(network, "Network")) stop("wrong class")
  
  new("Multinetwork",
      state = initialize_state_M(community_assignment, network),
      update_state = initialize_update_state_M(community_assignment, network)
  )
}


initialize_state_M <- function(community_assignment, network) {
  if(length(assignment(community_assignment)) != number_relations(network)) stop("error")
  
  state <- rlang::new_environment()
  state$statistics_communities <- hash::hash()
  state$global_statistics <- rlang::new_environment()
  
  maxpop <- 0L
  for(i in label_active_communities(community_assignment)) {
    state$statistics_communities[[as.character(i)]] <- compute_statistics_subnetwork(network, location_selected_communities_in_assignment(community_assignment, i))
    maxpop <- max(maxpop, state$statistics_communities[[as.character(i)]]$size_population)
  }
  state$global_statistics$size_largest_population <- maxpop
  
  return(state)
}  

initialize_update_state_M <- function(community_assignment, network) list(
  degrees = function(community, keys_degrees, new_degrees, object) {
    for(i in seq_along(keys_degrees)) object@state$statistics_communities[[as.character(community)]]$degrees[[as.character(keys_degrees[i])]] <- zero_to_null(new_degrees[[i]])
    return(invisible())
  },
  degree_frequencies = function(community, keys_degree_frequencies, variations, object) {
    for(i in seq_along(keys_degree_frequencies)) {
      freq_i <- null_to_zero(object@state$statistics_communities[[as.character(community)]]$degree_frequencies[[as.character(keys_degree_frequencies[i])]])
      object@state$statistics_communities[[as.character(community)]]$degree_frequencies[[as.character(keys_degree_frequencies[i])]] <- zero_to_null(freq_i + variations[[i]])
      rm(freq_i)
    }
    return(invisible())
  },
  arity_frequencies = function(community, keys_arities, new_arity_frequencies, object) {
    for(i in seq_along(keys_arities)) object@state$statistics_communities[[as.character(community)]]$arity_frequencies[[as.character(keys_arities[i])]] <- zero_to_null(new_arity_frequencies[[i]])
    return(invisible())
  },
  total_arity = function(community, new_total_arity, object) {
    object@state$statistics_communities[[as.character(community)]]$total_arity <- new_total_arity
    return(invisible())
  },
  size_population = function(community, new_size_population, object) {
    object@state$statistics_communities[[as.character(community)]]$size_population <- new_size_population
    return(invisible())
  },
  size_largest_population = function(new_size_largest_population, object) {
    object@state$global_statistics$size_largest_population <- new_size_largest_population
    return(invisible())
  },
  initialize_empty_community = function(label_new_community, object) {
    if(community_is_active(object, label_new_community)) stop("error")
    object@state$statistics_communities[[as.character(label_new_community)]] <- rlang::new_environment()
    ne <- object@state$statistics_communities[[as.character(label_new_community)]]
    ne$degrees <- hash::hash()
    ne$degree_frequencies <- hash::hash()
    ne$size_population <- 0L
    ne$arity_frequencies  <- hash::hash()
    ne$total_arity <- 0L
    rm(ne)
    return(invisible())
  },
  remove_empty_community = function(label_community_to_be_removed, object) {
    if(!community_is_empty(object, as.character(label_community_to_be_removed))) stop("error")
    object@state$statistics_communities[[as.character(label_community_to_be_removed)]] <- NULL
    return(invisible())
  }
)


setMethod("label_active_communities", "Multinetwork", function(object) { hash::keys(object@state$statistics_communities) })
setMethod("community_is_active", "Multinetwork", function(object, community) community %in% hash::keys(object@state$statistics_communities))

setGeneric("community_is_empty", function(object, label_community) standardGeneric("community_is_empty"))
setMethod("community_is_empty", "Multinetwork", function(object, label_community) {
  if(length(object@state$statistics_communities[[as.character(label_community)]]$degrees) != 0L) return(FALSE)
  if(length(object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies) != 0L) return(FALSE)
  if(length(object@state$statistics_communities[[as.character(label_community)]]$arity_frequencies) != 0L) return(FALSE)
  if(null_to_zero(object@state$statistics_communities[[as.character(label_community)]]$size_population) != 0L) return(FALSE)
  if(null_to_zero(object@state$statistics_communities[[as.character(label_community)]]$total_arity) != 0L) return(FALSE)
  return(TRUE)
})

setGeneric("degrees", function(object, label_community, label_actors, mode_returned_vector = NA) standardGeneric("degrees"))
setMethod("degrees", "Multinetwork", function(object, label_community, label_actors, mode_returned_vector = "integer") {
  extract_frequencies_hashmultiset(object@state$statistics_communities[[as.character(label_community)]]$degrees, keys = as.character(label_actors), mode_returned_vector)
})
setGeneric("degree_frequencies", function(object, label_community, label_degrees, mode_returned_vector = NA) standardGeneric("degree_frequencies"))
setMethod("degree_frequencies", "Multinetwork", function(object, label_community, label_degrees, mode_returned_vector = "integer") {
  extract_frequencies_hashmultiset(object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies, keys = as.character(label_degrees), mode_returned_vector) 
})
setGeneric("arity_frequencies", function(object, label_community, label_arities, mode_returned_vector = NA) standardGeneric("arity_frequencies"))
setMethod("arity_frequencies", "Multinetwork", function(object, label_community, label_arities, mode_returned_vector = "integer") {
  extract_frequencies_hashmultiset(object@state$statistics_communities[[as.character(label_community)]]$arity_frequencies, keys = as.character(label_arities), mode_returned_vector) 
})


setGeneric("pair_degrees_with_frequencies", function(object, label_community) standardGeneric("pair_degrees_with_frequencies"))
setMethod("pair_degrees_with_frequencies", "Multinetwork", function(object, label_community) {
  list(
    degrees = as.integer(hash::keys(  object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies)),
    degree_frequencies = hash::values(object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies)
  )
})
setGeneric("pair_arities_with_frequencies", function(object, label_community) standardGeneric("pair_arities_with_frequencies"))
setMethod("pair_arities_with_frequencies", "Multinetwork", function(object, label_community) {
  list(
    arities = as.integer(hash::keys( object@state$statistics_communities[[as.character(label_community)]]$arity_frequencies)),
    arity_frequencies = hash::values(object@state$statistics_communities[[as.character(label_community)]]$arity_frequencies)
  )
})


setMethod("size_population", "Multinetwork", function(object, label_community) null_to_zero(object@state$statistics_communities[[as.character(label_community)]]$size_population))
setGeneric("total_arity", function(object, label_community) standardGeneric("total_arity"))
setMethod("total_arity", "Multinetwork", function(object, label_community) null_to_zero(object@state$statistics_communities[[as.character(label_community)]]$total_arity))

setGeneric("size_largest_population", function(object) standardGeneric("size_largest_population"))
setMethod("size_largest_population", "Multinetwork", function(object) object@state$global_statistics$size_largest_population)

setGeneric("compute_proposed_statistics_after_flip", function(object, relation, old_community, new_community, network) standardGeneric("compute_proposed_statistics_after_flip"))
setMethod("compute_proposed_statistics_after_flip", "Multinetwork", function(object, relation, old_community, new_community, network) {
  if(old_community == new_community) stop("error")
  old_community <- as.character(old_community); new_community <- as.character(new_community)
  
  proposed <- list()
  proposed$info_flip <- list(relation = relation, old_community = old_community, new_community = new_community)
  #names(proposed$info_flip) <- c("relation", "old_community", "new_community")
  
  hashmultiset_degrees <- degrees_individual_relations(network, relation)[[1]]
  proposed$keys_degrees_relation <- hash::keys(hashmultiset_degrees)
  values_degrees_relation <- hash::values(hashmultiset_degrees)
  
  current_degrees_old_community <- degrees(object, old_community, proposed$keys_degrees_relation, "integer")
  current_degrees_new_community <- degrees(object, new_community, proposed$keys_degrees_relation, "integer")
  proposed$degrees_old_community <- compute_reduced_degrees_after_removal_subnetwork(current_degrees_old_community, values_degrees_relation)
  proposed$degrees_new_community <- compute_increased_degrees_after_inclusion_subnetwork(current_degrees_new_community, values_degrees_relation)
  
  proposed$size_population_leaving_old_community <- sum(proposed$degrees_old_community == 0L)
  proposed$size_population_joining_new_community <- sum(current_degrees_new_community  == 0L)
  
  proposed$size_population_old_community <- -proposed$size_population_leaving_old_community + size_population(object, old_community)
  proposed$size_population_new_community <-  proposed$size_population_joining_new_community + size_population(object, new_community)
  
  proposed$keys_degree_frequencies_old_community <- compute_keys_degree_frequencies_after_proposed_community_changes(current_degrees_old_community, proposed$degrees_old_community)
  proposed$keys_degree_frequencies_new_community <- compute_keys_degree_frequencies_after_proposed_community_changes(current_degrees_new_community, proposed$degrees_new_community)
  proposed$variation_degree_frequencies_old_community <- compute_variation_degree_frequencies_after_proposed_community_changes(proposed$keys_degree_frequencies_old_community, as.character(current_degrees_old_community), as.character(proposed$degrees_old_community), "integer")
  proposed$variation_degree_frequencies_new_community <- compute_variation_degree_frequencies_after_proposed_community_changes(proposed$keys_degree_frequencies_new_community, as.character(current_degrees_new_community), as.character(proposed$degrees_new_community), "integer")
  
  proposed$arity_relation <- arities_individual_relations(network, relation)[[1]]
  proposed$arity_frequencies_old_community <- -1L + arity_frequencies(object, old_community, as.character(proposed$arity_relation), "integer")
  proposed$arity_frequencies_new_community <-  1L + arity_frequencies(object, new_community, as.character(proposed$arity_relation), "integer")
  #proposed$arity_frequencies_new_community <-  1L + null_to_zero(arity_frequencies(object, new_community, as.character(proposed$arity_relation), "integer"))
  
  proposed$total_arity_old_community <- -proposed$arity_relation + total_arity(object, old_community)
  proposed$total_arity_new_community <-  proposed$arity_relation + total_arity(object, new_community)
  
  proposed$size_largest_population <- compute_size_largest_population_after_community_flip(object, old_community, new_community, proposed$size_population_old_community, proposed$size_population_new_community)

  return(proposed)
})

setMethod("set_state_after_flip", "Multinetwork", function(object, proposed_statistics) {
  new_community <- proposed_statistics$info_flip$new_community
  old_community <- proposed_statistics$info_flip$old_community
  
  if(!community_is_active(object, new_community)) object@update_state$initialize_empty_community(as.character(new_community), object)
  
  object@update_state$degrees(old_community, proposed_statistics$keys_degrees_relation, proposed_statistics$degrees_old_community, object)
  object@update_state$degrees(new_community, proposed_statistics$keys_degrees_relation, proposed_statistics$degrees_new_community, object)
  
  object@update_state$degree_frequencies(old_community, proposed_statistics$keys_degree_frequencies_old_community, proposed_statistics$variation_degree_frequencies_old_community, object)
  object@update_state$degree_frequencies(new_community, proposed_statistics$keys_degree_frequencies_new_community, proposed_statistics$variation_degree_frequencies_new_community, object)
  
  object@update_state$arity_frequencies(old_community, proposed_statistics$arity_relation, proposed_statistics$arity_frequencies_old_community, object)
  object@update_state$arity_frequencies(new_community, proposed_statistics$arity_relation, proposed_statistics$arity_frequencies_new_community, object)
  
  object@update_state$total_arity(old_community, proposed_statistics$total_arity_old_community, object)
  object@update_state$total_arity(new_community, proposed_statistics$total_arity_new_community, object)
  
  object@update_state$size_population(old_community, proposed_statistics$size_population_old_community, object)
  object@update_state$size_population(new_community, proposed_statistics$size_population_new_community, object)
  
  if(community_is_empty(object, old_community)) object@update_state$remove_empty_community(as.character(old_community), object)
  
  object@update_state$size_largest_population(proposed_statistics$size_largest_population, object)
  
  return(invisible())
})





compute_statistics_subnetwork <- function(network, relations_in_subnetwork, return_environment = TRUE) {
  if(return_environment) ne <- rlang::new_environment()
  else ne <- list()
  
  ne$degrees <- compute_degrees_combining_degrees_individual_relations(degrees_individual_relations(network, relations_in_subnetwork))
  ne$degree_frequencies <- create_hashmultiset(hash::values(ne$degrees))
  ne$size_population <- length(ne$degrees)
  
  ne$arity_frequencies  <- create_hashmultiset(unlist(arities_individual_relations(network, relations_in_subnetwork)))
  ne$total_arity <- sum(unlist(arities_individual_relations(network, relations_in_subnetwork)))
  return(ne)
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



compute_reduced_degrees_after_removal_subnetwork <- function(current_degrees, degrees_subnetwork) {
  reduced_degrees <- vector(typeof(current_degrees), length(current_degrees))
  for(i in seq_along(current_degrees)) reduced_degrees[[i]] <- current_degrees[[i]] - degrees_subnetwork[[i]]
  return(reduced_degrees)
}

compute_increased_degrees_after_inclusion_subnetwork <- function(current_degrees, degrees_subnetwork) {
  increased_degrees <- vector(typeof(current_degrees), length(current_degrees))
  for(i in seq_along(current_degrees)) increased_degrees[[i]] <- current_degrees[[i]] + degrees_subnetwork[[i]]
  return(increased_degrees)
}

compute_keys_degree_frequencies_after_proposed_community_changes <- function(current_degrees, proposed_degrees) {
  keys <- sort(unique(c(current_degrees,proposed_degrees)))
  return(as.character(keys[which(keys >= 1L)]))
}

compute_variation_degree_frequencies_after_proposed_community_changes <- function(keys_frequencies_to_be_modified, label_current_degrees, label_proposed_degrees, mode_returned_vector = "integer") { 
  variation <- vector("integer", length(keys_frequencies_to_be_modified))
  names(variation) <- keys_frequencies_to_be_modified
  for(i in seq_along(label_current_degrees))  if(label_current_degrees [[i]] != 0L) variation[[as.character(label_current_degrees [[i]])]] <- variation[[as.character(label_current_degrees [[i]])]] - 1L
  for(i in seq_along(label_proposed_degrees)) if(label_proposed_degrees[[i]] != 0L) variation[[as.character(label_proposed_degrees[[i]])]] <- variation[[as.character(label_proposed_degrees[[i]])]] + 1L
  mode(variation) <- mode_returned_vector
  return(unname(variation))
}

compute_size_largest_population_after_community_flip <- function(multinetwork, old_community, new_community, proposed_size_old_community, proposed_size_new_community) {
  lac <- hash::keys(multinetwork@state$statistics_communities)
  current_size_communities <- sapply(lac, function(x) size_population(multinetwork, x))
  names(current_size_communities) <- lac
  current_size_communities[[as.character(old_community)]] <- proposed_size_old_community
  current_size_communities[[as.character(new_community)]] <- proposed_size_new_community
  return(max(unlist(current_size_communities)))
}







