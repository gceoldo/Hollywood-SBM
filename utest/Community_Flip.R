library(hash)
library(testthat)

message("generic functions defined in other units:")
# set relations
setGeneric(name = "number_relations", def = function(object) standardGeneric("number_relations"))
# community assignment
setGeneric(name = "label_active_communities", def = function(object) standardGeneric("label_active_communities"))
setGeneric(name = "size_selected_communities", def = function(object, which_communities, mode_returned_vector) standardGeneric("size_selected_communities"))
setGeneric(name = "assignment", def = function(object) standardGeneric("assignment"))
setGeneric(name = "location_selected_communities_in_assignment", def = function(object, which_communities) standardGeneric("location_selected_communities_in_assignment"))
setGeneric(name = "community_is_active", function(object, community) standardGeneric("community_is_active"))
setGeneric(name = "select_assignment_from_relations", def = function(object, relations) standardGeneric("select_assignment_from_relations"))
setGeneric("set_state_after_flip", function(object, proposed_statistics) standardGeneric("set_state_after_flip"))
# network
setGeneric("size_population", function(object, label_community) standardGeneric("size_population"))
setGeneric("degrees_individual_relations",      def = function(object, which_relations) standardGeneric("degrees_individual_relations"))
setGeneric("arities_individual_relations",      def = function(object, which_relations) standardGeneric("arities_individual_relations"))
# multinetwork
setGeneric("compute_proposed_statistics_after_flip", function(object, relation, old_community, new_community, network) standardGeneric("compute_proposed_statistics_after_flip"))
setGeneric("degrees", function(object, label_community, label_actors, mode_returned_vector = NA) standardGeneric("degrees"))
setGeneric("degree_frequencies", function(object, label_community, label_degrees, mode_returned_vector = NA) standardGeneric("degree_frequencies"))
setGeneric("arity_frequencies", function(object, label_community, label_arities, mode_returned_vector = NA) standardGeneric("arity_frequencies"))
setGeneric("total_arity", function(object, label_community) standardGeneric("total_arity"))
setGeneric("size_largest_population", function(object) standardGeneric("size_largest_population"))
# parameter
setGeneric("strength", function(object, label_community) standardGeneric("strength"))
setGeneric("discount", function(object, label_community) standardGeneric("discount"))
setGeneric("overlap", function(object) standardGeneric("overlap"))

# prototypes classes and methods to whom Community_Flip depends on
setClass("CommunityAssignment", slots = c(state = "environment", update_state = "list"))
CommunityAssignment <- function(assignment) {
  initialize_update_state_CA <- function() {
      update_number_communities <- function(new_number_community, object) stop("number of community is fixed")
      include_label_active_communities <- function(new_community, object) stop("number of community is fixed")
      exclude_label_active_communities <- function(rm_community, object)  stop("number of community is fixed")
      set_first_inactive_community <- function(new_empty_community, object) stop("number of community is fixed")
      add_size_community <- function(new_size, label_new_community, object) stop("number of community is fixed")
      rm_size_community <- function(label_removed_community, object) stop("number of community is fixed")
      add_location_relations_in_new_community <- function(relations, new_community, object) stop("number of community is fixed")
      rm_community_from_locations <- function(removed_community, object) stop("number of community is fixed")
    update_size_communities <- function(new_sizes, communities, object) { object@state$table_sizes[as.character(communities)] <- new_sizes ; return(invisible()) }
    set_assignment <- function(new_assignment, relations, object) { object@state$assignment[relations] <- new_assignment ; return(invisible()) }
    update_location_communities_in_assignment <- function(relations, old_community, new_community, object) {
      remove_elements_from_hashset(object@state$location_communities_in_assignment[[as.character(old_community)]], relations)
      include_elements_to_hashset (object@state$location_communities_in_assignment[[as.character(new_community)]], relations)
      return(invisible())
    }
    return(list(
      update_number_communities = update_number_communities,
      include_label_active_communities = include_label_active_communities,
      exclude_label_active_communities = exclude_label_active_communities,
      set_first_inactive_community = set_first_inactive_community,
      add_size_community = add_size_community,
      rm_size_community = rm_size_community,
      add_location_relations_in_new_community = add_location_relations_in_new_community,
      rm_community_from_locations = rm_community_from_locations,
      update_size_communities = update_size_communities,
      set_assignment = set_assignment,
      update_location_communities_in_assignment = update_location_communities_in_assignment,
      community_is_active = community_is_active
    ))
  }
  assignment <- as.integer(assignment)
  label_active_communities <- sort(unique(assignment))
  table_sizes <- table(assignment)
  location_communities_in_assignment <- hash::hash()
  for(i in label_active_communities) location_communities_in_assignment[i] <- create_hashset(which(assignment == i))
  new("CommunityAssignment", state = rlang::new_environment(list(assignment = assignment, label_active_communities = label_active_communities, location_communities_in_assignment = location_communities_in_assignment, table_sizes = table_sizes)),
                             update_state = initialize_update_state_CA())
}
setClass("Network", slots = c(individual_statistics = "list", global_statistics = "list"))
Network <- function(set_relations) {
  compute_individual_statistics <- function(relation) { 
    stats <- vector("list", 2)
    names(stats) <- c("degrees", "arity")
    stats$degrees <- compute_hashmultiset_degrees_from_relation(relation)
    stats$arity <- length(relation)
    return(stats) 
  } 
  compute_hashmultiset_degrees_from_relation <- function(relation) { 
    degrees <- hash::hash()
    for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
    return(degrees) 
  } 
  individual_statistics <- vector("list", length(set_relations))
  for(i in 1:length(set_relations)) individual_statistics[[i]] <- compute_individual_statistics(set_relations[[i]])
  names(individual_statistics) <- as.character(1:length(set_relations))
  global_statistics <- list(size_population = length(unique(unlist(set_relations))))
  new("Network", individual_statistics = individual_statistics, global_statistics = global_statistics)
}
setClass("Parameter", slots = c(state = "environment", update_state = "list", name_local_parameters  = "character"))
Parameter <- function(community_assignment, name_local_parameters, name_global_parameters, initial_values) {
  state <- rlang::new_environment()
  state$local_parameters  <- hash::hash()
  state$global_parameters <- vector("list", length(name_global_parameters))
  names(state$global_parameters) <- name_global_parameters
  
  for(i in label_active_communities(community_assignment)) {
    state$local_parameters[[as.character(i)]] <- vector("list", length(name_local_parameters))
    names(state$local_parameters[[as.character(i)]]) <- name_local_parameters
    for(j in seq_along(name_local_parameters)) state$local_parameters[[as.character(i)]][[name_local_parameters[j]]] <- unname(initial_values[[name_local_parameters[j]]][as.character(i)])
  }
  for(j in seq_along(name_global_parameters)) state$global_parameters[[name_global_parameters[j]]] <- initial_values[[name_global_parameters[j]]]
  
  new("Parameter", state = state, name_local_parameters  = name_local_parameters)
}
setClass("Multinetwork", slots = c(state = "environment", update_state = "list"))
Multinetwork <- function(community_assignment, network) {
  compute_statistics_subnetwork <- function(network, relations_in_subnetwork) {
    ne <- rlang::new_environment()
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
  initialize_update_state_M <- function(community_assignment, network) { list(
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
    }
  )}
  new("Multinetwork", state = initialize_state_M(community_assignment, network),
      update_state = initialize_update_state_M(community_assignment, network))
}



source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/Community_Flip.R")
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/generative_model.R")




setMethod( "degrees_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$degrees))
setMethod( "arities_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$arity))
setMethod("size_population", "Network", function(object, label_community) object@global_statistics$size_population)
setMethod("number_relations", "Network", function(object) length(object@individual_statistics))

setMethod("number_relations", "CommunityAssignment", function(object) length(object@state$assignment))
setMethod("label_active_communities", "CommunityAssignment", function(object) object@state$label_active_communities)
setMethod("size_selected_communities", "CommunityAssignment", function(object, which_communities, mode_returned_vector) {
  if(missing(mode_returned_vector)) mode_returned_vector <- "integer"
  sizes <- vector(mode=mode_returned_vector, length = length(which_communities))
  for(i in seq_along(which_communities)) sizes[[i]] <- null_to_zero(object@state$table_sizes[[as.character(which_communities[i])]])
  return(sizes)
})
setMethod("assignment", "CommunityAssignment", function(object) object@state$assignment)
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
setMethod("select_assignment_from_relations", "CommunityAssignment", function(object, relations) object@state$assignment[relations])
setMethod("set_state_after_flip", "CommunityAssignment", function(object, proposed_statistics) {
  relation <- as.integer(proposed_statistics$info_flip$relation)
  new_community <- as.integer(proposed_statistics$info_flip$new_community)
  old_community <- as.integer(proposed_statistics$info_flip$old_community)
  
  new_sizes <- size_selected_communities(object, c(old_community, new_community))  + c(-1L, 1L)*(old_community != new_community)
  
  object@update_state$set_assignment(new_community, relation, object)
  object@update_state$update_size_communities(new_sizes, c(old_community, new_community), object)
  object@update_state$update_location_communities_in_assignment(relation, old_community, new_community, object)
  
  return(invisible())
})

setMethod("degrees", "Multinetwork", function(object, label_community, label_actors, mode_returned_vector = "integer") {
  extract_frequencies_hashmultiset(object@state$statistics_communities[[as.character(label_community)]]$degrees, keys = as.character(label_actors), mode_returned_vector) 
})
setMethod("degree_frequencies", "Multinetwork", function(object, label_community, label_degrees, mode_returned_vector = "integer") {
  extract_frequencies_hashmultiset(object@state$statistics_communities[[as.character(label_community)]]$degree_frequencies, keys = as.character(label_degrees), mode_returned_vector) 
})
setMethod("arity_frequencies", "Multinetwork", function(object, label_community, label_arities, mode_returned_vector = "integer") {
  extract_frequencies_hashmultiset(object@state$statistics_communities[[as.character(label_community)]]$arity_frequencies, keys = as.character(label_arities), mode_returned_vector) 
})
setMethod("total_arity", "Multinetwork", function(object, label_community) object@state$statistics_communities[[as.character(label_community)]]$total_arity)
setMethod("size_largest_population", "Multinetwork", function(object) object@state$global_statistics$size_largest_population)
setMethod("size_population", "Multinetwork", function(object, label_community) object@state$statistics_communities[[as.character(label_community)]]$size_population)
setMethod("compute_proposed_statistics_after_flip", "Multinetwork", function(object, relation, old_community, new_community, network) {
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
  
  if(old_community == new_community) stop("error")
  old_community <- as.character(old_community); new_community <- as.character(new_community)
  
  proposed <- list()
  proposed$info_flip <- list(relation = relation, old_community = old_community, new_community = new_community)
  
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
  proposed$arity_frequencies_old_community <- -1L +              arity_frequencies(object, old_community, as.character(proposed$arity_relation), "integer")
  proposed$arity_frequencies_new_community <-  1L + null_to_zero(arity_frequencies(object, new_community, as.character(proposed$arity_relation), "integer"))
  
  proposed$total_arity_old_community <- -proposed$arity_relation + total_arity(object, old_community)
  proposed$total_arity_new_community <-  proposed$arity_relation + total_arity(object, new_community)
  
  proposed$size_largest_population <- compute_size_largest_population_after_community_flip(object, old_community, new_community, proposed$size_population_old_community, proposed$size_population_new_community)
  
  return(proposed)
})
setMethod("set_state_after_flip", "Multinetwork", function(object, proposed_statistics) {
  new_community <- proposed_statistics$info_flip$new_community
  old_community <- proposed_statistics$info_flip$old_community
  
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
  
  
  object@update_state$size_largest_population(proposed_statistics$size_largest_population, object)
  
  return(invisible())
})

setMethod("label_active_communities", "Parameter", function(object) hash::keys(object@state$local_parameters))
setMethod("strength", "Parameter", function(object, label_community) {
  if("strength" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$strength)
  else return(object@state$global_parameters$strength)
})
setMethod("discount", "Parameter", function(object, label_community) {
  if("discount" %in% object@name_local_parameters) return(object@state$local_parameters[[as.character(label_community)]]$discount)
  else return(object@state$global_parameters$discount)
})
setMethod("overlap", "Parameter", function(object) { return(object@state$global_parameters$overlap) })


# functions from utils
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
  expect_s4_class(object = CommunityFlip(), class = "CommunityFlip")
  
  f <- function(x) x
  list_of_f <- function(names_functions) {
    returned_list <- vector("list", length(names_functions))
    for(i in 1:length(names_functions)) returned_list[[i]] <- f
    names(returned_list) <- names_functions
    returned_list
  } 
  
  expect_s4_class(CommunityFlip(
    proposal = list_of_f(c("get_relations_for_which_flip_is_allowed", "sample_relation", "sample_proposed_flip")),
    evaluation = list_of_f(c("proposed_flip_is_skipped", "compute_proposed_statistics", "get_current_statistics", "compute_MH_logratio")),
    update_state_process = list_of_f(c("flip_is_accepted", "community_assignment", "multinetwork")),
    store_info = list()
  ), class = "CommunityFlip")
  
  expect_s4_class(CommunityFlip(
    update_state_process = list_of_f(c("flip_is_accepted", "community_assignment", "multinetwork"))
  ), class = "CommunityFlip")
  
  expect_error(CommunityFlip( # wrong length
    proposal = list_of_f(c("sample_relation", "get_relations_for_which_flip_is_allowed")),
  ))
  
  expect_error(CommunityFlip( # switched names
    proposal = list_of_f(c("sample_relation", "get_relations_for_which_flip_is_allowed", "sample_proposed_flip")),
  ))
  
})


message("test default logratios")
test_that("test default logratios", {
  logdhollywood <- function(set_relations, alpha, theta) {
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
    
    v <- length(unique(unlist(set_relations)))
    m <- sum(sapply(set_relations, length))
    deg <- compute_degrees_combining_degrees_individual_relations(lapply(set_relations, compute_hashmultiset_degrees_from_relation))
    unique_deg <- as.integer(sort(unique(hash::values(deg))))
    deg_freq <- as.integer(table(hash::values(deg)))
    
    return(v*log(alpha) + lgamma(theta/alpha + v) - lgamma(theta/alpha) - lgamma(theta + m) + lgamma(theta)
           + sum( deg_freq * (lgamma(unique_deg - alpha) - lgamma(1 - alpha))))
  }
  logdmerge <- function(list_networks, overlap) {
    population_sizes <- as.integer(lapply(list_networks, function(x) length(unique(unlist(x)))))
    maximum_size_population <- max(population_sizes)
    total_size_population <- length(unique(unlist(list_networks)))
    product <- prod(as.double(lapply(population_sizes, function(x) 1-x/(maximum_size_population + overlap))))
    
    return(log(dbinom(maximum_size_population + overlap - total_size_population, maximum_size_population + overlap, product)))
  }
  logdprior <- function(assignment) {
    #active_communities <- sort(unique(assignment))
    sizes_communities <- as.integer(table(assignment))
    return(
      + sum(lgamma(sizes_communities + 1)) - lgamma(length(assignment) + 1)
      + lgamma(length(assignment)) + lgamma(length(assignment) - length(sizes_communities) - 1) - lgamma(length(assignment))
      - log(length(assignment))
    )
  }
  
  
  number_tests <- 3L
  for(n in 1:number_tests) {
    ass <- as.integer(c(2,2,1,1,1,4,3,3,4,2,1))
    set_relations <- sample_HBM(ass, .7, .1, 12L, function() 2L+rpois(1,3))
    
    community_assignment <- CommunityAssignment(ass)
    network <- Network(set_relations)
    multinetwork <- Multinetwork(community_assignment, network)
    parameter <- Parameter(community_assignment, character(0), c("strength", "discount", "overlap"), list(strength = .7, discount = .1, overlap = 12L))
    
    for(i in 1:length(ass)) {
      ass2 <- ass
      current_active_communities <- unique(ass)
      ass2[i] <- resample(current_active_communities[which(current_active_communities != ass[i])],1)
      proposed_statistics <- compute_proposed_statistics_after_flip(multinetwork, i, as.character(ass[i]), as.character(ass2[i]), network)
      list_networks <- list_networks2 <- vector("list", length(current_active_communities))
      for(j in seq_along(current_active_communities)) list_networks[[j]]  <- set_relations[which(ass  == current_active_communities[j])]
      for(j in seq_along(current_active_communities)) list_networks2[[j]] <- set_relations[which(ass2 == current_active_communities[j])]
      
      # test logprior
      expect_equal(default_logratiosCF$prior_communities(community_assignment, as.character(ass[i]), as.character(ass2[i])),
                   logdprior(ass2) - logdprior(ass))
      
      # test logratio network
      current_logdensity <- lapply(sort(unique(ass)), function(x) logdhollywood(set_relations[which(ass == x)], strength(parameter), discount(parameter)))
      proposed_logdensity <- lapply(sort(unique(ass2)), function(x) logdhollywood(set_relations[which(ass2 == x)], strength(parameter), discount(parameter)))
      expect_equal(default_logratiosCF$network(multinetwork, parameter, as.character(ass[i]), as.character(ass2[i]), proposed_statistics),
                   sum(unlist(proposed_logdensity) - unlist(current_logdensity)))
      
      # test logratio merge
      current_logdensity <- logdmerge(list_networks, 12L)
      proposed_logdensity <- logdmerge(list_networks2, 12L)
      expect_equal(default_logratiosCF$merge(community_assignment, multinetwork, parameter, network, as.character(ass[i]), as.character(ass2[i]), proposed_statistics),
                   proposed_logdensity - current_logdensity)
      
    }
  }
})

message("test components MH step")
test_that("test components MH step", {
  relabel_assignment <- function(assignment) {
    old_labels <- sort(unique(assignment))
    new_labels <- 1:length(old_labels)
    for(i in 1:length(assignment)) assignment[i] <- new_labels[which(assignment[i] == old_labels)]
    return(assignment)
  }
  
  number_trials <- 200L
  
  community_flip <- CommunityFlip()
  
  counter <- 0L
  for(i in 1:number_trials) {
    # table(replicate(10000, min(table(as.integer(runif(12, 1, 5))))))/10000
    # prob 0.45 communities with one component
    # prob < 0.0001 all equal
    
    ass <- relabel_assignment(as.integer(runif(12, 1, 5)))
    set_relations <- sample_HBM(ass, .7, .1, 11L, function() 2L+rpois(1,3))
    network <- Network(set_relations)
    community_assignment <- CommunityAssignment(ass)
    parameter <- Parameter(community_assignment, character(0), c("strength", "discount", "overlap"), list(strength = .7, discount = .1, overlap = 12L))
    multinetwork <- Multinetwork(community_assignment, network)
    
    output_proposal <- sample_community_flip(community_flip, community_assignment)
    eval <- evaluation(output_proposal, community_flip, community_assignment, multinetwork, parameter, network)
    if(sum(ass == ass[output_proposal$relation]) == 1L) { # sampled relation is of community of size 1
      expect_equal(output_proposal$old_community, output_proposal$new_community)
      expect_equal(-Inf, eval$logratio)
      expect_false(update_state_process(eval$logratio, eval$proposed_statistics, 0, community_flip, community_assignment, multinetwork))
      counter <- counter + 1L
    } 
    else {
      expect_true(output_proposal$old_community != output_proposal$new_community)
    }
  }  
  print(sprintf("sampled relation is of community of size 1 in %i/%i trials", counter, number_trials))
  
  
  
})

message("complete test MH step")
test_that("complete test MH step", {
  number_iterations <- 500L
  ass <- as.integer(c(2,2,1,1,1,4,3,3,3,2,1))
  ac <- sort(unique(ass))
  set_relations <- sample_HBM(ass, .7, .1, 12L, function() 2L+rpois(1,3))
  
  community_assignment <- CommunityAssignment(ass)
  network <- Network(set_relations)
  multinetwork <- Multinetwork(community_assignment, network)
  parameter <- Parameter(community_assignment, character(0), c("strength", "discount", "overlap"), list(strength = .7, discount = .1, overlap = 12L))
  community_flip <- CommunityFlip()
  
  for(i in 1:number_iterations) {
    #print(assignment(community_assignment))
    metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
    #expect_equal(ac, sort(unique(assignment(community_assignment))))
  }  
  expect_equal(ac, sort(unique(assignment(community_assignment))))
  
  
  ass_final <- assignment(community_assignment)
  mn_final <- Multinetwork(community_assignment, network)
  
  for(i in ac) {
    expect_equal(degrees(multinetwork, i, 1:30), degrees(mn_final, i, 1:30))
    expect_equal(degree_frequencies(multinetwork, i, 1:30), degree_frequencies(mn_final, i, 1:30))
    expect_equal(arity_frequencies(multinetwork, i, 1:10), arity_frequencies(mn_final, i, 1:10))
    expect_equal(total_arity(multinetwork, i), total_arity(mn_final, i))
    expect_equal(size_population(multinetwork, i), size_population(mn_final, i))
  }
  expect_equal(size_largest_population(multinetwork), size_largest_population(mn_final))
  
})





