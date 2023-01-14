setClass("Parameter", slots = c(
  state = "environment", 
  update_state = "list",
  name_local_parameters  = "character"
))

Parameter <- function(community_assignment, parametric_space, initial_values) {
  if(!is(community_assignment, "CommunityAssignment")) stop("wrong class")
  if(!is(parametric_space, "ParametricSpace")) stop("wrong class")
  if(!is(initial_values, "list")) stop("wrong class")
  
  new("Parameter",
      state = initialize_state_P(community_assignment, parametric_space, initial_values),
      update_state = initialize_update_state_P(community_assignment, parametric_space),
      name_local_parameters  = local_parameters(parametric_space)
  )
}

initialize_state_P <- function(community_assignment, parametric_space, initial_values) {
  state <- rlang::new_environment()
  
  name_local_parameters  <- local_parameters(parametric_space)
  name_global_parameters <- global_parameters(parametric_space)
  name_initial_values <- names(initial_values)
  
  initial_values <- check_valid_initial_values(initial_values, community_assignment, parametric_space)
  
  state$local_parameters  <- hash::hash()
  state$global_parameters <- vector("list", length(name_global_parameters))
  names(state$global_parameters) <- name_global_parameters
  
  for(i in label_active_communities(community_assignment)) {
    state$local_parameters[[as.character(i)]] <- vector("list", length(name_local_parameters))
    names(state$local_parameters[[as.character(i)]]) <- name_local_parameters
    for(j in seq_along(name_local_parameters)) state$local_parameters[[as.character(i)]][[name_local_parameters[j]]] <- unname(initial_values[[name_local_parameters[j]]][as.character(i)])
  }
  for(j in seq_along(name_global_parameters)) state$global_parameters[[name_global_parameters[j]]] <- initial_values[[name_global_parameters[j]]]
  
  return(state)
} 

initialize_update_state_P <- function(community_assignment, parametric_space) { list(
  update_local_parameter  = function(new_value, label_community, name_parameter, object) { object@state$local_parameters[[as.character(label_community)]][[as.character(name_parameter)]] <- new_value; return(invisible()) }, 
  update_global_parameter = function(new_value, name_parameter, object) { object@state$global_parameters[[as.character(name_parameter)]] <- new_value; return(invisible()) }
)} 





setMethod("label_active_communities", "Parameter", function(object) hash::keys(object@state$local_parameters))

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

setGeneric("set_strength", function(object, proposed_value, label_community) standardGeneric("set_strength"))
setMethod( "set_strength", "Parameter", function(object, proposed_value, label_community) {
  if(proposed_value >= 1) return("error")
  if("strength"   %in% object@name_local_parameters) {
    if(!(label_community %in% label_active_communities(object))) stop("error")
    if("discount" %in% object@name_local_parameters) { if(proposed_value <= max(0, -discount(object, label_community))) stop("error") }
    else { if(proposed_value <= max(0, -discount(object))) stop("error") }
    object@update_state$update_local_parameter(as.double(proposed_value[[1L]]), as.character(label_community), "strength", object)
  } 
  else {
    if("discount" %in% object@name_local_parameters) { if(proposed_value <= max(0, -sapply(label_active_communities(object), function(x) object@state$local_parameters[[x]]$discount))) stop("error") }
    else { if(proposed_value <= max(0, -discount(object))) stop("error") }
    object@update_state$update_global_parameter(proposed_value[[1L]], "strength", object)
  } 
  return(invisible())
})
setGeneric("set_discount", function(object, proposed_value, label_community) standardGeneric("set_discount"))
setMethod( "set_discount", "Parameter", function(object, proposed_value, label_community) {
  if("discount"   %in% object@name_local_parameters) {
    if(!(label_community %in% label_active_communities(object))) stop("error")
    if("strength" %in% object@name_local_parameters) { if(proposed_value <= -strength(object, label_community)) stop("error") }
    else { if(proposed_value <= -strength(object)) stop("error") }
    object@update_state$update_local_parameter(as.double(proposed_value[[1L]]), as.character(label_community), "discount", object)
  } 
  else {
    if("strength" %in% object@name_local_parameters) { if(proposed_value <= max(-sapply(label_active_communities(object), function(x) object@state$local_parameters[[x]]$strength))) stop("error") }
    else { if(proposed_value <= -strength(object)) stop("error") }
    object@update_state$update_global_parameter(proposed_value[[1L]], "discount", object)
  } 
  return(invisible())
})
setGeneric("set_overlap", function(object, proposed_value) standardGeneric("set_overlap"))
setMethod( "set_overlap", "Parameter", function(object, proposed_value) {
  if(proposed_value < 0) stop("error")
  object@update_state$update_global_parameter(as.integer(proposed_value[[1L]]), "overlap", object)
  return(invisible())
})

# generic defined in Parametric_Space
setMethod("local_parameters",  "Parameter", function(object) {
  key_first_active_community <- hash::keys(object@state$local_parameters)[1]
  return(names(object@state$local_parameters[[key_first_active_community]]))
})
setMethod("global_parameters", "Parameter", function(object) names(object@state$global_parameters))

# generic defined in Community_Assignment
setMethod("label_active_communities", "Parameter", function(object) hash::keys(object@state$local_parameters))





check_valid_initial_values <- function(initial_values, community_assignment, parametric_space) { 
  name_local_parameters  <- local_parameters(parametric_space)
  name_global_parameters <- global_parameters(parametric_space)
  
  if(length(initial_values) != (length(name_local_parameters) + length(name_global_parameters))) return("error")
  if(any(sort(names(initial_values)) != sort(c(name_local_parameters,name_global_parameters)))) return("error")
  for(i in seq_along(name_local_parameters)) {
    if(!is.numeric(initial_values[[name_local_parameters[i]]])) return("error")
    if(length(initial_values[[name_local_parameters[i]]]) != number_communities(community_assignment)) return("error")
    if(!all(sort(names(initial_values[[name_local_parameters[i]]])) == as.character(label_active_communities(community_assignment)))) return("error")
    mode(initial_values[[name_local_parameters[i]]]) <- type_parameter(parametric_space, name_local_parameters[i])
  }
  for(i in seq_along(name_global_parameters)) {
    if(!is.numeric(initial_values[[name_global_parameters[i]]]))  return("error")
    if(length(initial_values[[name_global_parameters[i]]]) != 1L) return("error")
    mode(initial_values[[name_global_parameters[i]]]) <- type_parameter(parametric_space, name_global_parameters[i])
  }
  
  if(any(initial_values[["strength"]] <= 0 | initial_values[["strength"]] >= 1)) stop("error")
  if(any(initial_values[["discount"]] <= -initial_values[["strength"]])) stop("error")
  
  return(initial_values) 
}

setGeneric("create_empty_parameter", def = function(community_assignment, parametric_space) standardGeneric("create_empty_parameter"))
setMethod("create_empty_parameter", c("CommunityAssignment", "ParametricSpace"), function(community_assignment, parametric_space) {
  name_local_parameters  <- local_parameters(parametric_space)
  name_global_parameters <- global_parameters(parametric_space)
  empty_parameter <- vector("list", length(name_local_parameters) + length(name_global_parameters))
  for(i in seq_along(name_local_parameters)) {
    empty_parameter[[i]] <- rep(NA, number_communities(community_assignment))
    names(empty_parameter[[i]]) <- as.character(label_active_communities(community_assignment))
    mode(empty_parameter[[i]]) <- type_parameter(parametric_space, name_local_parameters[i])
  }
  for(i in seq_along(name_global_parameters)) {
    empty_parameter[[length(name_local_parameters) + i]] <- NA
    mode(empty_parameter[[length(name_local_parameters) + i]]) <- type_parameter(parametric_space, name_global_parameters[i])
  }
  names(empty_parameter) <- c(name_local_parameters, name_global_parameters)
  return(empty_parameter)
})

