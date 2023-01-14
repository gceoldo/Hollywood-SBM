setClass("ChainCommunity", slots = c(state = "environment", update_state = "list"))

ChainCommunity <- function(length_chain) {
  state <- rlang::new_environment()
  state$chain <- data.frame(
    type_update = vector("character", length_chain),
    relation = vector("integer", length_chain),
    old_community = vector("integer", length_chain),
    new_community = vector("integer", length_chain),
    accepted = vector("logical", length_chain))
  state$first_empty_observation <- 1L
  
  update_state <- list()
  update_state$flip = function(chain_community, info_proposed_flip) { 
    for(i in names(chain_community@state$chain)) chain_community@state$chain[chain_community@state$first_empty_observation, i] <- info_proposed_flip[[i]] 
    chain_community@state$first_empty_observation <- chain_community@state$first_empty_observation + 1L
    return(invisible(NULL))
  }
  
  new("ChainCommunity", state = state, update_state = update_state)
}

setGeneric("store_proposed_flip", function(chain_community, info_proposed_flip) standardGeneric("store_proposed_flip"))
setMethod("store_proposed_flip", "ChainCommunity", function(chain_community, info_proposed_flip) {
  if(!proposed_flip_is_valid(info_proposed_flip)) stop("error")
  chain_community@update_state$flip(chain_community, info_proposed_flip)
  return(invisible(NULL))
})

proposed_flip_is_valid <- function(info_proposed_flip) {
  if(!is.list(info_proposed_flip)) return(FALSE)
  if(length(info_proposed_flip) != 5L) return(FALSE)
  if(!all(names(info_proposed_flip) == c("type_update", "relation", "old_community", "new_community", "accepted"))) return(FALSE)
  if(!all(sapply(info_proposed_flip, typeof) == c("character", "integer", "integer", "integer", "logical"))) return(FALSE)
  return(TRUE)
}

setGeneric("recover_assignment", function(chain_community, assignment_vector, number_iterations, backward = TRUE) standardGeneric("recover_assignment"))
setMethod("recover_assignment", c(chain_community = "ChainCommunity"), function(chain_community, assignment_vector, number_iterations, backward = TRUE) {
  if(  backward  & (number_iterations > chain_community@state$first_empty_observation)) stop("error1")
  if((!backward) & (number_iterations > nrow(chain_community@state$chain))) stop("error2")
  
  begin_loop <- c(chain_community@state$first_empty_observation, 0)[2-backward]
  for(i in 1:number_iterations) assignment_vector <- flip_community(assignment_vector, chain_community, begin_loop + i*c(-1,1)[2-backward], backward)
  
  return(assignment_vector)
})

flip_community <- function(assignment_vector, chain_community, iteration, backward) {
  if(chain_community@state$chain[iteration, "type_update"] != ("flip" | "uflip")) return(assignment_vector) 
  if(chain_community@state$chain[iteration, "accepted"]) {
    relation <- chain_community@state$chain[iteration, "relation"]
    community <- chain_community@state$chain[iteration, c("old_community", "new_community")[2-backward]]
    assignment_vector[relation] <- community  
  }
  return(assignment_vector)
}



