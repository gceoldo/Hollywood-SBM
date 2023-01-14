setClass("ChainParameter", slots = c(state = "environment", update_state = "function"))

ChainParameter <- function(length_chain) {
  if(length_chain < 1) stop("error")
  state <- rlang::new_environment()
  state$chain <- rep(NA, length_chain)
  state$first_empty_observation <- 1L
  
  update_state <- function(chain_parameter, value) { 
    chain_parameter@state$chain[chain_parameter@state$first_empty_observation] <- value
    chain_parameter@state$first_empty_observation <- chain_parameter@state$first_empty_observation + 1L
    return(invisible(NULL))
  }
  
  new("ChainParameter", state = state, update_state = update_state)
}

setGeneric("store_parameter", function(chain_parameter, current_value) standardGeneric("store_parameter"))
setMethod("store_parameter", "ChainParameter", function(chain_parameter, current_value) {
  chain_parameter@update_state(chain_parameter, current_value)
  return(invisible(NULL))
})

setGeneric("proposed_is_accepted", function(chain_parameter, first_value) standardGeneric("proposed_is_accepted"))
setMethod("proposed_is_accepted", "ChainParameter", function(chain_parameter, first_value) {
  if(missing(first_value)) c(NA, abs(diff(chain_parameter@state$chain)) > 0)
  else c(first_value != chain_parameter@state$chain[1], abs(diff(chain_parameter@state$chain)) > 0)
})

