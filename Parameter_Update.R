setClass("ParameterUpdate", slots = c(
  proposal = "list", 
  evaluation = "list", 
  proposal_is_accepted = "function"
))

ParameterUpdate <- function(parameter, proposal, evaluation, update_state_process) {
  #name_local_parameters  <- local_parameters(parameter)
  #name_global_parameters <- global_parameters(parameter)
  
  if(missing(proposal)) proposal <- set_defaultPU("proposal") #, name_local_parameters, name_global_parameters)
  else check_valid_inputPU("proposal", proposal)
  if(missing(evaluation)) evaluation <- set_defaultPU("evaluation") #, name_local_parameters, name_global_parameters)
  else check_valid_inputPU("evaluation", evaluation)
  if(missing(update_state_process)) proposal_is_accepted <- set_defaultPU("proposal_is_accepted") #, name_local_parameters, name_global_parameters)
  else check_valid_inputPU("proposal_is_accepted", proposal_is_accepted)
  
  new("ParameterUpdate", proposal = proposal, evaluation = evaluation, proposal_is_accepted = proposal_is_accepted)
}

set_defaultPU <- function(input) { switch(input, 
  proposal = list(
    strength = function(label_community, parameter, spread_proposal = .2) boundary_reflection(rnorm(1, strength(parameter, label_community), spread_proposal), 0, 1),
    discount = function(label_community, parameter, spread_proposal = 1)  boundary_reflection(rnorm(1, discount(parameter, label_community), spread_proposal), -strength(parameter,label_community), Inf),
    overlap  = function(parameter, multinetwork, network, spread_proposal = 10L) { 
      return(as.integer(boundary_reflection(
        overlap(parameter) + sample(c(-1L, 1L), 1) * sample.int(spread_proposal, 1),
        size_population(network) - size_largest_population(multinetwork), Inf))
      )
    }
  ),
  evaluation = list(
    strength = function(proposed_strength, label_community, parameter, multinetwork) {
      alpha <- strength(parameter, as.character(label_community))
      theta <- discount(parameter, as.character(label_community))
      v <- size_population(multinetwork, as.character(label_community))
      dwf <- pair_degrees_with_frequencies(multinetwork, as.character(label_community))
      return(
        + v * (log(proposed_strength) - log(alpha))
        + lgamma(theta/proposed_strength + v) - lgamma(theta/proposed_strength)
        - lgamma(theta/alpha + v) + lgamma(theta/alpha)
        + sum(dwf[["degree_frequencies"]] * ((lgamma(dwf[["degrees"]] - proposed_strength) - lgamma(dwf[["degrees"]] - alpha)) - (lgamma(1 - proposed_strength) - lgamma(1 - alpha)))) 
      )
    },
    discount = function(proposed_discount, label_community, parameter, multinetwork) {
      theta <- discount(parameter, as.character(label_community))
      alpha <- strength(parameter, as.character(label_community))
      v <- size_population(multinetwork, as.character(label_community))
      m <- total_arity(multinetwork, as.character(label_community))
      return(
        + lgamma(proposed_discount/alpha + v) - lgamma(proposed_discount/alpha)
        - lgamma(theta/alpha + v) + lgamma(theta/alpha)
        - lgamma(proposed_discount + m) + lgamma(proposed_discount)
        + lgamma(theta + m) - lgamma(theta)
      )
    },
    overlap =  function(proposed_overlap,  parameter, multinetwork, network) {
      active_communities <- label_active_communities(multinetwork)
      current_size_populations <- as.integer(lapply(active_communities, function(x) size_population(multinetwork, x)))
      proposed_product <- prod(sapply(current_size_populations, function(x) 1-x/(size_largest_population(multinetwork) + proposed_overlap)))
      current_product  <- prod(sapply(current_size_populations, function(x) 1-x/(size_largest_population(multinetwork) + overlap(parameter))))
      return(
        + lgamma(size_largest_population(multinetwork) + proposed_overlap + 1L) - lgamma(size_largest_population(multinetwork) + overlap(parameter) + 1L)
        - lgamma(size_largest_population(multinetwork) + proposed_overlap - size_population(network) + 1L) + lgamma(size_largest_population(multinetwork) + overlap(parameter) - size_population(network) + 1L) 
        + size_population(network) * (log(1 - proposed_product) - log(1 - current_product))
        + (size_largest_population(multinetwork) + proposed_overlap   - size_population(network)) * log(proposed_product)
        - (size_largest_population(multinetwork) + overlap(parameter) - size_population(network)) * log(current_product)
      )
    }
  ),
  proposal_is_accepted = function(logratio, logtemperature = 0) { log(runif(1)) < (logratio + logtemperature) }
)}



setGeneric("metropolis_hastings_strength", function(parameter_update, parameter, multinetwork, label_community) standardGeneric("metropolis_hastings_strength"))
setMethod("metropolis_hastings_strength", c(parameter_update = "ParameterUpdate", parameter = "Parameter", multinetwork = "Multinetwork"), function(parameter_update, parameter, multinetwork, label_community) { 
  proposed_strength <- parameter_update@proposal$strength(as.character(label_community), parameter)
  if(missing(label_community)) {
    logratio <- 0.0
    if("strength" %in% global_parameters(parameter)) for(i in label_active_communities(multinetwork)) logratio <- logratio + parameter_update@evaluation$strength(proposed_strength, as.character(i), parameter, multinetwork)
    else stop("error")
  } 
  else {
    if("strength" %in% local_parameters(parameter)) logratio <- parameter_update@evaluation$strength(proposed_strength, as.character(label_community), parameter, multinetwork)
    else stop("error")
  } 
  if(parameter_update@proposal_is_accepted(logratio)) return(proposed_strength)
  else return(strength(parameter, label_community))
})

setGeneric("metropolis_hastings_discount", function(parameter_update, parameter, multinetwork, label_community) standardGeneric("metropolis_hastings_discount"))
setMethod("metropolis_hastings_discount", c(parameter_update = "ParameterUpdate", parameter = "Parameter", multinetwork = "Multinetwork"), function(parameter_update, parameter, multinetwork, label_community) { 
  proposed_discount <- parameter_update@proposal$discount(as.character(label_community), parameter)
  if(missing(label_community)) {
    logratio <- 0.0
    if("discount" %in% global_parameters(parameter)) for(i in label_active_communities(multinetwork)) logratio <- logratio + parameter_update@evaluation$discount(proposed_discount, as.character(i), parameter, multinetwork)
    else stop("error")
  } 
  else {
    if("discount" %in% local_parameters(parameter)) logratio <- parameter_update@evaluation$discount(proposed_discount, as.character(label_community), parameter, multinetwork)
    else stop("error")
  } 
  if(parameter_update@proposal_is_accepted(logratio)) return(proposed_discount)
  else return(discount(parameter, label_community))
})

setGeneric("metropolis_hastings_overlap", function(parameter_update, parameter, multinetwork, network) standardGeneric("metropolis_hastings_overlap"))
setMethod("metropolis_hastings_overlap", c(parameter_update = "ParameterUpdate", parameter = "Parameter", multinetwork = "Multinetwork", network = "Network"), function(parameter_update, parameter, multinetwork, network) { 
  proposed_overlap <- parameter_update@proposal$overlap(parameter, multinetwork, network)
  logratio <- parameter_update@evaluation$overlap(proposed_overlap, parameter, multinetwork, network)
  if(parameter_update@proposal_is_accepted(logratio)) return(proposed_overlap)
  else return(overlap(parameter))
})
          


boundary_reflection <- function(parameter, left_boundary, right_boundary) {
  while((parameter < left_boundary) | (parameter > right_boundary)) {
    if(parameter < left_boundary) parameter <- 2*left_boundary - parameter
    else parameter <- 2*right_boundary - parameter
  }
  return(parameter)
}

check_valid_inputCF <- function(which_input, input) { switch(which_input, 
  proposal = { if(!is.list(input)) stop("proposal is not a list") },
  evaluation = { if(!is.list(input)) stop("evaluation is not a list") },
  proposal_is_accepted = { if(!is.function(input)) stop("proposal_is_accepted is not a function") },
  #store_accepted_update = { if(!is.function(input)) stop("store_accepted_update is not a function") }
)}


