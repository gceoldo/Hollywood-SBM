setClass("CommunityUnrestrictedFlip", slots = c(
  proposal = "list", 
  evaluation = "list", 
  update_state_process = "list", 
  store_info = "list"
))

CommunityUnrestrictedFlip <- function(proposal, evaluation, update_state_process, store_info) {
  if(missing(proposal)) proposal <- set_defaultCUF("proposal")
  else check_valid_inputCUF("proposal", proposal)
  if(missing(evaluation)) evaluation <- set_defaultCUF("evaluation")
  else check_valid_inputCUF("evaluation", evaluation)
  if(missing(update_state_process)) update_state_process <- set_defaultCUF("update_state_process")
  else check_valid_inputCUF("update_state_process", update_state_process)
  if(missing(store_info)) store_info <- set_defaultCUF("store_info")
  else check_valid_inputCUF("store_info", store_info)
  
  new("CommunityUnrestrictedFlip", proposal = proposal, evaluation = evaluation, update_state_process = update_state_process, store_info = store_info)
}

set_defaultCUF <- function(input) { switch(
  input, 
  proposal = list(
    get_relations_for_which_flip_is_allowed = function(community_assignment) { number_relations(community_assignment) }, 
    sample_relation = function(relations) { sample.int(relations, 1L) },
    sample_proposed_flip = function(relation, community_assignment, probability_new_community) { 
      current_community <- select_assignment_from_relations(community_assignment, relation)
      active_communities <- label_active_communities(community_assignment)
      new_community <- label_first_inactive_community(community_assignment)
      new_community_is_sampled <- rbinom(1,1,probability_new_community)
      if(new_community_is_sampled) {
        if(size_selected_communities(community_assignment, as.character(current_community), "integer") == 1L) proposed_community <- current_community
        else proposed_community <- label_first_inactive_community(community_assignment)
      }
      else proposed_community <- resample(active_communities[active_communities != current_community], 1L)
      return(list(relation = relation, old_community = current_community, new_community = proposed_community))
    }
  ),
  evaluation = list(
    proposed_flip_is_skipped = function(current_community, proposed_community) { current_community == proposed_community },
    compute_proposed_statistics = function(multinetwork, relation, current_community, proposed_community, network) { 
      compute_proposed_statistics_after_flip(multinetwork, relation, current_community, proposed_community, network) 
    },
    get_current_statistics = function(community_assignment, multinetwork, proposed_statistics) {},
    compute_MH_logratio = function(community_assignment, parameter, multinetwork, network, proposed_statistics, current_statistics, probability_new_community) { 
      old_community <- proposed_statistics$info_flip$old_community
      new_community <- proposed_statistics$info_flip$new_community
      return(
        + default_logratiosCUF$prior_communities(community_assignment, old_community, new_community)
        + default_logratiosCUF$network(multinetwork, parameter, old_community, new_community, proposed_statistics)
        + default_logratiosCUF$merge(community_assignment, multinetwork, parameter, network, old_community, new_community, proposed_statistics)
        + default_logratiosCUF$proposal(community_assignment, old_community, new_community, probability_new_community)
      )
    }
  ),
  update_state_process = list(
    flip_is_accepted = function(logratio, logtemperature = 0) { log(runif(1)) < (logratio + logtemperature) },
    community_assignment = function(community_assignment, proposed_statistics) {set_state_after_flip(community_assignment, proposed_statistics)},
    multinetwork = function(multinetwork, proposed_statistics) {set_state_after_flip(multinetwork, proposed_statistics)}
  ),
  store_info = list(
    create_empty_info_flip = function() {info_flip <- vector("list", 5); names(info_flip) <-c("type_update", "relation", "old_community", "new_community", "accepted"); info_flip$type_update = "uflip"; return(info_flip)}
  )
)}




setGeneric("metropolis_hastings_uflip", function(community_unrestricted_flip, probability_new_community, community_assignment, multinetwork, parameter, network) standardGeneric("metropolis_hastings_uflip"))
setMethod("metropolis_hastings_uflip", c(community_unrestricted_flip = "CommunityUnrestrictedFlip", community_assignment = "CommunityAssignment",  multinetwork = "Multinetwork", parameter = "Parameter", network = "Network"),
          function(community_unrestricted_flip, probability_new_community, community_assignment, multinetwork, parameter, network) {
            proposed_flip <- sample_community_unrestricted_flip(community_unrestricted_flip, community_assignment, probability_new_community)
            result_evaluation <- evaluation_unrestricted_flip(proposed_flip, probability_new_community, community_unrestricted_flip, community_assignment, multinetwork, parameter, network)
            flip_is_accepted <- update_state_process(result_evaluation$logratio, result_evaluation$proposed_statistics, logtemperature = 0, community_unrestricted_flip, community_assignment, multinetwork)
            info_flip <- store_info(community_unrestricted_flip, proposed_flip, flip_is_accepted)
            return(info_flip)
          })


setGeneric(name = "sample_community_unrestricted_flip", def = function(community_unrestricted_flip, community_assignment, probability_new_community) standardGeneric("sample_community_unrestricted_flip"))
setMethod("sample_community_unrestricted_flip", c(community_unrestricted_flip = "CommunityUnrestrictedFlip", community_assignment = "CommunityAssignment"), 
          function(community_unrestricted_flip, community_assignment, probability_new_community) {
            relations_to_be_sampled <- community_unrestricted_flip@proposal$get_relations_for_which_flip_is_allowed(community_assignment)
            relation <- community_unrestricted_flip@proposal$sample_relation(relations_to_be_sampled)
            return(community_unrestricted_flip@proposal$sample_proposed_flip(relation, community_assignment, probability_new_community))
          })

setGeneric("evaluation_unrestricted_flip", function(proposed_flip, probability_new_community, community_flip, community_assignment, multinetwork, parameter, network) standardGeneric("evaluation_unrestricted_flip"))
setMethod("evaluation_unrestricted_flip", c(community_flip = "CommunityUnrestrictedFlip", community_assignment = "CommunityAssignment", multinetwork = "Multinetwork", parameter = "Parameter", network = "Network"),
          function(proposed_flip, probability_new_community, community_flip, community_assignment, multinetwork, parameter, network) {
            skip_flip <- community_flip@evaluation$proposed_flip_is_skipped(proposed_flip$old_community, proposed_flip$new_community)
            if(skip_flip) return(list(logratio = -Inf, proposed_statistics = list()))
            else {
              proposed_statistics <- community_flip@evaluation$compute_proposed_statistics(multinetwork, proposed_flip$relation, proposed_flip$old_community, proposed_flip$new_community, network)
              current_statistics  <- community_flip@evaluation$get_current_statistics(community_assignment, multinetwork, proposed_statistics)
              return(list(logratio = community_flip@evaluation$compute_MH_logratio(community_assignment, parameter, multinetwork, network, proposed_statistics, current_statistics, probability_new_community),
                          proposed_statistics = proposed_statistics))
            }
          })

setMethod("update_state_process", c(community_flip = "CommunityUnrestrictedFlip", community_assignment = "CommunityAssignment", multinetwork = "Multinetwork"),
          function(logratio, proposed_statistics, logtemperature, community_flip, community_assignment, multinetwork) {
            accepted_flip <- community_flip@update_state_process$flip_is_accepted(logratio, logtemperature)
            if(accepted_flip) {
              community_flip@update_state_process$community_assignment(community_assignment, proposed_statistics)
              community_flip@update_state_process$multinetwork(multinetwork, proposed_statistics)
            }
            return(accepted_flip)
          })

setMethod("store_info", c(community_flip = "CommunityUnrestrictedFlip"), 
          function(community_flip, proposed_flip, accepted) {
            info_flip <- community_flip@store_info$create_empty_info_flip()
            info_flip$relation = proposed_flip$relation
            info_flip$old_community = proposed_flip$old_community
            info_flip$new_community = proposed_flip$new_community
            info_flip$accepted = accepted
            return(info_flip)
          })





default_logratiosCUF <- list(
  prior_communities = function(community_assignment, old_community, new_community) {
    size_old <- size_selected_communities(community_assignment, old_community, "integer")
    size_new <- size_selected_communities(community_assignment, new_community, "integer")
    current_nc <- number_communities(community_assignment)
    nrel <- number_relations(community_assignment)
    lratio_community_size <- 0 # assume no relabel unique community
    if(size_old == 1L) lratio_community_size <- - log(current_nc - 1) + log(nrel - current_nc + 1)
    if(size_new == 0L) lratio_community_size <- + log(current_nc) - log(nrel - current_nc)
    return(log(size_new + 1L) - log(size_old) + lratio_community_size
    )
  },
  network = function(multinetwork, parameter, old_community, new_community, proposed_statistics) {
    if(community_is_active(multinetwork, new_community)) {
      strength_new_community <- strength(parameter, new_community)
      discount_new_community <- discount(parameter, new_community)
    }
    else {
      strength_new_community <- strength(parameter, old_community)
      discount_new_community <- discount(parameter, old_community)
    }
    return(
      + (proposed_statistics$size_population_new_community - size_population(multinetwork, new_community)) * log(strength_new_community)
      + (proposed_statistics$size_population_old_community - size_population(multinetwork, old_community)) * log(strength(parameter, old_community))
    
      + lgamma(discount_new_community/strength_new_community + proposed_statistics$size_population_new_community)
      - lgamma(discount_new_community/strength_new_community + size_population(multinetwork, new_community))
      + lgamma(discount(parameter, old_community)/strength(parameter, old_community) + proposed_statistics$size_population_old_community)
      - lgamma(discount(parameter, old_community)/strength(parameter, old_community) + size_population(multinetwork, old_community))
    
      - lgamma(discount_new_community + proposed_statistics$total_arity_new_community)
      + lgamma(discount_new_community + total_arity(multinetwork, new_community))
      - lgamma(discount(parameter, old_community) + proposed_statistics$total_arity_old_community)
      + lgamma(discount(parameter, old_community) + total_arity(multinetwork, old_community))
    
      + sum(proposed_statistics$variation_degree_frequencies_new_community * (sapply(proposed_statistics$keys_degree_frequencies_new_community, function(x) lgamma(as.double(x)-strength_new_community)) - lgamma(1-strength_new_community)))
      + sum(proposed_statistics$variation_degree_frequencies_old_community * (sapply(proposed_statistics$keys_degree_frequencies_old_community, function(x) lgamma(as.double(x)-strength(parameter, old_community))) - lgamma(1-strength(parameter, old_community))))
  )},
  merge = function(community_assignment, multinetwork, parameter, network, old_community, new_community, proposed_statistics) {
    active_communities_with_new <- unique(c(new_community, as.character(label_active_communities(community_assignment))))
    proposed_size_populations <- current_size_populations <- as.integer(lapply(active_communities_with_new, function(x) size_population(multinetwork, x)))
    names(proposed_size_populations) <- active_communities_with_new
    proposed_size_populations[[as.character(old_community)]] <- proposed_statistics$size_population_old_community
    proposed_size_populations[[as.character(new_community)]] <- proposed_statistics$size_population_new_community
    
    proposed_product <- prod(sapply(proposed_size_populations, function(x) 1-x/(proposed_statistics$size_largest_population + overlap(parameter))))
    current_product  <- prod(sapply(current_size_populations,  function(x) 1-x/(size_largest_population(multinetwork) + overlap(parameter))))
    
    return( 
      + lgamma(proposed_statistics$size_largest_population + overlap(parameter) + 1L)
      - lgamma(size_largest_population(multinetwork) + overlap(parameter) + 1L)
      + lgamma(size_largest_population(multinetwork) + overlap(parameter) + 1L - size_population(network))
      - lgamma(proposed_statistics$size_largest_population + overlap(parameter) + 1L - size_population(network))
      
      + size_population(network) * log(1 - proposed_product) 
      - size_population(network) * log(1 - current_product)
      + (proposed_statistics$size_largest_population + overlap(parameter) - size_population(network)) * log(proposed_product)
      - (size_largest_population(multinetwork)       + overlap(parameter) - size_population(network)) * log(current_product)
    )},
  proposal = function(community_assignment, old_community, new_community, probability_new_community) {
    size_old <- size_selected_communities(community_assignment, old_community, "integer")
    size_new <- size_selected_communities(community_assignment, new_community, "integer")
    if(size_old >  1L & size_new >= 1L) return(0)
    if(size_old >  1L & size_new == 0L) return(-log(number_communities(community_assignment)) - log(probability_new_community))
    if(size_old == 1L & size_new >= 1L) return(+log(number_communities(community_assignment) - 1L) + log(probability_new_community))
    stop("error")
  }
)


check_valid_inputCUF <- function(which_input, input) { switch(
  which_input, 
  proposal = {
    if(!is.list(input)) stop("proposal is not a list")
    if(length(input) != 3L) stop("proposal must of length 3")
    if(sum(unlist(lapply(input, function(x) mode(x) == "function"))) < 3L) stop("proposal must be a list of functions")
    if(any(names(input) != c("get_relations_for_which_flip_is_allowed", "sample_relation", "sample_proposed_flip"))) stop("error")
  },
  evaluation = {
    if(!is.list(input)) stop("evaluation is not a list")
    if(length(input) != 4L) stop("evaluation must of length 4")
    if(sum(unlist(lapply(input, function(x) mode(x) == "function"))) < 4L) stop("evaluation must be a list of functions")
    if(any(names(input) != c("proposed_flip_is_skipped", "compute_proposed_statistics", "get_current_statistics", "compute_MH_logratio"))) stop("error")
  },
  update_state_process = {
    if(!is.list(input)) stop("update_state_process is not a list")
    if(length(input) != 3L) stop("update_state_process must of length 3")
    if(sum(unlist(lapply(input, function(x) mode(x) == "function"))) < 3L) stop("update_state_process must be a list of functions")
    if(any(names(input) != c("flip_is_accepted", "community_assignment", "multinetwork"))) stop("error")
  },
  store_info = {
    if(!is.list(input)) stop("proposal is not a list")
    if(length(input) != 0L) stop("proposal must of length 0")
  }
)}
