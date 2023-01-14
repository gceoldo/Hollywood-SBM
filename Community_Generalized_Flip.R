setClass("CommunityUnrestrictedFlip", slots = c(
  proposal = "list", 
  evaluation = "list", 
  update_state_process = "list", 
  store_info = "list"
))

CommunityFlip <- function(proposal, evaluation, update_state_process, store_info) {
  if(missing(proposal)) proposal <- set_defaultCUF("proposal")
  else check_valid_inputCUF("proposal", proposal)
  if(missing(evaluation)) evaluation <- set_defaultCUF("evaluation")
  else check_valid_inputCUF("evaluation", evaluation)
  if(missing(update_state_process)) update_state_process <- set_defaultCUF("update_state_process")
  else check_valid_inputCUF("update_state_process", update_state_process)
  if(missing(store_info)) store_info <- set_defaultCUF("store_info")
  else check_valid_inputCUF("store_info", store_info)
  
  new("CommunityFlip", proposal = proposal, evaluation = evaluation, update_state_process = update_state_process, store_info = store_info)
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
    compute_MH_logratio = function(community_assignment, parameter, multinetwork, network, proposed_statistics, current_statistics) { 
      old_community <- proposed_statistics$info_flip$old_community
      new_community <- proposed_statistics$info_flip$new_community
      return(
        + default_logratios$prior_communities(community_assignment, old_community, new_community)
        + default_logratios$network(multinetwork, parameter, old_community, new_community, proposed_statistics)
        + default_logratios$merge(community_assignment, multinetwork, parameter, network, old_community, new_community, proposed_statistics)
      )
    }
  ),
  update_state_process = list(
    flip_is_accepted = function(logratio, logtemperature = 0) { log(runif(1)) < (logratio + logtemperature) },
    community_assignment = function(community_assignment, proposed_statistics) {set_state_after_flip(community_assignment, proposed_statistics)},
    multinetwork = function(multinetwork, proposed_statistics) {set_state_after_flip(multinetwork, proposed_statistics)}
  ),
  store_info = list(
    create_empty_info_flip = function() {info_flip <- vector("list", 5); names(info_flip) <-c("type_update", "relation", "old_community", "new_community", "accepted"); info_flip$type_update = "flip"; return(info_flip)}
  )
)}


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
