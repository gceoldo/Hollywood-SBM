setClass("Statistics", slots = c(
  names = "character",
  compute_statistic = "list",
  add_info_to_statistic  = "list",
  rm_info_from_statistic = "list"
))

Statistics <- function(model) {
  if(!is(model, "Model")) stop("wrong class")
  
  names = initialize_names(model)
  new("Statistics", 
      names = names, 
      compute_statistic = initialize_compute_statistic(names), 
      add_info_to_statistic  = initialize_add_info_to_statistic(names), 
      rm_info_from_statistic = initialize_rm_info_from_statistic(names))
}

initialize_names <- function(model) {
  unique(c(
    switch(model_for_networks(model),
           PitmannYor = c("population_size", "degrees", "degree_frequencies", "arities", "arity_frequencies")  
           ),
    switch(model_for_union(model),
           uniform = c("population_size", "maximum_population_size")
           )
  ))
}

initialize_compute_statistic <- function(names) {
  list_functions <- vector("list", length(names))
  for(i in seq_along(names)) {
    
  }
}

setClass("ContainerFunctionsStatistics", slots = c(functions = "list"))

ContainerFunctionsStatistics <- function() {
  functions <- list()
  
  functions$compute_degrees_from_relation <- function(relation) {
    boundary <- relation[ c(1,length(relation))]
    interior <- relation[-c(1,length(relation))]
    return(as.gset(boundary) + as.gset(rep(interior,2)))
  }
  functions$combine_degrees_from_multisets_of_degrees <- function(list_of_multisets) {
    degrees <- gset()
    for(i in seq_along(list_of_multisets)) degrees <- degrees + list_of_multisets[[i]]
    return(degrees)
  }
  functions$compute_arity_relation <- function(relation) length(relation)
  functions$compute_arity_frequencies <- function(vector_of_arities) as.gset(vector_of_arities)
  functions$compute_degree_frequencies <- function(multiset_degrees) as.gset(as.integer(gset_memberships(multiset_degrees)))
  functions$compute_size_population_from_degrees <- function(multiset_degrees) length(multiset_degrees)
  
  functions$update_degrees_after_removal_subnetwork <- function(current_state, multiset_degrees_subnetwork) {
    current_state$degrees <- current_state$degrees - multiset_degrees_subnetwork
    return(invisible())
  }
  functions$update_degrees_after_inclusion_subnetwork <- function(current_state, multiset_degrees_subnetwork) {
    current_state$degrees <- current_state$degrees + multiset_degrees_subnetwork
    return(invisible())
  }
  functions$update_degree_frequencies_after_removal_subnetwork <- function(
    current_state, multiset_degrees_subnetwork,
    degrees_before_removal,
    degrees_after_removal
  ) {
    degree_frequencies_before_removal <- 
    return(invisible())
  }
  
  
  
  
  new("ContainerFunctionsStatistics", functions = functions)
}
