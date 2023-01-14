setClass("Network", slots = c(individual_statistics = "list", global_statistics = "list"))

Network <- function(label_relations, set_relations) {
  if(!is(label_relations, "LabelRelations")) stop("wrong class")
  if(!is.list(set_relations)) stop("wrong class")
  
  set_relations <- check_set_relations_is_valid(set_relations, label_relations)
  
  individual_statistics <- vector("list", number_relations(label_relations))
  for(i in 1:number_relations(label_relations)) individual_statistics[[i]] <- compute_individual_statistics(set_relations[[i]])
  names(individual_statistics) <- as.character(1:number_relations(label_relations))
  
  global_statistics <- compute_global_statistics(set_relations)
  
  new("Network", individual_statistics = individual_statistics, global_statistics = global_statistics)
}

check_set_relations_is_valid <- function(set_relations, label_relations) {
  if(number_relations(label_relations) != length(set_relations)) stop("wrong number of relations")
  if(any(as.logical(lapply(set_relations, anyNA)))) stop("missing values are not allowed")
  if(any(as.logical(lapply(set_relations, function(x) any(x < 1L))))) stop("label actors must be at least 1")
  if(any(as.logical(lapply(set_relations, function(x) length(x) < 2L)))) stop("relations must be of length at least 2")
  return(lapply(set_relations, as.integer))
}

compute_individual_statistics <- function(relation) {
  stats <- vector("list", 3)
  names(stats) <- c("degrees", "degree_frequencies", "arity")
  stats$degrees <- compute_hashmultiset_degrees_from_relation(relation)
  stats$degree_frequencies <- create_hashmultiset(hash::values(stats$degrees))
  stats$arity <- length(relation)
  return(stats)
}

compute_global_statistics <- function(set_relations) {
  stats <- vector("list", 1)
  names(stats) <- c("size_population")
  stats$size_population <- length(unique(unlist(set_relations)))
  return(stats)
}



setMethod("number_relations", "Network", function(object) length(object@individual_statistics))

setGeneric("degrees_individual_relations",      def = function(object, which_relations) standardGeneric("degrees_individual_relations"))
setMethod( "degrees_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$degrees))
setGeneric("degree_frequencies_individual_relations",      def = function(object, which_relations) standardGeneric("degree_frequencies_individual_relations"))
setMethod( "degree_frequencies_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$degree_frequencies))
setGeneric("arities_individual_relations",      def = function(object, which_relations) standardGeneric("arities_individual_relations"))
setMethod( "arities_individual_relations", "Network", function(object, which_relations) lapply(object@individual_statistics[which_relations], function(x) x$arity))

setGeneric("size_population", function(object, label_community) standardGeneric("size_population"))
setMethod("size_population", "Network", function(object, label_community) object@global_statistics$size_population)





compute_hashmultiset_degrees_from_relation <- function(relation) {
  degrees <- hash::hash()
  #length_relation <- length(relation)
  #boundary <- as.character(relation[ c(1,length(relation))])
  #interior <- as.character(relation[-c(1,length(relation))])
  #for(i in seq_along(boundary)) degrees[[boundary[i]]] <- null_to_zero(degrees[[boundary[i]]]) + 1L
  #for(i in seq_along(interior)) degrees[[interior[i]]] <- null_to_zero(degrees[[interior[i]]]) + 2L
  for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
  return(degrees)
}










