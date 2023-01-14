setClass("ParametricSpace", slots = c(name_parameter = "character", parameter_is_global = "logical", type_parameter = "character"))

ParametricSpace <- function(model, strength_is_global = FALSE, discount_is_global = FALSE) {
  if(!is(model, "Model")) stop("wrong class")
  if(length(strength_is_global)!=1L | length(discount_is_global)!=1L) stop("error")
  if(!is.logical(strength_is_global) | !is.logical(discount_is_global) | is.na(strength_is_global) | is.na(discount_is_global)) stop("error")
  
  if(model_for_networks(model) == "PitmannYor") {
    name_network_parameters <- c("strength", "discount")
    type_network_parameters <- c("double", "double")
  } 
  
  if(model_for_union(model) == "uniform") {
    name_union_parameters <- c("overlap")
    type_union_parameters <- c("integer")
  } 
  
  name_parameter <- c(name_network_parameters, name_union_parameters)
  type_parameter <- c(type_network_parameters, type_union_parameters)
  parameter_is_global <- c(strength_is_global, discount_is_global, TRUE)

  new("ParametricSpace", name_parameter = name_parameter, parameter_is_global = parameter_is_global, type_parameter = type_parameter)
}

setGeneric("local_parameters",  function(object) standardGeneric("local_parameters"))
setGeneric("global_parameters", function(object) standardGeneric("global_parameters"))
setMethod("local_parameters",  "ParametricSpace", function(object) object@name_parameter[which(object@parameter_is_global == FALSE)])
setMethod("global_parameters", "ParametricSpace", function(object) object@name_parameter[which(object@parameter_is_global == TRUE)])
setGeneric("type_parameter", function(object, which_parameter) standardGeneric("type_parameter"))
setMethod("type_parameter", "ParametricSpace", function(object, which_parameter) object@type_parameter[[which(object@name_parameter == which_parameter)]])



