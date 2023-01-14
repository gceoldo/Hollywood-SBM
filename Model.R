setClass("Model", slots = c(
  model_for_networks = "character",
  model_for_union = "character"
))

Model <- function(model_for_networks = "PitmannYor", model_for_union = "uniform") {
  valid_network_models <- c("PitmannYor")
  valid_union_models <- c("uniform")
  
  model_for_networks <- as.character(model_for_networks)[[1]]
  model_for_union <- as.character(model_for_union)[[1]]
  
  if(!(model_for_networks %in% valid_network_models)) stop("error")
  if(!(model_for_union %in% valid_union_models)) stop("error")
  
  new("Model", model_for_networks = model_for_networks, model_for_union = model_for_union)
}


setGeneric("model_for_networks", function(object) {"model_for_networks"})
setMethod("model_for_networks", "Model", function(object) object@model_for_networks)
setGeneric("model_for_union", function(object) {"model_for_union"})
setMethod("model_for_union", "Model", function(object) object@model_for_union)
