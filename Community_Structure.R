setClass("CommunityStructure", slots = c(
  number_communities_is_fixed = "logical",
  maximum_allowed_number_communities = "integer",
  minimum_allowed_number_communities = "integer"
))

CommunityStructure <- function(
  label_relations,
  number_communities_is_fixed = FALSE,
  maximum_allowed_number_communities = 1e4L,
  minimum_allowed_number_communities = 1L
) {
  
  if(!is(label_relations, "LabelRelations")) stop("wrong class")
  if(number_relations(label_relations) < minimum_allowed_number_communities[[1]]) stop("error")
  if(number_relations(label_relations) < maximum_allowed_number_communities[[1]]) {
    old_maximum <- maximum_allowed_number_communities[[1]]
    maximum_allowed_number_communities[[1]] <- number_relations(label_relations)
    warning(paste0("maximum_allowed_number_community, originally equal to ", old_maximum,", is decreased to ", maximum_allowed_number_communities[[1]]))
  }
  
  new("CommunityStructure",
      number_communities_is_fixed = as.logical(number_communities_is_fixed[[1]]),
      maximum_allowed_number_communities = as.integer(maximum_allowed_number_communities[[1]]),
      minimum_allowed_number_communities = as.integer(minimum_allowed_number_communities[[1]])
  )
}


setGeneric(name = "number_communities_is_fixed", def = function(object) standardGeneric("number_communities_is_fixed"))
setGeneric(name = "maximum_allowed_number_communities", def = function(object) standardGeneric("maximum_allowed_number_communities"))
setGeneric(name = "minimum_allowed_number_communities", def = function(object) standardGeneric("minimum_allowed_number_communities"))

setMethod(f = "number_communities_is_fixed", signature = "CommunityStructure", definition = function(object) object@number_communities_is_fixed)
setMethod("maximum_allowed_number_communities", "CommunityStructure", function(object) object@maximum_allowed_number_communities)
setMethod("minimum_allowed_number_communities", "CommunityStructure", function(object) object@minimum_allowed_number_communities)

setMethod(f = "show", signature = "CommunityStructure", 
  function(object) {
    cat("Class: CommunityStructure (read-only)\nSlots:\n")
    cat(paste0("  number_communities_is_fixed: ", object@number_communities_is_fixed,"\n"))
    cat(paste0("  maximum_allowed_number_communities: ", object@maximum_allowed_number_communities,"\n"))
    cat(paste0("  minimum_allowed_number_communities: ", object@minimum_allowed_number_communities,"\n"))
  }
)

setValidity(Class = "CommunityStructure", method = 
  function(object) {
    if(is.na(object@number_communities_is_fixed       )) return("can not be coerced to a positive integer")
    if(is.na(object@maximum_allowed_number_communities)) return("can not be coerced to a positive integer")
    if(is.na(object@minimum_allowed_number_communities)) return("can not be coerced to a positive integer")
    
    if(object@maximum_allowed_number_communities < 1L) return("value must be ge 1")
    if(object@minimum_allowed_number_communities < 1L) return("value must be ge 1")
    
    if(object@minimum_allowed_number_communities > object@maximum_allowed_number_communities) return("maximum must be bigger than minimum")

    return(TRUE)
  }
)






