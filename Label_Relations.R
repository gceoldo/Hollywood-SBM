setClass("LabelRelations", slots = c(number_relations = "integer"))

LabelRelations <- function(number_relations) new("LabelRelations", number_relations = as.integer(number_relations[[1]]))

setGeneric("number_relations", function(object) standardGeneric("number_relations"))
setMethod("number_relations", "LabelRelations", function(object) object@number_relations)

setMethod("show", "LabelRelations", 
  function(object) {
    cat("Class: LabelRelations (read-only)\nSlots:\n")
    cat(paste0("  number_relations: ", object@number_relations,"\n"))
  }
)

setValidity("LabelRelations", method = 
  function(object) {
    if(is.na(object@number_relations)) return("can not coerced to a positive integer")
    if(object@number_relations < 1L) return("value must be ge 1")
    return(TRUE)
  }
)
