get_first_free_label <- function(ordered_vector) {
  for(i in 1:length(ordered_vector)) if(i != ordered_vector[i]) return(i)
  return(length(ordered_vector) + 1L)
}

add_value_to_ordered_vector <- function(new_value, ordered_vector) {
  #return(append(ordered_vector, new_value, after = tail(which(ordered_vector < new_value),1)))
  return(c(ordered_vector[which(ordered_vector < new_value)], new_value, ordered_vector[which(ordered_vector > new_value)]))
} 

rm_value_from_ordered_vector <- function(rm_value, ordered_vector) {
  return(ordered_vector[which(rm_value != ordered_vector)])
} 



copy_hash_object <- function(object) {
  new_object <- hash::hash()
  for(i in names(object)) new_object[i] <- object[[i]]
  return(new_object)
}

hash_are_equal <- function(object1, object2) all(hash::keys(object1) == hash::keys(object2)) & all(hash::values(object1) == hash::values(object2))

copy_environment <- function(envir) {
  return(rlang::new_environment(as.list(envir), parent = empty_env()))
} 



create_hashset <- function(initial_sequence) {
  hashset <- hash::hash()
  hashset[initial_sequence] <- NA_integer_
  return(hashset)
}

remove_elements_from_hashset <- function(hashset, elements_to_be_removed) {
  for(i in seq_along(elements_to_be_removed)) hashset[[as.character(elements_to_be_removed[i])]] <- NULL
  return(invisible())
}

include_elements_to_hashset <- function(hashset, elements_to_be_included) {
  for(i in seq_along(elements_to_be_included)) hashset[[as.character(elements_to_be_included[i])]] <- NA_integer_
  return(invisible())
}


create_hashmultiset <- function(initial_sequence) {
  hashmultiset <- hash::hash()
  hashmultiset[sort(unique(initial_sequence))] <- as.integer(table(initial_sequence))
  return(hashmultiset)
}

extract_frequencies_hashmultiset <- function(hashmultiset, keys, mode_returned_vector) {
  frequencies <- vector(mode_returned_vector, length(keys))
  for(i in seq_along(keys)) frequencies[[i]] <- null_to_zero(hashmultiset[[as.character(keys[i])]])
  return(frequencies)
}

modify_frequencies_hashmultiset <- function(hashmultiset, keys, new_frequencies_or_variation, inputed_variation = FALSE) {
  if(!inputed_variation) for(i in seq_along(keys)) hashmultiset[[as.character(keys[i])]] <- zero_to_null(new_frequencies_or_variation[[i]])
  else for(i in seq_along(keys)) hashmultiset[[as.character(keys[i])]] <- zero_to_null(null_to_zero(hashmultiset[[as.character(keys[i])]]) + new_frequencies_or_variation[[i]])
  return(invisible())
}




resample <- function(x, ...) x[sample.int(length(x), ...)]
relabel <- function(x) {
  new_x <- rep(NA_integer_, length(x))
  unique_x <- sort(unique(x))
  for(i in 1:length(unique_x)) new_x[which(x == unique_x[i])] <- as.integer(i)
  return(new_x)
}

null_to_zero <- function(value) {
  if(!is.null(value)) return(value)
  else return(0L)
}

zero_to_null <- function(value) {
  if(value != 0L) return(value)
  else return(NULL)
}

