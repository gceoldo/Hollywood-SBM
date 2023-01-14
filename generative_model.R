sample_HBM <- function(assignment, strength, discount, overlap, sample_arity) {
  number_relations <- length(assignment)
  active_communities <- sort(unique(assignment))
  number_communities <- length(active_communities)
  list_networks <- vector("list", number_communities)
  
  if(length(strength) == 1L) strength <- rep(strength, number_communities)
  if(length(discount) == 1L) discount <- rep(discount, number_communities)
  if(length(sample_arity) == 1L) sample_arity <- replicate(number_communities, sample_arity)
  
  for(i in seq_along(active_communities)) {
    list_networks[[i]] <- sample_hollywood(sum(assignment == active_communities[i]), strength[i], discount[i], sample_arity[[i]])
  } 
  return(merge_networks(list_networks, overlap, assignment))
}

# sample_HBM(c(2,2,1,1,1,4,3,3,4,2,1), .7, .1, 9L, function() 2L+rpois(1,4))

sample_hollywood <- function(number_relations, strength, discount, sample_arity) {
  set_relations <- vector("list", number_relations)
  number_roles <- list()
  
  number_actors <- 1L
  number_roles[[1]] <- 1L
  if(number_relations > 0L) {
    new_relation <- sample_relation_hollywood(sample_arity(), number_actors, number_roles, strength, discount, TRUE)
    set_relations[[1]] <- new_relation$relation
    number_actors <- new_relation$number_actors
    number_roles <- new_relation$number_roles
  }  
  if(number_relations > 1L) {
    for(j in 2:number_relations) {
      new_relation <- sample_relation_hollywood(sample_arity(), number_actors, number_roles, strength, discount, FALSE)
      set_relations[[j]] <- new_relation$relation
      number_actors <- new_relation$number_actors
      number_roles <- new_relation$number_roles
    } 
  }
  return(set_relations)
}

sample_relation_hollywood <- function(size_relation, number_actors, number_roles, strength, discount, first_relation) {
  relation <- rep(NA_integer_, size_relation)
  if(first_relation) relation[1] <- 1L
  for(l in (1+first_relation):size_relation) {
    new_value <- sample.int(number_actors + 1L, 1, FALSE, 
                            prob = c(unlist(number_roles) - strength, discount + strength*number_actors))
    if(new_value == (number_actors + 1L)) {
      number_actors <- number_actors + 1L
      number_roles[[number_actors]] <- 1L
    }
    else number_roles[[new_value]] <- number_roles[[new_value]] + 1L
    relation[l] <- new_value
  }
  return(list(relation = relation, number_actors = number_actors, number_roles = number_roles))
}

#sample_hollywood(5L, .8, .1, function() 1+rpois(1,4))


merge_networks <- function(list_networks_in_size_biased_order, overlap, assignment) {
  size_populations <- as.integer(lapply(list_networks_in_size_biased_order, function(x) length(unique(unlist(x)))))
  size_largest_population <- max(size_populations)
  number_relations <- length(assignment)
  number_communities <- length(unique(assignment))
  stopifnot(length(list_networks_in_size_biased_order) == number_communities)
  
  set_relations <- vector("list", number_relations)
  strings_matrix <- matrix(0L, size_largest_population+overlap, number_communities)
  
  for(h in 1L:number_communities) {
    pos <- sample.int(size_largest_population + overlap, size_populations[h], replace=F)
    strings_matrix[pos,h] <- sample.int(size_populations[h],size_populations[h],F)
  } 
  
  for(h in 1L:number_communities) {
    set_relations_h_in_vector_representation <- list_to_vectors_representation(list_networks_in_size_biased_order[[h]])
    set_relations_h_new_labels <- rep(NA_integer_, length(set_relations_h_in_vector_representation$vec))
    for(i in 1L:size_populations[h]) {
      nodes_with_same_label <- which(set_relations_h_in_vector_representation$vec == i)
      set_relations_h_new_labels[nodes_with_same_label] <- which(strings_matrix[,h] == i)
    }
    which_community_h <- which(assignment == h)
    set_relations[which_community_h] <- vectors_to_list_representation(set_relations_h_new_labels, set_relations_h_in_vector_representation$ar)
  }
  
  return(size_biased_order(set_relations))
  
}

size_biased_order <- function(y) {
  unlisted_y <- unlist(y)
  original_population <- unique(unlisted_y) # population in order of appearance 
  unlisted_y_new <- rep(NA_integer_, length(unlisted_y))
  
  for(i in seq_along(original_population)) {
    which_i = which(unlisted_y == original_population[i])
    unlisted_y_new[which_i] <- i
  }
  
  arities <- as.integer(lapply(y, length))
  y_new <- list()
  cum_arity <- 0L
  for(j in 1:length(y)) {
    y_new[[j]] <- unlisted_y_new[cum_arity + (1:length(y[[j]]))]
    cum_arity <- cum_arity + length(y[[j]])
  }
  
  return(y_new)
}

vectors_to_list_representation <- function(vec, ar) {
  cum_arity <- 0L
  y <- vector("list", length(ar))
  for(j in 1:length(ar)) {
    y[[j]] <- vec[cum_arity + (1:ar[j])]
    cum_arity <- cum_arity + ar[j]
  }
  return(y)
}


list_to_vectors_representation <- function(lis) list(vec=unlist(lis), ar=as.integer(lapply(lis, length)))

merge_networks(list(list(as.integer(c(1,2,1)),as.integer(c(3,1))),list(as.integer(c(1,1)),as.integer(c(2,3))),list(as.integer(c(1,2)),as.integer(c(3,2,1)))),
               5L, c(2,1,1,3,3,2))


sample_prior_community <- function(n_total, cc, cc_max) {
  n_total <- as.integer(n_total)
  if(missing(cc_max)) cc_max <- n_total
  if(missing(cc)) {
    cc <- sample.int(min(n_total, cc_max),1)
  } else cc <- as.integer(cc)
  n <- rep(NA_integer_, cc)
  if(cc > 1L) {
    sample_bars <- sort(sample.int(n_total-1, cc-1, FALSE))
    if(cc > 2L) {
      n[1] <- sample_bars[1]
      n[cc] <- n_total-sample_bars[cc-1]
      n[-c(1,cc)] <- diff(sample_bars)
    } else { n <- c(sample_bars, n_total-sample_bars) }
    return(sample(unlist(purrr::map2(1:cc, n, ~ rep(.x,times=.y))), n_total, replace = F))
  } else return(rep(1L, n_total))
}
