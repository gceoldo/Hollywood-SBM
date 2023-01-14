source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/generative_model.R")

relabel <- function(x) {
  new_x <- rep(NA_integer_, length(x))
  unique_x <- sort(unique(x))
  for(i in 1:length(unique_x)) new_x[which(x == unique_x[i])] <- as.integer(i)
  return(new_x)
}

compute_population_overlaps <- function(list_networks) {
  null_to_zero <- function(value) ifelse(is.null(value), 0L, value)
  create_list_of_hash <- function(number_actors) {
    actor_counts_in_communities <- vector("list", number_actors)
    for(i in seq_along(actor_counts_in_communities)) actor_counts_in_communities[[i]] <- hash::hash()
    names(actor_counts_in_communities) <- 1:number_actors
    return(actor_counts_in_communities)
  }

  actors <- sort(unique(unlist(unlist(list_networks))))
  if(max(actors) != tail(actors,1)) stop("network must be in canonical order")
  number_actors <- length(actors)
  actor_counts_in_communities <- create_list_of_hash(number_actors)
  for(i in seq_along(list_networks)) {
    table_counts <- table(unlist(list_networks[[i]]))
    actors_in_network <- as.integer(names(table_counts))
    counts_in_network <- as.integer(table_counts)
    for(h in seq_along(actors_in_network)) actor_counts_in_communities[[actors_in_network[h]]][[names(list_networks[i])]] <- null_to_zero(actor_counts_in_communities[[actors_in_network[h]]][[names(list_networks[i])]]) + counts_in_network[[h]]
  }
  return(actor_counts_in_communities)
}


length_assignment <- 10L
max_number_communities <- 10L
real_overlap <- 4L
real_strength <- .8
real_discount <- .1
arity_parameter1 <- 2L
arity_parameter2 <- 0L


real_assignment <- sample_prior_community(length_assignment, cc_max = max_number_communities)
set_relations <- sample_HBM(relabel(real_assignment), real_strength, real_discount, real_overlap, function() arity_parameter1+rpois(1,arity_parameter2))
list_networks <- split(set_relations, real_assignment)

compute_population_overlaps(list_networks)



logdmerge <- function(list_networks, overlap) {
  prob <- function(v, vj) {
    NA_to_zero <- function(value) ifelse(is.na(value), return(0), return(value)) 
    set_binomial <- function(vjj) ifelse(vjj == 0, stop(), return(polynom::polynomial(c(1, rep(0, vjj), -1))))
    
    reduce_ratio <- function(ratio) {
      gcd <- GCD(ratio$numerator, ratio$denominator)
      return(list(numerator = ratio$numerator/gcd, denominator = ratio$denominator/gcd))
    }
    invert_polynomial <- function(coef_pol, max_degree) {
      if(coef_pol[1] == 0) stop()
      coef_inverted <- rep(NA_real_, max_degree+1)
      coef_inverted[1] <- 1/coef_pol[1]
      if(max_degree > 0) for(i in 2:(max_degree+1)) coef_inverted[i] <- -sum(coef_pol[2:i] * coef_inverted[(i-1):1]) * coef_inverted[1]
      return(coef_inverted)
    }
    
    vj <- as.integer(sort(vj, decreasing = TRUE))
    v <- as.integer(v)
    v_star <- v - vj[1]
    if( (v < vj[1]) | (v > sum(vj)) ) return(0)
    if(length(vj) < 2L) {
      if(v_star != 0L) return(0)
      else return(1)
    }  
    
    
    num <- polynom::polynomial(1)
    for(i in seq_along(vj[-1])) num <- num * set_binomial(vj[1+i])
    den <- polynom::polynomial(c(1,-1)) ^ (length(vj) - 1L)
    coef_numerator <-   sapply(1:(v_star+1), function(i) NA_to_zero(num[i]))
    coef_denominator <- sapply(1:(v_star+1), function(i) NA_to_zero(den[i]))
    coef_inverted_denominator <- invert_polynomial(coef_denominator, v_star)
    
    return( sum(coef_numerator * coef_inverted_denominator[(v_star+1):1]) / prod(vj[-1]+1) )
  }
  
  population_sizes <- as.integer(lapply(list_networks, function(x) length(unique(unlist(x)))))
  number_communities <- length(population_sizes)
  maximum_size_population <- max(population_sizes)
  total_size_population <- length(unique(unlist(list_networks)))
  product <- prod(as.double(lapply(population_sizes, function(x) 1-x/(maximum_size_population + overlap))))
  
  return(
    + dbinom(log = TRUE, x = total_size_population, size = maximum_size_population + overlap, prob = 1 - product) 
    - log(
      + pbinom(q = sum(population_sizes),        size = maximum_size_population + overlap, prob = 1 - product)
      - pbinom(q = maximum_size_population - 1L, size = maximum_size_population + overlap, prob = 1 - product)
    )
    + log(prob(total_size_population, population_sizes))
  )
}



logdmerge <- function(list_networks, overlap) {
  prob <- function(population_sizes, total_size_population) {
    compute_coef_poly <- function(v, v_star) {
      current_value <- polynom::polynomial(1)
      for(i in seq_along(v)) current_value <- current_value*polynom::polynomial(c(1,rep(0, v[i]),-1))
      if(length(current_value) < v_star) current_value <- c(current_value, rep(0L, v_star - length(current_value) + 1))
      return(as.double(current_value)[1:(v_star+1)])
    }
    invert_poly <- function(coef_poly, v_star) {
      coef_inverted <- rep(NA_real_, v_star+1)
      coef_inverted[1] <- 1
      for(i in 2:(v_star+1)) coef_inverted[i] <- -sum(coef_poly[2:i]*coef_inverted[(i-1):1])
      return(coef_inverted/coef_poly[1])
    }
    multiply_poly <- function(coef_poly1, coef_poly2, v_star) sum(coef_poly1[1:(v_star+1)]*coef_poly2[(v_star+1):1])
    
    v <- sort(population_sizes, decreasing = TRUE)
    v_star <- total_size_population - v[1]
    
    coef_num <- compute_coef_poly(v, v_star)
    coef_def <- compute_coef_poly(rep(0, length(v)-1), v_star)
    return(multiply_poly(coef_num, invert_poly(coef_def, v_star), v_star)/prod(v[-1]+1))
  }
  
  population_sizes <- as.integer(lapply(list_networks, function(x) length(unique(unlist(x)))))
  maximum_size_population <- max(population_sizes)
  total_size_population <- length(unique(unlist(list_networks)))
  product <- prod(as.double(lapply(population_sizes, function(x) 1-x/(maximum_size_population + overlap))))
  
  return(
    + dbinom(log = TRUE, x = total_size_population, size = maximum_size_population + overlap, prob = 1 - product) 
    - log(
      + pbinom(q = sum(population_sizes),        size = maximum_size_population + overlap, prob = 1 - product)
      - pbinom(q = maximum_size_population - 1L, size = maximum_size_population + overlap, prob = 1 - product)
    )
    + log(prob(population_sizes, total_size_population))
  )
}
