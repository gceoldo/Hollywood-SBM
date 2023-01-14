source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/generative_model.R")

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
compute_hashmultiset_degrees_from_relation <- function(relation) { 
  degrees <- hash::hash()
  for(i in seq_along(relation)) degrees[[as.character(relation[i])]] <- null_to_zero(degrees[[as.character(relation[i])]]) + 1L
  return(degrees) 
}
compute_degrees_combining_degrees_individual_relations <- function(degrees_individual_relations) {
  degrees <- hash::hash()
  for(i in 1:length(degrees_individual_relations)) {
    hashmultiset_degrees_in_relation_i <- degrees_individual_relations[[i]]
    keys_relation_i <- hash::keys(hashmultiset_degrees_in_relation_i)
    for(j in seq_along(keys_relation_i)) {
      degrees[[keys_relation_i[j]]] <- null_to_zero(degrees[[keys_relation_i[j]]]) + hashmultiset_degrees_in_relation_i[[keys_relation_i[j]]] 
    }
  } 
  return(degrees)
}
logdprior <- function(assignment) {
  sizes_communities <- as.integer(table(assignment))
  return(
    + sum(lgamma(sizes_communities + 1)) - lgamma(length(assignment) + 1)
    + lgamma(length(sizes_communities)) + lgamma(length(assignment) - length(sizes_communities) + 1) - lgamma(length(assignment))
    - log(length(assignment))
  )
}
logdhollywood <- function(set_relations, alpha, theta) {
  v <- length(unique(unlist(set_relations)))
  m <- sum(sapply(set_relations, length))
  deg <- compute_degrees_combining_degrees_individual_relations(lapply(set_relations, compute_hashmultiset_degrees_from_relation))
  unique_deg <- as.integer(sort(unique(hash::values(deg))))
  deg_freq <- as.integer(table(hash::values(deg)))
  return(
    v*log(alpha)  
    + lgamma(theta/alpha + v) - lgamma(theta/alpha) 
    - lgamma(theta + m) + lgamma(theta)
    + sum( deg_freq * (lgamma(unique_deg - alpha) - lgamma(1 - alpha)))
  )
}

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


entropy <- function(assignment) {
  proportions <- as.integer(table(assignment))/length(assignment)
  return(-sum(proportions * log(proportions)))
}

#which_model <- 2

#length_assignment <- 50L * c(3L, 1L)[which_model]
#real_overlap <- 22L
#real_strength <- .8
#real_discount <- .1
#arity_parameter1 <- c(2L,6L)[which_model]
#arity_parameter2 <- 0L

length_assignment <- 50L
max_number_communities <- 50L
real_overlap <- 102L
real_strength <- .8
real_discount <- .1
arity_parameter1 <- 20L
arity_parameter2 <- 0L

n_real_samples <- 50L 
n_samples <- 50L


effective_real <- density_real <- effective_real_fixed_ncomm <- density_real_fixed_ncomm <- rep(NA_real_, n_real_samples)
densities <- effectives <- densities_fixed_ncomm <- effectives_fixed_ncomm <- matrix(NA_real_, n_real_samples, n_samples)
overlaps <- overlaps_fixed_ncomm <- matrix(NA_integer_, n_real_samples, n_samples)


for(i in 1:n_real_samples) {
#for(i in 1:n_real_samples) {
  print(i)
  real_assignment <- sample_prior_community(length_assignment, cc_max = max_number_communities)
  effective_real[i] <- exp(entropy(real_assignment))
  set_relations <- sample_HBM(relabel(real_assignment), real_strength, real_discount, real_overlap, function() arity_parameter1+rpois(1,arity_parameter2))
  density_real[i] <- (
    logdprior(real_assignment) 
    + sum(unlist(lapply(split(set_relations, real_assignment), function(x) logdhollywood(x, real_strength, real_discount)))) 
    + logdmerge(split(set_relations, real_assignment), real_overlap)
  ) 
  if(is.infinite(density_real[i])) break
  total_size_population <- length(unique(unlist(set_relations)))
  for(j in 1:n_samples) {
  # for(j in 1:n_samples) {  
    sampled_assignment <- sample_prior_community(length_assignment, cc_max = max_number_communities)
    effectives[i,j] <- exp(entropy(sampled_assignment))
    list_networks <- split(set_relations, sampled_assignment)
    maximum_size_population <- max(as.integer(lapply(list_networks, function(x) length(unique(unlist(x))))))
    overlaps[i,j]  <- max(total_size_population - maximum_size_population, real_overlap)
    densities[i,j] <- (
      logdprior(sampled_assignment) 
      + sum(unlist(lapply(list_networks, function(x) logdhollywood(x, real_strength, real_discount)))) 
      + logdmerge(list_networks, overlaps[i,j])
    ) 
    #if(is.nan(densities[i,j])) stop()
  }
}
logratios <- matrix(matrix(rep(density_real), n_real_samples, n_samples) - densities, n_real_samples, n_samples)
mean(logratios < 0)


for(i in 1:n_real_samples) {
  print(i)
  real_assignment <- sample_prior_community(length_assignment, cc_max = max_number_communities)
  effective_real_fixed_ncomm[i] <- exp(entropy(real_assignment))
  set_relations <- sample_HBM(relabel(real_assignment), real_strength, real_discount, real_overlap, function() arity_parameter1+rpois(1,arity_parameter2))
  density_real_fixed_ncomm[i] <- (
    logdprior(real_assignment)
    + sum(unlist(lapply(split(set_relations, real_assignment), function(x) logdhollywood(x, real_strength, real_discount))))
    + logdmerge(split(set_relations, real_assignment), real_overlap)
  ) 
  total_size_population <- length(unique(unlist(set_relations)))
  for(j in 1:n_samples) {
    sampled_assignment <- sample_prior_community(length_assignment, length(unique(real_assignment)))
    effectives_fixed_ncomm[i,j] <- exp(entropy(sampled_assignment))
    list_networks <- split(set_relations, sampled_assignment)
    maximum_size_population <- max(as.integer(lapply(list_networks, function(x) length(unique(unlist(x))))))
    overlaps_fixed_ncomm[i,j]  <- max(total_size_population - maximum_size_population, real_overlap)
    densities_fixed_ncomm[i,j] <- (
      logdprior(sampled_assignment)
      + sum(unlist(lapply(list_networks, function(x) logdhollywood(x, real_strength, real_discount))))
      + logdmerge(list_networks, overlaps_fixed_ncomm[i,j])
    ) 
  }
}
logratios_fixed_ncomm <- matrix(matrix(rep(density_real_fixed_ncomm), n_real_samples, n_samples) - densities_fixed_ncomm, n_real_samples, n_samples)
mean(logratios_fixed_ncomm < 0)




par(mfrow=c(2,2))
plot(0,0,type="n", xlim=c(0, max_number_communities), ylim = range(logratios[which(abs(logratios) < Inf)]), main = "MH logratio, sample from full prior", xlab = "effective number of communities real assignment + noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(rep(effective_real[i] + rnorm(1,0,.1), n_samples), logratios[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = 0)
abline(v = c(1, max_number_communities), lty=3)
abline(h = range(logratios_fixed_ncomm[which(abs(logratios_fixed_ncomm) < Inf)]), lty = 3)

diffeffectives <- matrix(matrix(rep(effective_real), n_real_samples, n_samples) - effectives, n_real_samples, n_samples)
plot(0,0,type="n", xlim=range(diffeffectives), ylim = range(logratios[which(abs(logratios) < Inf)]), main = "MH logratio, sample from full prior", xlab = "difference effective number of communities, real e.n.c. - sampled e.n.c.", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(diffeffectives[i,], logratios[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = 0)
abline(v = 0)

diffeffectives_fixed_ncomm <- matrix(matrix(rep(effective_real_fixed_ncomm), n_real_samples, n_samples) - effectives_fixed_ncomm, n_real_samples, n_samples)
points(range(diffeffectives_fixed_ncomm)[c(1,2,2,1,1)], range(logratios_fixed_ncomm[which(abs(logratios_fixed_ncomm) < Inf)])[c(1,1,2,2,1)], type="l", lty=3)
#abline(h = range(logratios_fixed_ncomm), lty = 3)
#abline(v = range(diffeffectives_fixed_ncomm), lty = 3)


plot(0,0,type="n", xlim=c(0, max_number_communities), ylim = range(logratios_fixed_ncomm[which(abs(logratios_fixed_ncomm) < Inf)]), main = "MH logratio, sample from prior, given real number communities", xlab = "effective number of communities real assignment + noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(rep(effective_real_fixed_ncomm[i] + rnorm(1,0,.1), n_samples), logratios_fixed_ncomm[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = 0)
abline(v = c(1, 250L), lty=3)
abline(h = range(logratios_fixed_ncomm[which(abs(logratios_fixed_ncomm) < Inf)]), lty = 3)

#diffeffectives_fixed_ncomm <- matrix(matrix(rep(effective_real_fixed_ncomm), n_real_samples, n_samples) - effectives_fixed_ncomm, n_real_samples, n_samples)
plot(0,0,type="n", xlim=range(diffeffectives_fixed_ncomm), ylim = range(logratios_fixed_ncomm[which(logratios_fixed_ncomm>-Inf)]), main = "MH logratio, sample from prior, given real number communities", xlab = "difference effective number of communities, real e.n.c. - sampled e.n.c.", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(diffeffectives_fixed_ncomm[i,], logratios_fixed_ncomm[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = 0)
abline(v = 0)
points(range(diffeffectives_fixed_ncomm)[c(1,2,2,1,1)], range(logratios_fixed_ncomm[which(logratios_fixed_ncomm>-Inf)])[c(1,1,2,2,1)], type="l", lty=3)



##

#plot(0,0,type="n", xlim=c(0, length_assignment), ylim = range(logratios_fixed_effnc), main = "MH logratio, sample from prior, given real community sizes", xlab = "effective number of communities real assignment + noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
#for(i in 1:n_real_samples) points(rep(effective_real_fixed_effnc[i] + rnorm(1,0,.1), n_samples), logratios_fixed_effnc[i,], col = rgb(0,0,0,.1), pch = 16)
#abline(h = 0)

#diffeffectives_fixed_effnc <- matrix(matrix(rep(effective_real_fixed_effnc), n_real_samples, n_samples) - effectives_fixed_effnc, n_real_samples, n_samples)
#plot(0,0,type="n", xlim=c(-.3,.3), ylim = range(logratios_fixed_effnc), main = "MH logratio, sample from prior, given real community sizes", xlab = "noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
#for(i in 1:n_real_samples) points(diffeffectives_fixed_effnc[i,] + rnorm(1, 0, .1), logratios_fixed_effnc[i,], col = rgb(0,0,0,.1), pch = 16)
#abline(h = 0)



par(mfrow=c(3,2))
plot(0,0,type="n", xlim=c(0, length_assignment), ylim = range(overlaps), main = "MH logratio, sample from full prior", xlab = "effective number of communities real assignment + noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(rep(effective_real[i] + rnorm(1,0,.1), n_samples), overlaps[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = real_overlap)

diffeffectives <- matrix(matrix(rep(effective_real), n_real_samples, n_samples) - effectives, n_real_samples, n_samples)
plot(0,0,type="n", xlim=range(diffeffectives), ylim = range(overlaps), main = "MH logratio, sample from full prior", xlab = "difference effective number of communities, real e.n.c. - sampled e.n.c.", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(diffeffectives[i,], overlaps[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = real_overlap)
abline(v = 0)

plot(0,0,type="n", xlim=c(0, length_assignment), ylim = range(overlaps_fixed_ncomm), main = "MH logratio, sample from prior, given real number communities", xlab = "effective number of communities real assignment + noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(rep(effective_real_fixed_ncomm[i] + rnorm(1,0,.1), n_samples), overlaps_fixed_ncomm[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = real_overlap)

diffeffectives_fixed_ncomm <- matrix(matrix(rep(effectives_fixed_ncomm), n_real_samples, n_samples) - effective_real_fixed_ncomm, n_real_samples, n_samples)
plot(0,0,type="n", xlim=range(diffeffectives_fixed_ncomm), ylim = range(overlaps_fixed_ncomm), main = "MH logratio, sample from prior, given real number communities", xlab = "difference effective number of communities, real e.n.c. - sampled e.n.c.", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(diffeffectives_fixed_ncomm[i,], overlaps_fixed_ncomm[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = real_overlap)
abline(v = 0)

plot(0,0,type="n", xlim=c(0, length_assignment), ylim = range(overlaps_fixed_effnc), main = "MH logratio, sample from prior, given real community sizes", xlab = "effective number of communities real assignment + noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(rep(effective_real_fixed_effnc[i] + rnorm(1,0,.1), n_samples), overlaps_fixed_effnc[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = 0)

diffeffectives_fixed_effnc <- matrix(matrix(rep(effectives_fixed_effnc), n_real_samples, n_samples) - effective_real_fixed_effnc, n_real_samples, n_samples)
plot(0,0,type="n", xlim=range(diffeffectives_fixed_effnc), ylim = range(overlaps_fixed_effnc), main = "MH logratio, sample from prior, given real community sizes", xlab = "noise(sigma = 0.1)", ylab = "logdensity real - logdensity sampled")
for(i in 1:n_real_samples) points(diffeffectives_fixed_effnc[i,] + rnorm(1, 0, .1), overlaps_fixed_effnc[i,], col = rgb(0,0,0,.1), pch = 16)
abline(h = 0)





generate_partitions <- function(number_values) {
  add_digit_to_string <- function(string = integer()) {
    maximum_value <- max(max(string), 0L)
    lapply(1:(maximum_value+1L), function(x) c(string, x))
  }
  vec_to_list <- function(vec, size) {
    li <- vector("list", length(vec)/size)
    for(i in seq_along(li)) li[[i]] <- vec[((i-1)*size)+1:size]
    li
  }
  current_state <- suppressWarnings(add_digit_to_string())
  if(number_values > 1L) for(i in 2:number_values) current_state <- vec_to_list(unlist(lapply(current_state, add_digit_to_string)), i)
  current_state
}
# generate_partitions(5) # Bell(5) = 52
#sum(unlist(lapply(generate_partitions(2), function(x) exp(logdprior(x)))))

generate_increasing_partitions <- function(number_values) {
  add_digit_to_string2 <- function(string = 1L) {
    last_value <- tail(string,1)
    lapply(last_value:(last_value+1L), function(x) c(string, x))
  }
  vec_to_list <- function(vec, size) {
    li <- vector("list", length(vec)/size)
    for(i in seq_along(li)) li[[i]] <- vec[((i-1)*size)+1:size]
    li
  }
  current_state <- list(1L)
  if(number_values > 1L) for(i in 2:number_values) current_state <- vec_to_list(unlist(lapply(current_state, add_digit_to_string2)), i)
  return(current_state)
}
# generate_increasing_partitions(4) # 2^3

generate_all_samples <- function(number_values) {
  partitions <- generate_partitions(number_values)
  increasing_partitions <- generate_increasing_partitions(number_values)
  samples <- vector("list", length(partitions))
  for(i in seq_along(samples)) samples[[i]] <- lapply(increasing_partitions, function(x) split(partitions[[i]], x))
  return(samples)
}

# generate_all_samples(3)


tree_to_list <- function(tree) {
  list_leaves <- list()
  add_leaves_to_list <- function(tree) {
    if(all(unlist(lapply(tree, is.numeric)))) list_leaves <<- c(list_leaves, list(tree))
    else for(i in seq_along(tree)) add_leaves_to_list(tree[[i]])
  }
  add_leaves_to_list(tree)
  return(list_leaves)
}

tree_to_list(list(1,2))
tree_to_list(list(list(1,2),list(3,4,5)))
tree_to_list(list(list(1,2),
                  list(3,4,5),
                  list(list(6,7), 
                       list(8,9))
                  ))

tree_to_list(generate_all_samples(4)) # Bell(4) * 2^(4-1) = 15 * 8

samples <- tree_to_list(generate_all_samples(4))
lapply(samples, function(x) exp(logdhollywood(x, .8, 1)))
samples[1:10]



generate_all_samples_with_fixed_arity <- function(number_values, arity) {
  partitions <- generate_partitions(number_values)
  increasing_partitions <- rep(1:(number_values/arity), each = arity)
  samples <- vector("list", length(partitions))
  for(i in seq_along(samples)) samples[[i]] <- split(partitions[[i]], increasing_partitions)
  return(samples)
}

samples <- tree_to_list(generate_all_samples_with_fixed_arity(4,2))
sum(unlist(lapply(samples, function(x) exp(logdhollywood(x, .8, 1)))))
length(samples)

b <- lapply(samples, function(x) c(sapply(x, function(y) length(unique(y))), max(c(sapply(x, function(y) length(unique(y))))), length(unique(unlist(x)))))
networks <- matrix(NA_integer_, length(samples), 4)
for(i in seq_along(b)) networks[i,] <- b[[i]]
ord <- c(1,4, 6,10,2,3,5,14, 7,9,8,11,12,13, 15)
networks <- networks[ord,]
samples <- samples[ord]

multinetworks <- vector("list", length(samples))
#communities <- c(1,1,1,2)
communities <- c(1,2)
for(i in seq_along(multinetworks)) multinetworks[[i]] <- split(samples[[i]], communities)
a <- unlist(lapply(multinetworks, function(x) prod(sapply(x, function(y) exp(logdhollywood(y, .8, 1)))) * exp(logdmerge(x, 50L))))
sum(a)
#sum(unique(unlist(lapply(multinetworks, function(x) prod(sapply(x, function(y) exp(logdhollywood(y, .8, 1)))) * exp(logdmerge(x, 15L))))))
numerators   <- c(1,1, 4,4,4,4, 2,2, 2,2, 4,4,4,4, 1)
denominators <- c(2,2, 6,6,6,6, 6,6, 7,7, 7,7,7,7, 7)
#numerators   <- c(1,1, 1,1,1,1, 1,1, 1,1, 1,1,1,1, 1)
#denominators <- c(2,2, 4,4,4,4, 2,2, 2,2, 4,4,4,4, 1)
#numerators   <- c(1,1, 1,1,1,1, 1,1, 1,1, 1,1,1,1, 1)
#denominators <- c(2,2, 6,6,6,6, 6,6, 7,7, 7,7,7,7, 7)
sum(a * numerators/denominators)
values_sum <- rep(NA_real_, 100)
for(i in 0:(length(values_sum) - 1)) {
  a <- unlist(lapply(multinetworks, function(x) prod(sapply(x, function(y) exp(logdhollywood(y, .8, 1)))) * exp(logdmerge(x, i))))
  values_sum[i+1] <- sum(a * numerators/denominators)
}
plot(0:(length(values_sum) - 1), values_sum)
abline(h = 1)

d <- unlist(lapply(multinetworks, function(x) exp(logdmerge(x, 15L))))
sum(d[1:2])
sum(d[c(3,7)])
sum(d[c(9,11,15)])
sum(d/c(1,1, 4,4,4,4, 2,2, 2,2, 4,4,4,4, 1))



samples <- tree_to_list(generate_all_samples_with_fixed_arity(4,1))






generate_all_community_structures_with_fixed_arity <- function(number_values, arity) {
  number_relations <- number_values / arity
  samples <- generate_all_samples_with_fixed_arity(number_values, arity)
  partitions <- generate_partitions(number_relations)
  community_structures <- vector("list", length(samples))
  for(i in seq_along(community_structures)) community_structures[[i]] <- lapply(partitions, function(x) split(samples[[i]], x))
  return(community_structures)
}

samples <- generate_all_community_structures_with_fixed_arity(6, 2)

length(samples)
names(samples[[1]])
sapply(samples[[1]], length)
lapply(samples[[1]], names)
samples[[1]][[1]]
samples[[1]][[2]]



samples <- tree_to_list(generate_all_samples_with_fixed_arity(8,2))
community_assignment <- c(1,2,1,3)
statistics <- matrix(NA_integer_, length(samples), length(unique(community_assignment)) + 1)
colnames(statistics) <- c(paste0("v", sort(unique(community_assignment))), "v")
statistics[1:5,]
for(i in seq_along(samples)) {
  list_actors <- lapply(split(samples[[i]], community_assignment), function(x) unique(unlist(x)))
  statistics[i,1:3] <- sapply(list_actors, length)
  statistics[i,4] <- length(unique(unlist(list_actors)))
}

statistics[1:10,]


f <- function(vbarj, vsortj, ibarj, ij) choose(vbarj - ibarj, ij) * choose(vsortj, ij) * factorial(ij)


g <- function(v) {
  vsort <- sort(v, decreasing = TRUE)
  vbar <- rep(0L, length(v))
  for(i in 1:length(v)) vbar[i] <- sum(vsort[1:i])
  I <- vector("list", length(v))
  I[[1]] <- 0
  for(i in 2:length(v)) I[[i]] <- 0:vsort[i]
  return(list(vsort=vsort, vbar=vbar, I=I))
}

D <- function() {}



