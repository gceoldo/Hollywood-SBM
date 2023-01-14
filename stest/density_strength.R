real_strength <- 0.8
real_discount <- 0.1
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/generative_model.R")
library(hash)

null_to_zero <- function(value) {
  if(!is.null(value)) return(value)
  else return(0L)
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


n_samples <- 50L
grid <- seq(from = 0.1, to = 0.99, by = 0.03)
logratios <- matrix(NA_integer_, n_samples, length(grid))
for(i in 1:n_samples) {
  print(i)
  assignment <- trunc(runif(50, 1, 5))
  set_relations <- sample_HBM(assignment, real_strength, real_discount, 100L, function() 3L)
  ldreal <- sum(sapply(split(set_relations, assignment), function(x) logdhollywood(x, real_strength, real_discount)))
  for(j in seq_along(grid)) logratios[i, j] <- sum(sapply(split(set_relations, assignment), function(x) logdhollywood(x, grid[j], real_discount))) - ldreal
}

ylimits <- c(-100, 0)
par(mfrow=c(1,1))
plot(0,0, type="n", xlim=range(grid), ylim=c(-100,max(logratios)))
for(i in 1:n_samples) {
  #points(grid, logratios[i,], pch = 16, cex = .5)
  points(grid, logratios[i,], type="l")
} 
abline(h = 0)
abline(v = real_strength, col=2)
abline(h = log(qunif(c(.1,.01,.001))), col = 2)

