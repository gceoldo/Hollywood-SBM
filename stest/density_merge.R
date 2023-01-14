real_overlap <- 100L
source("/Users/giacomoceoldo/Github/Holliwood-SBM/v5/generative_model.R")

logdmerge <- function(list_networks, overlap) {
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
  )
}

n_samples <- 50L
grid <- seq(from = 0L, to = 500L, by = 1L)
densities <- matrix(NA_integer_, n_samples, length(grid))
for(i in 1:n_samples) {
  print(i)
  assignment <- trunc(runif(200, 1, 5))
  set_relations <- sample_HBM(assignment, .8, .1, real_overlap, function() 2L+rpois(1,2))
  densities[i, ] <- sapply(grid, function(x) logdmerge(split(set_relations, assignment), x))
}

lower_lim <- min(densities[which(densities != -Inf)])
plot(0,0, type="n", xlim=range(grid), ylim=c(-10,max(densities[which(densities != -Inf)])))
for(i in 1:length(grid)) {
  if(max(densities[,i]) < Inf) {
    #if(min(densities[,i]) > -Inf) points(rep(grid[i],2), range(densities[,i]), type="l")
    #else points(rep(grid[i],2), c(2*lower_lim, max(densities[,i])), type="l")
    #points(grid[i], median(densities[,i]), pch = 16)
    points(rep(grid[i],length(densities[,i])), densities[,i], pch = 20, cex = .1)
  }
} 
abline(h = 0)
abline(v = real_overlap, col=2, lty = 1, lwd = 2)
abline(h = log(qunif(c(.1,.01,.001))), col = 2)

a <- density(grid[apply(densities, 1, which.max)])
points(a$x, -10 + 50*a$y, type="l", col = 4, lwd = 2)
