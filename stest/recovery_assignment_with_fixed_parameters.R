library(hash)

path_files <- "/Users/giacomoceoldo/Github/Holliwood-SBM/v5/"

source(paste0(path_files, "generative_model.R"))
source(paste0(path_files, "utils.R"))

source(paste0(path_files, "Label_Relations.R"))
source(paste0(path_files, "Model.R"))

source(paste0(path_files, "Community_Structure.R"))
source(paste0(path_files, "Network.R"))
source(paste0(path_files, "Parametric_Space.R"))

source(paste0(path_files, "Community_Assignment.R"))
source(paste0(path_files, "Multinetwork.R"))
source(paste0(path_files, "Parameter.R"))

source(paste0(path_files, "Community_Flip.R"))
source(paste0(path_files, "ChainCommunity.R"))

size_set_relations <- 100L
label_relations <- LabelRelations(size_set_relations)
model <- Model("PitmannYor", "uniform")
parametric_space <- ParametricSpace(model, strength_is_global = TRUE, discount_is_global = TRUE)
community_structure <- CommunityStructure(label_relations, number_communities_is_fixed = TRUE)

arity_parameter1 <- 2L
arity_parameter2 <- 0L

real_assignment <- rep(1:4, size_set_relations/4)[sample.int(100,100)]
size_network <- length(real_assignment)
initial_values_parameter <- list(strength = .8, discount = 1.0, overlap = 25L)
set_relations <- sample_HBM(real_assignment, initial_values_parameter$strength, initial_values_parameter$discount,
                            initial_values_parameter$overlap, function() arity_parameter1 + rpois(1, arity_parameter2))

network <- Network(label_relations, set_relations)

initial_assignment <- sample_prior_community(size_set_relations, length(unique(real_assignment))); table(initial_assignment)
community_assignment <- CommunityAssignment(label_relations, community_structure, initial_assignment)
multinetwork <- Multinetwork(community_assignment, network)
parameter <- Parameter(community_assignment, parametric_space, initial_values_parameter)

length_chain_MCMC <- 2000L

community_flip <- CommunityFlip()
chain_community <- ChainCommunity(length_chain_MCMC)

for(i in 1:length_chain_MCMC) {
  if(i %% 1000L == 0L) print(sprintf("iteration %d/%d", i,length_chain_MCMC))
  info_flip <- metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
  store_proposed_flip(chain_community, info_flip)
}
table(assignment(community_assignment))

compute_moving_average <- function(x, n){filter(x, rep(1 / n, n), sides = 2)}
window_moving_average  <- 50L
moving_average <- compute_moving_average(chain_community@state$chain$accepted, window_moving_average)
plot(NULL,NULL,type="n", xlim=c(0, length_chain_MCMC), ylim=c(0,1), xlab = "", ylab = "")
points(25:(length_chain_MCMC - 25), moving_average[25:(length_chain_MCMC - 25)], type="l")
mean_acceptance_rate <- mean(chain_community@state$chain$accepted[-(1:1000)])
abline(h=mean_acceptance_rate)
title("moving average acceptance rate, window 50", xlab = "iteration", ylab = "acceptance rate")


number_samples <- 50L
number_iterations_per_sample <- as.integer(length(initial_assignment) / mean_acceptance_rate) / 3L

assignments <- matrix(NA_integer_, number_samples, ncol = size_network)
for(i in 1:number_samples) {
  print(sprintf("sample %d/%d", i,number_samples))
  for(j in 1:number_iterations_per_sample) metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
  assignments[i,] <- assignment(community_assignment)
}
t(apply(assignments, 1, function(x) as.integer(table(x))))

array_same_communities <- array(NA_integer_, dim = c(number_samples, size_network, size_network))
order_real_assignment <- order(real_assignment, decreasing = FALSE)
real_assignment[order_real_assignment]
assignments <- assignments[, order_real_assignment]

for(i in 1:number_samples) {
  for(j in 1:size_network) {
    for(h in 1:size_network) {
      array_same_communities[i,j,h] <- assignments[i,j] == assignments[i,h]
    }
  }
}

array_same_communities[1:5,1:3,1:3]
probability_same_communities <- apply(array_same_communities, c(2,3), mean)

par(mar=c(.5,.5,2,.5))
size_square <- 1
plot(NULL, NULL, type = "n", xlim = c(0, 100), ylim = c(0, 100), main = "probability relations are the same community", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
for(i in 1:100) {
  for(j in 1:i) {
    polygon(-1 + c(j, j+size_square, j+size_square, j), (100 - i) + c(0, 0, size_square, size_square), border = NA, 
            col = rgb(0,0,0,probability_same_communities[i,j]))
  }
}
for(i in 1:3) segments(25 * i - .1, 0, 25 * i - .1, 100, col = 2)
for(i in 1:3) segments(0, 25 * i - .1, 100, 25 * i - .1, col = 2)

block_averages <- matrix(NA, 4,4)
mean_diagonal_block <- function(matrix_prob) mean(matrix_prob[lower.tri(matrix_prob)])
mean_nondiagonal_block <- function(matrix_prob) mean(matrix_prob)

for(i in 1:4) {
  for(j in 1:i) {
    if(i == j) block_averages[i, j] <- mean_diagonal_block(probability_same_communities[25*(i-1)+1:25, 25*(i-1)+1:25])
    else block_averages[i, j] <- mean_nondiagonal_block(probability_same_communities[25*(i-1)+1:25, 25*(j-1)+1:25])
  }
}
  
for(j in 1:100) {
  for(i in 1:j) {
    polygon(-1 + c(j, j+size_square, j+size_square, j), (100 - i) + c(0, 0, size_square, size_square), border = NA, 
            col = rgb(0,0,0,t(block_averages)[1+((i-1) %/% 25),1+((j-1) %/% 25)]))
  }
}






