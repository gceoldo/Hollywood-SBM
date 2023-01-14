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

source(paste0(path_files, "Parameter_Update.R"))
source(paste0(path_files, "Community_Flip.R"))
source(paste0(path_files, "Community_Unrestricted_Flip.R"))
source(paste0(path_files, "ChainCommunity.R"))

size_set_relations <- 100L
label_relations <- LabelRelations(size_set_relations)
model <- Model("PitmannYor", "uniform")
parametric_space <- ParametricSpace(model, strength_is_global = TRUE, discount_is_global = TRUE)
community_structure <- CommunityStructure(label_relations, number_communities_is_fixed = FALSE)

real_number_communities <- 4L
arity_parameter1 <- 2L
arity_parameter2 <- 0L

real_assignment <- rep(1:real_number_communities, size_set_relations/real_number_communities)[sample.int(size_set_relations,size_set_relations)]
size_network <- length(real_assignment)
real_overlap <- 25L
initial_values_parameter <- list(strength = .8, discount = 1.0, overlap = real_overlap)
set_relations <- sample_HBM(real_assignment, initial_values_parameter$strength, initial_values_parameter$discount,
                            initial_values_parameter$overlap, function() arity_parameter1 + arity_parameter2)

network <- Network(label_relations, set_relations)

initial_assignment <- sample_prior_community(size_set_relations); table(initial_assignment)
#initial_assignment <- sample_prior_community(size_set_relations, 2*real_number_communities)
community_assignment <- CommunityAssignment(label_relations, community_structure, initial_assignment)
multinetwork <- Multinetwork(community_assignment, network)

initial_values_parameter$overlap <- max(size_population(network) - size_largest_population(multinetwork) + 1L, real_overlap)
parameter <- Parameter(community_assignment, parametric_space, initial_values_parameter)


length_chain_MCMC <- 10000L

community_flip <- CommunityFlip()
community_unrestricted_flip <- CommunityUnrestrictedFlip()
chain_community <- ChainCommunity(length_chain_MCMC)
chain_effective_number_communities <- rep(NA_real_, length_chain_MCMC)
parameter_update <- ParameterUpdate(parameter)


for(i in 1:length_chain_MCMC) {
  if(i %% 1000L == 0L) {
    print(sprintf("iteration %d/%d", i,length_chain_MCMC))
  } 
  if(rbinom(1,1,1)) info_flip <- metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
  else info_flip <- metropolis_hastings_uflip(community_unrestricted_flip, .1, community_assignment, multinetwork, parameter, network)
  store_proposed_flip(chain_community, info_flip)
  chain_effective_number_communities[i] <- exp(entropy(community_assignment))
  #metropolis_hastings_overlap(parameter_update, parameter, multinetwork, network)
  #metropolis_hastings_strength(parameter_update, parameter, multinetwork)
}
overlap(parameter)
table(assignment(community_assignment))

par(mfrow = c(1,3))
par(mar = c(5.1, 4.1, 4.1, 2.1))
compute_moving_average <- function(x, n){filter(x, rep(1 / n, n), sides = 2)}
window_moving_average  <- 50L
moving_average <- compute_moving_average(chain_community@state$chain$accepted, window_moving_average)
plot(NULL,NULL,type="n", xlim=c(0, length_chain_MCMC), ylim=c(0,1), xlab = "", ylab = "")
points(25:(length_chain_MCMC - 25), moving_average[25:(length_chain_MCMC - 25)], type="l")
mean_acceptance_rate <- mean(chain_community@state$chain$accepted[-(1:1000)])
abline(h=mean_acceptance_rate)
title("moving average acceptance rate, window 50", xlab = "iteration", ylab = "acceptance rate")

plot(chain_effective_number_communities, type = "l", main = "chain effective number communities", xlab = "iteration", ylab = "effective number communities")
abline(h = exp(-sum(rep(0.25,4)* log(rep(0.25,4)))), col = 2)

acf(chain_effective_number_communities, lag.max = 1000, main = "ACF effective number communities")



number_samples <- 50L
number_iterations_per_sample <- as.integer(length(initial_assignment) / mean_acceptance_rate) 

assignments <- matrix(NA_integer_, number_samples, ncol = size_network)
for(i in 1:number_samples) {
  print(sprintf("sample %d/%d", i,number_samples))
  for(j in 1:number_iterations_per_sample) {
    if(rbinom(1,1,1)) metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
    else metropolis_hastings_uflip(community_unrestricted_flip, .1, community_assignment, multinetwork, parameter, network)
    
    #metropolis_hastings_overlap(parameter_update, parameter, multinetwork, network)
    #metropolis_hastings_strength(parameter_update, parameter, multinetwork)
  } 
  assignments[i,] <- assignment(community_assignment)
}
#t(apply(assignments, 1, function(x) as.integer(table(x))))
apply(assignments, 1, function(x) exp(entropy(CommunityAssignment(label_relations, community_structure, x))))

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

#array_same_communities[1:5,1:3,1:3]
probability_same_communities <- apply(array_same_communities, c(2,3), mean)

par(mfrow=c(1,1))
par(mar=c(.5,.5,2,.5))
size_square <- 1
plot(NULL, NULL, type = "n", xlim = c(0, size_set_relations), ylim = c(0, size_set_relations), main = "probability relations are the same community", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
for(i in 1:size_set_relations) {
  for(j in 1:i) {
    polygon(-1 + c(j, j+size_square, j+size_square, j), (size_set_relations - i) + c(0, 0, size_square, size_square), border = NA, 
            col = rgb(0,0,0,probability_same_communities[i,j]))
  }
}

number_relations_per_community <- size_set_relations / real_number_communities
for(i in 1:(real_number_communities-1L)) segments(number_relations_per_community * i, 0, number_relations_per_community * i, size_set_relations, col = 2)
for(i in 1:(real_number_communities-1L)) segments(0, number_relations_per_community * i, size_set_relations, number_relations_per_community * i, col = 2)

block_averages <- matrix(NA, real_number_communities, real_number_communities)
mean_diagonal_block <- function(matrix_prob) mean(matrix_prob[lower.tri(matrix_prob)])
mean_nondiagonal_block <- function(matrix_prob) mean(matrix_prob)

for(i in 1:real_number_communities) {
  for(j in 1:i) {
    if(i == j) block_averages[i, j] <- mean_diagonal_block(probability_same_communities[number_relations_per_community*(i-1)+1:number_relations_per_community, number_relations_per_community*(i-1)+1:number_relations_per_community])
    else block_averages[i, j] <- mean_nondiagonal_block(probability_same_communities[number_relations_per_community*(i-1)+1:number_relations_per_community, number_relations_per_community*(j-1)+1:number_relations_per_community])
  }
}

for(j in 1:size_set_relations) {
  for(i in 1:j) {
    polygon(-1 + c(j, j+size_square, j+size_square, j), (size_set_relations - i) + c(0, 0, size_square, size_square), border = NA, 
            col = rgb(0,0,0,t(block_averages)[1+((i-1) %/% number_relations_per_community),1+((j-1) %/% number_relations_per_community)]))
  }
}

