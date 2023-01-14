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
source(paste0(path_files, "Parameter_Update.R"))
source(paste0(path_files, "Chain_Local_Parameter.R"))

real_strength <- 0.8
real_discount <- 4.5
real_overlap  <- 400L


size_set_relations <- 1000L
label_relations <- LabelRelations(size_set_relations)
model <- Model("PitmannYor", "uniform")
parametric_space <- ParametricSpace(model, strength_is_global = TRUE, discount_is_global = TRUE)
community_structure <- CommunityStructure(label_relations, number_communities_is_fixed = TRUE)

real_assignment <- rep(1:4, size_set_relations/4)[sample.int(1000,1000)]
size_network <- length(real_assignment)
initial_values_parameter <- list(strength = real_strength, discount = real_discount, overlap = real_overlap)
set_relations <- sample_HBM(real_assignment, real_strength, real_discount,
                            real_overlap, function() 2L + rpois(1, 3))

network <- Network(label_relations, set_relations)

initial_assignment <- sample_prior_community(size_set_relations, length(unique(real_assignment))); table(initial_assignment)
community_assignment <- CommunityAssignment(label_relations, community_structure, initial_assignment)
multinetwork <- Multinetwork(community_assignment, network)
parameter <- Parameter(community_assignment, parametric_space, initial_values_parameter)

length_chain_MCMC <- 20000L

community_flip <- CommunityFlip()
parameter_update <- ParameterUpdate(parameter)

chain_community <- ChainCommunity(length_chain_MCMC)
chain_strength <- ChainParameter(length_chain_MCMC)
chain_discount <- ChainParameter(length_chain_MCMC)
chain_overlap  <- ChainParameter(length_chain_MCMC)


for(i in 1:length_chain_MCMC) {
  if(i %% 1000L == 0L) print(sprintf("iteration %d/%d", i,length_chain_MCMC))
  
  current_strength <- metropolis_hastings_strength(parameter_update, parameter, multinetwork)
  set_strength(parameter, current_strength)
  store_parameter(chain_strength, current_strength)
  
  #current_discount <- metropolis_hastings_discount(parameter_update, parameter, multinetwork)
  #set_discount(parameter, current_discount)
  #store_parameter(chain_discount, current_discount)
  
  current_overlap <- metropolis_hastings_overlap(parameter_update, parameter, multinetwork, network)
  set_overlap(parameter, current_overlap)
  store_parameter(chain_overlap, current_overlap)
  
  info_flip <- metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
  store_proposed_flip(chain_community, info_flip)
}
table(assignment(community_assignment))

par(mfrow=c(1,3))
plot(chain_strength@state$chain, main = "chain strength", xlab = "iteration", ylab = "strength")
abline(h = real_strength, col=2)
#plot(chain_discount@state$chain, main = "chain discount", xlab = "iteration", ylab = "discount")
#abline(h = real_discount, col=2)
plot(chain_overlap@state$chain,  main = "chain overlap", xlab = "iteration", ylab = "overlap")
abline(h = real_overlap, col=2)

compute_moving_average <- function(x, n){filter(x, rep(1 / n, n), sides = 2)}
window_moving_average  <- 50L
moving_average <- compute_moving_average(chain_community@state$chain$accepted, window_moving_average)
plot(NULL,NULL,type="n", xlim=c(0, length_chain_MCMC), ylim=c(0,1), xlab = "", ylab = "")
points(25:(length_chain_MCMC - 25), moving_average[25:(length_chain_MCMC - 25)], type="l")
mean_acceptance_rate <- mean(chain_community@state$chain$accepted[-(1:1000)])
abline(h=mean_acceptance_rate)
title("moving av. acc.rate, window 50, assignment", xlab = "iteration", ylab = "acceptance rate")


number_samples <- 50L
number_iterations_per_sample <- as.integer(length(initial_assignment) / mean_acceptance_rate) / 3L

assignments <- matrix(NA_integer_, number_samples, ncol = size_network)
list_chains_parameters <- vector("list", number_samples)
for(i in 1:number_samples) {
  print(sprintf("sample %d/%d", i,number_samples))
  list_chains_parameters[[i]] <- list(strength = ChainParameter(number_iterations_per_sample),
                                      discount = ChainParameter(number_iterations_per_sample),
                                      overlap  = ChainParameter(number_iterations_per_sample))
  
  for(j in 1:number_iterations_per_sample) {
    current_strength <- metropolis_hastings_strength(parameter_update, parameter, multinetwork)
    set_strength(parameter, current_strength)
    store_parameter(list_chains_parameters[[i]]$strength, current_strength)
    
    #current_discount <- metropolis_hastings_discount(parameter_update, parameter, multinetwork)
    #set_discount(parameter, current_discount)
    #store_parameter(list_chains_parameters[[i]]$discount, current_discount)
    
    current_overlap <- metropolis_hastings_overlap(parameter_update, parameter, multinetwork, network)
    set_overlap(parameter, current_overlap)
    store_parameter(list_chains_parameters[[i]]$overlap, current_overlap)
    
    metropolis_hastings_flip(community_flip, community_assignment, multinetwork, parameter, network)
  } 
  assignments[i,] <- assignment(community_assignment)
}


par(mfrow=c(1,3))
plot(0,0,type="n", xlim = c(.75,.85), ylim = c(0,75), main = "posterior strength", xlab = "strength", ylab = "probability")
for(i in 1:number_samples) points(density(list_chains_parameters[[i]]$strength@state$chain), type = "l")
points(density(unlist(lapply(list_chains_parameters, function(x) x$strength@state$chain))), type="l", col=2)
abline(v = real_strength, col=2)

#plot(0,0,type="n", xlim = c(0,40), ylim = c(0,.2), main = "posterior discount", xlab = "discount", ylab = "probability")
#for(i in 1:number_samples) points(density(list_chains_parameters[[i]]$discount@state$chain), type = "l")
#points(density(unlist(lapply(list_chains_parameters, function(x) x$discount@state$chain))), type="l", col=2)
#abline(v = real_discount, col=2)

#for(i in 1:number_samples) {
#  prob <- table(list_chains_parameters[[i]]$overlap@state$chain)/number_iterations_per_sample
#  for(i in names(prob)) points(as.numeric(i), prob[i], pch = 3)
#} 
prob <- table(unlist(lapply(list_chains_parameters, function(x) x$overlap@state$chain)))/(number_samples * number_iterations_per_sample)
plot(0,0,type="n", xlim = range(as.numeric(names(prob))), ylim = c(0,max(prob)), main = "posterior overlap", xlab = "overlap", ylab = "probability")
for(i in names(prob)) points(as.numeric(c(i,i)), c(0, prob[i]), type="l", lwd = 5)
abline(v = real_overlap, col = 2)

array_same_communities <- array(NA_integer_, dim = c(number_samples, size_network, size_network))
order_real_assignment <- order(real_assignment, decreasing = FALSE)
assignments <- assignments[, order_real_assignment]

for(i in 1:number_samples) {
  for(j in 1:size_network) {
    for(h in 1:size_network) {
      array_same_communities[i,j,h] <- assignments[i,j] == assignments[i,h]
    }
  }
}

probability_same_communities <- apply(array_same_communities, c(2,3), mean)

size_square <- 1
plot(NULL, NULL, type = "n", xlim = c(0, 1000), ylim = c(0, 1000), main = "probability relations are the same community", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
for(i in 1:1000) {
  for(j in 1:i) {
    polygon(-1 + c(j, j+size_square, j+size_square, j), (1000 - i) + c(0, 0, size_square, size_square), border = NA, 
            col = rgb(0,0,0,probability_same_communities[i,j]))
  }
}
for(i in 1:3) segments(250 * i, 0, 250 * i, 1000, col = 2)
for(i in 1:3) segments(0, 250 * i, 1000, 250 * i, col = 2)

block_averages <- matrix(NA, 4,4)
mean_diagonal_block <- function(matrix_prob) mean(matrix_prob[lower.tri(matrix_prob)])
mean_nondiagonal_block <- function(matrix_prob) mean(matrix_prob)

for(i in 1:4) {
  for(j in 1:i) {
    if(i == j) block_averages[i, j] <- mean_diagonal_block(probability_same_communities[250*(i-1)+1:250, 250*(i-1)+1:250])
    else block_averages[i, j] <- mean_nondiagonal_block(probability_same_communities[250*(i-1)+1:250, 250*(j-1)+1:250])
  }
}

for(j in 1:1000) {
  for(i in 1:j) {
    polygon(-1 + c(j, j+size_square, j+size_square, j), (1000 - i) + c(0, 0, size_square, size_square), border = NA, 
            col = rgb(0,0,0,t(block_averages)[1+((i-1) %/% 250),1+((j-1) %/% 250)]))
  }
}


sd_diagonal_block <- function(matrix_prob) sd(matrix_prob[lower.tri(matrix_prob)])
sd_nondiagonal_block <- function(matrix_prob) sd(matrix_prob)
block_sdev <- matrix(NA, 4,4)

for(i in 1:4) {
  for(j in 1:i) {
    if(i == j) block_sdev[i, j] <- sd_diagonal_block(probability_same_communities[250*(i-1)+1:250, 250*(i-1)+1:250])
    else block_sdev[i, j] <- sd_nondiagonal_block(probability_same_communities[250*(i-1)+1:250, 250*(j-1)+1:250])
  }
}

block_sdev

uniform_assignments <- t(replicate(50, rep(1:4, 250)[sample.int(1000,1000)]))
# apply(uniform_assignments, 1, function(x) table(x))
array_same_communities_uniform <- array(NA_integer_, dim = c(number_samples, size_network, size_network))
for(i in 1:number_samples) {
  for(j in 1:size_network) {
    for(h in 1:size_network) {
      array_same_communities_uniform[i,j,h] <- uniform_assignments[i,j] == uniform_assignments[i,h]
    }
  }
}
probability_same_communities_uniform <- apply(array_same_communities_uniform, c(2,3), mean)
block_averages_uniform <- matrix(NA, 4,4)
block_sdev_uniform <- matrix(NA, 4,4)
for(i in 1:4) {
  for(j in 1:i) {
    if(i == j) {
      block_averages_uniform[i, j] <- mean_diagonal_block(probability_same_communities_uniform[250*(i-1)+1:250, 250*(i-1)+1:250])
      block_sdev_uniform[i, j] <- sd_diagonal_block(probability_same_communities_uniform[250*(i-1)+1:250, 250*(i-1)+1:250])
    } 
    else {
      block_averages_uniform[i, j] <- mean_nondiagonal_block(probability_same_communities_uniform[250*(i-1)+1:250, 250*(j-1)+1:250])
      block_sdev_uniform[i, j] <- sd_nondiagonal_block(probability_same_communities_uniform[250*(i-1)+1:250, 250*(j-1)+1:250])
    } 
  }
}

block_averages
block_averages_uniform
block_sdev
block_sdev_uniform
