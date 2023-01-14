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
source(paste0(path_files, "Chain_Local_Parameter.R"))

real_strength <- 0.8
real_discount <- 4.5

size_set_relations <- 1000L
label_relations <- LabelRelations(size_set_relations)
model <- Model("PitmannYor", "uniform")
parametric_space <- ParametricSpace(model, strength_is_global = TRUE, discount_is_global = TRUE)
community_structure <- CommunityStructure(label_relations, number_communities_is_fixed = TRUE)

real_assignment <- rep(1:4, size_set_relations/4)[sample.int(size_set_relations,size_set_relations)]
size_network <- length(real_assignment)
initial_values_parameter <- list(strength = .2, discount = 0.1, overlap = 250L)
set_relations <- sample_HBM(real_assignment, real_strength, real_discount,
                            initial_values_parameter$overlap, function() 2L + rpois(1, 3))

network <- Network(label_relations, set_relations)

community_assignment <- CommunityAssignment(label_relations, community_structure, real_assignment)
multinetwork <- Multinetwork(community_assignment, network)
parameter <- Parameter(community_assignment, parametric_space, initial_values_parameter)

length_chain_MCMC <- 5000L

parameter_update <- ParameterUpdate(parameter)
chain_strength <- ChainParameter(length_chain_MCMC)
chain_discount <- ChainParameter(length_chain_MCMC)

for(i in 1:length_chain_MCMC) {
  if(i %% 1000L == 0L) print(sprintf("iteration %d/%d", i,length_chain_MCMC))
  current_strength <- metropolis_hastings_strength(parameter_update, parameter, multinetwork)
  set_strength(parameter, current_strength)
  store_parameter(chain_strength, current_strength)
  
  current_discount <- metropolis_hastings_discount(parameter_update, parameter, multinetwork)
  set_discount(parameter, current_discount)
  store_parameter(chain_discount, current_discount)
}

par(mfrow=c(1,2))
plot(chain_strength@state$chain)
abline(h = real_strength, col=2)
plot(chain_discount@state$chain)
abline(h = real_discount, col=2)
