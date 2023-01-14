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


real_overlap <- 120L

size_set_relations <- 200L
label_relations <- LabelRelations(size_set_relations)
model <- Model("PitmannYor", "uniform")
parametric_space <- ParametricSpace(model, strength_is_global = TRUE, discount_is_global = TRUE)
community_structure <- CommunityStructure(label_relations, number_communities_is_fixed = TRUE)

real_assignment <- rep(1:4, size_set_relations/4)[sample.int(size_set_relations,size_set_relations)]
size_network <- length(real_assignment)
initial_values_parameter <- list(strength = .8, discount = 1.0, overlap = 105L)
set_relations <- sample_HBM(real_assignment, initial_values_parameter$strength, initial_values_parameter$discount,
                            real_overlap, function() 2L + rpois(1, 3))

network <- Network(label_relations, set_relations)

community_assignment <- CommunityAssignment(label_relations, community_structure, real_assignment)
multinetwork <- Multinetwork(community_assignment, network)
parameter <- Parameter(community_assignment, parametric_space, initial_values_parameter)

length_chain_MCMC <- 5000L

parameter_update <- ParameterUpdate(parameter)
chain_overlap <- ChainParameter(length_chain_MCMC)

for(i in 1:length_chain_MCMC) {
  if(i %% 1000L == 0L) print(sprintf("iteration %d/%d", i,length_chain_MCMC))
  current_overlap <- metropolis_hastings_overlap(parameter_update, parameter, multinetwork, network)
  set_overlap(parameter, current_overlap)
  store_parameter(chain_overlap, current_overlap)
}

plot(chain_overlap@state$chain)
burn_in <- 100
prob <- table(chain_overlap@state$chain[-(1:burn_in)])/(length_chain_MCMC - burn_in)
plot(0,0,type="n", xlim=range(as.numeric(names(prob))), ylim=c(0, max(prob)))
for(i in names(prob)) points(as.numeric(c(i,i)), c(0, prob[i]), type="l", lwd = 5)
abline(v = real_overlap, col = 2)
