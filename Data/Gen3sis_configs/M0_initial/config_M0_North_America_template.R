######################################
###            METADATA            ###
######################################
# gen3sis configuration
#
# Version: 1.5.11
#
# Author: Lucas Buffan
#
# Date: 2025
#
# Landscape: ./Data/Landscape/Fully_connected/
#
# Description: config to simulate M0 biodiversity experiment starting from North America.
#
######################################

######################################
###         General settings       ###
######################################

# Random seed for simulation's reproducibility
random_seed <- params$seed
# Max total nb. of species
max_number_of_species <- params$max_species
# Max nb. of species per cell
max_number_of_coexisting_species <- params$max_coex_sp
# Initial abundance
initial_abundance <- params$init_ab

# a list of traits to include with each species
trait_names <- c("temp_optimum", "prec_optimum")

# ranges to scale the input environments with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# listed with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
environmental_ranges <- list(temp = NA, prec = NA)


#########################
### Observer Function ###
#########################

end_of_timestep_observer <- function(data, vars, config){
  #save richness plots, matrix and phylogeny
  plot_richness(data$all_species, data$landscape)
  save_species()
  save_phylogeny()
}


######################################
###         Initialisation         ###
######################################

create_ancestor_species <- function(landscape, config) {
  # Coordinates of North American grid cells included in our landscape
  North_America_coordinates <- landscape$coordinates[which(landscape$coordinates[,1] < -54 & 
                                                             landscape$coordinates[,2] > 12),] 
  # Initial environment
  North_America_environment <- landscape$environment[which(landscape$coordinates[,1] < -54 & 
                                                             landscape$coordinates[,2] > 12), 1]
  # Rasterise
  North_America_raster <- raster::rasterFromXYZ(cbind(North_America_coordinates, North_America_environment))
  # Extend initial raster to reach the extent of the landscape
  North_America_raster <- extend(North_America_raster, landscape$extent)
  
  #Set up ancestral species
  North_America_start_cells  <- sample(Which(North_America_raster, cells=T), 1)
  North_America_start_cells  <- as.character(North_America_start_cells)

  # now fill the list of new species
  North_America_species <- create_species(North_America_start_cells, config)
  North_America_species$traits[ , "temp_optimum"] <- median(landscape$environment[North_America_start_cells, "temp"], na.rm=T)
  North_America_species$traits[ , "prec_optimum"] <- median(landscape$environment[North_America_start_cells, "prec"], na.rm=T)
  
  all_species <- list(North_America_species)
  
  return(all_species)
}


#################
### Dispersal ###
#################

# Raster resolution in km (at the equator)
grid_cell_distance <- params$grid_cell_distance
# Shape and scale of the Weibull distribution
dispersal_scale <-  params$disp_scale
dispersal_shape <-  params$disp_shape
# Dispersal => sample in a weibull for each species
get_dispersal_values <- function(n, species, landscape, config) {
  values <- rweibull(n = length(species$traits[, 1]),
                     shape = dispersal_shape, 
                     scale = c(grid_cell_distance * dispersal_scale))
  values[which(values < grid_cell_distance)] <- grid_cell_distance
  return(values)
}


##################
### Divergence ###
##################

# threshold for genetic distance after which a speciation event takes place
divergence_threshold <- params$S

# adds a value of 1 to each geographic population cluster
get_divergence_factor <- function(species, cluster_indices, landscape, config) {
  return(0.1)
}


#######################
### Trait Evolution ###
#######################

sigma_e <- params$sigma_e

apply_evolution <- function(species, cluster_indices, landscape, config) {
  # cell names
  traits <- species[["traits"]]
  cells <- rownames( traits )
  
  # evolve traits for each cluster
  for(cluster_index in unique(cluster_indices)){
    
    cells_cluster <- cells[which(cluster_indices == cluster_index)]
    
    # median local temp and prec values for the cluster
    t_theta_cluster <- median(landscape$environment[cells_cluster, "temp"], na.rm=T)
    p_theta_cluster <- median(landscape$environment[cells_cluster, "prec"], na.rm=T)
    
    # evolve temperature and precipitation niche optima
    delta_temp_cluster <- abs(rnorm(1, mean = 0, sd = sigma_e))
    delta_prec_cluster <- abs(rnorm(1, mean = 0, sd = sigma_e))
    
    # get the current trait values assigned to the cluster
    t_opt_cluster <- traits[cells_cluster, "temp_optimum"][1]
    p_opt_cluster <- traits[cells_cluster, "prec_optimum"][1]
    
    # find the difference between environment and trait value
    # if env is cooler or drier than niche it will be negative
    # if env is warmer or wetter than niche it will be positive
    diff_t <- t_theta_cluster - t_opt_cluster
    diff_p <- p_theta_cluster - p_opt_cluster
    
    # new value will be in the direction of theta, i.e., local environment
    new_value_t <- t_opt_cluster + delta_temp_cluster * sign(diff_t)
    new_value_p <- p_opt_cluster + delta_prec_cluster * sign(diff_p)
    
    # however if the difference is small relative to the rate (sigma_e), the evolutionary change can overshoot the optima
    # if the new niche is more poorly adapted than the original then we need to change it (under the assumption that the niche is under positive selection)
    new_diff_t <-  t_theta_cluster - new_value_t 
    new_diff_p <-  p_theta_cluster - new_value_p 
    
    # so if the new difference is > than the original difference, draw the value back to match the original difference (this time on the other side of the optima)
    if(abs(new_diff_t) > abs(diff_t)){new_value_t = t_theta_cluster - (sign(new_diff_t) * abs(diff_t))}
    if(abs(new_diff_p) > abs(diff_p)){new_value_p = p_theta_cluster - (sign(new_diff_p) * abs(diff_p))}
    
    # add new values back to traits matrix
    traits[cells_cluster, "temp_optimum"]  <- new_value_t
    traits[cells_cluster, "prec_optimum"]  <- new_value_p
    
    
  }
  
  # set bounds between 0 and 1 so the species can't evolve a niche beyond that present in the data (all env variables are scaled between 0 and 1)
  if(any(traits[, "temp_optimum"] > 1)){traits[which(traits[,"temp_optimum"] > 1), "temp_optimum"] <- 1}
  if(any(traits[, "temp_optimum"] < 0)){traits[which(traits[,"temp_optimum"] < 0), "temp_optimum"] <- 0}
  
  if(any(traits[, "prec_optimum"] > 1)){traits[which(traits[,"prec_optimum"] > 1), "prec_optimum"] <- 1}
  if(any(traits[, "prec_optimum"] < 0)){traits[which(traits[,"prec_optimum"] < 0), "prec_optimum"] <- 0}
  
  
  return(traits)
  
}


##############################################
### Environmental and Ecological Filtering ###
##############################################

temp_omega <- params$omega_T # environmental filtering parameter [0, n]
prec_omega <- params$omega_P # environmental filtering parameter [0, n]

inflexion <- params$inflexion # extinction parameter: inflexion point of extinction probability curve
decay <- params$decay  # extinction parameter: decay rate of extinction probability curve
K_max <- 10 # Carrying capacity of each site

apply_ecology <- function(abundance, traits, landscape, config) {
  
  # define the traits
  temp_opt <- traits[, "temp_optimum"]
  prec_opt <- traits[, "prec_optimum"]
  temp_site <- landscape[, "temp"]
  prec_site <- landscape[, "prec"]
  
  # define K with environment
  K_site <- K_max 
  # determine abundance
  abundance <- exp(-((temp_opt-temp_site)^2/(2*(temp_omega^2)))) * exp(-((prec_opt-prec_site)^2/(2*(prec_omega^2))))
  # extirpation process (whether species goes extinct in the site)
    # compute prob extirpation across all species inhabiting the site
  prob_extirpation <- 1 / (1 + exp(-decay*(inflexion - abundance)))
    # given a species, set within-site abundance to 0 with a probability equal to local prob extirpation
  locally_extinct <- sapply(prob_extirpation, 
                            FUN=function(x){sample(x = c(0,1),
                                                   size = 1, 
                                                   prob = c(x, 1-x))})
  extinct_idx <- which(locally_extinct==0)
  abundance[extinct_idx] <- 0
  abundance_sum <- sum(abundance)
  
  # if the total abundance in a grid cell is above K_site, then sequentially reduce the species' abundances until it isn't anymore
  while_i <- 0.01
  while_sd  <- 0.2
  
  while(abundance_sum > K_site){
    
    #number of species to reduce (can go slow when doing one at time if lots of species in the cell)
    # select 10% of species
    n_species_to_reduce <- pmax(1, round(0.1*length(which(abundance>0))))
    
    reduce_prob <- rep(1, length(abundance))
    reduce_prob[which(abundance==0)] <- 0
    
    # choose a species inversely proportional to how well adapated they are
    species_to_reduce <- sample(1:length(abundance), n_species_to_reduce, prob=reduce_prob)
    
    # reduce abundance by a value drawn from a normal distribution with mean 0
    abundance[species_to_reduce] <- abundance[species_to_reduce] - abs(rnorm(1, 0, sd=while_sd+while_i))
    
    # recalculate extirpation probabilities
    prob_extirpation <- (1/(1+exp(-decay*(inflexion - abundance))))
    abundance[which(sapply(prob_extirpation, FUN=function(x){sample(c(0,1),1, prob= c(x, 1-(x)))})==0)] <- 0
    
    # condition to stop the while loop and reduce all species
    if(while_i >= 1){
      #print("breaking while loop")
      abundance[order(abundance, decreasing=T)[which(cumsum(abundance[order(abundance, decreasing=T)]) > K_site)]] <- 0
    }
    # sum again abundances
    abundance_sum <- sum(abundance, na.rm=T)
    while_i <- while_i + 0.01
  }
  
  return(abundance)
  
  
  
  
}

