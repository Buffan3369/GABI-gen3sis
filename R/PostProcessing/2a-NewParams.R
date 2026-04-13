################################################################################
# Name: 5a-NewParams.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Goal: Function adding post-simulation parameters, such as the final nb of
#       species, the numbers of speciation and extinction events, 
#       timestep when the simulation finished...
################################################################################

Add_new_params <- function(param_tbl,        # Pre-simulation parameter table
                           n_gen,            # Number of generations
                           n_sim,            # Number of simulation per scenario
                           sim_dir,          # Where simulation outputs are stored
                           mdl,              # Simulation scenario (M0, M1, ..)
                           start_continent,  # Starting continent ("North" or "South") 
                           eq_dist,          # Whether we're working on simulations with standardised distance to isthmus or not
                           oscill            # Whether we're working with oscillayer-based simulations
                           ){
  ## Initialise new parameters ---------------------------------------------------
  param_tbl[, c("finished", "final_timestep", "initial_nb_species", 
             "final_nb_species", "sp_events", "ex_events")] <- 0
  
  n_gen <- as.numeric(n_gen)
  n_sim <- as.numeric(n_sim)
  
  ## Loop across all simulations -------------------------------------------------
  for(i in 1:n_sim){
    # Simulation recap file
    if(eq_dist | oscill){
      simdir <- paste0(sim_dir, "/Config_", i, "/config_", 
                       mdl, "_", start_continent, "_America_run_", i, "/sgen3sis.rds")
    }
    else{
      simdir <- paste0(sim_dir, "/config_", 
                       mdl, "_", start_continent, "_America_run_", i, "/sgen3sis.rds")
    }
    #  print(simdir)
    if(file.exists(simdir)){
      #    print("Queue")
      sim <- readRDS(simdir)
      phylo_sum <- as.data.frame(sim$summary[[1]])
      # Precise whether the simulation finished and the final timestep reached
      if(nrow(phylo_sum) == n_gen+1){
        param_tbl$finished[i] <- 1
      }
      param_tbl$final_timestep[i] <- n_gen+1 - nrow(phylo_sum)
      # Precise the final diversity, the total number of speciation and extinction events
      param_tbl$initial_nb_species[i] <- phylo_sum$alive[1]
      param_tbl$final_nb_species[i] <- phylo_sum$alive[nrow(phylo_sum)]
      param_tbl$sp_events[i] <- sum(phylo_sum$speciations)
      param_tbl$ex_events[i] <- sum(phylo_sum$extinctions)
    }
  }
  return(param_tbl)
}