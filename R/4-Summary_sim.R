################################################################################
# Goal: Add to the param table additional features related to simulations after 
#   being run.
################################################################################

write.tbl.std <- function(...){
  write.table(sep = "\t",
              na = "",
              row.names = FALSE,
              quote = FALSE,
              ...)
}

## Parameter table -------------------------------------------------------------
params <- read.table("../Data/param_tables/North_America_parameters.txt")

## Initialise new parameters ---------------------------------------------------
params[, c("finished", "final_timestep", "initial_nb_species",
           "final_nb_species", "sp_events", "ex_events")] <- 0
n_gen <- 5000 # theoretical number of time steps
n_sim <- 100  # number of simulations that were run

## Loop across all simulations -------------------------------------------------
for(i in 1:n_sim){
  # Simulation recap file
  simdir <- paste0("../Outputs/M0/North_America_start/Config_", i, 
                   "/config_M0_North_America_run_", i, "/sgen3sis.rds")
  if(file.exists(simdir)){
    sim <- readRDS(simdir)
    phylo_sum <- as.data.frame(sim$summary[[1]])
    # Precise whether the simulation finished and the final timestep reached
    if(nrow(phylo_sum) == n_gen+1){
      params$finished[i] <- 1
    }
    params$final_timestep[i] <- (n_gen+1) - nrow(phylo_sum)
    # Precise the final diversity, the total number of speciation and extinction events
    params$initial_nb_species[i] <- phylo_sum$alive[1]
    params$final_nb_species[i] <- phylo_sum$alive[nrow(phylo_sum)]
    params$sp_events[i] <- sum(phylo_sum$speciations)
    params$ex_events[i] <- sum(phylo_sum$extinctions)
  }
}

## Save ------------------------------------------------------------------------
write.tbl.std(params, "../Data/param_tables/North_America_parameters_EXTENDED.txt")