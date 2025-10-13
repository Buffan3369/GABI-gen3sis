################################################################################
# Name: 3b-CountCrash.R
# Author: Lucas Buffan
# Contact: lucas.l.buffan@gmail.com
# Aim: Assess proportions of simulations leading to no living representatives
###############################################################################

counter <- data.frame(Start = c("North America", "North America Standardised", "South America"),
                      M0 = c(NA, NA, NA), 
                      M1 = c(NA, NA, NA), 
                      M2 = c(NA, NA, NA), 
                      M3 = c(NA, NA, NA))
for(start in c("North", "North_std", "South")){
  for(mdl in c("M0", "M1", "M2", "M3")){
    if(start == "North_std"){
      recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Equal_dist/", mdl, "/North_America_parameters_EqDist_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"), 
                              sep = "\t", header = TRUE)
      tk <- "North America Standardised"
    }
    else{
      recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"), 
                              sep = "\t", header = TRUE)
      tk <- paste0(start, " America")
    }
    n_finished <- length(which(recap_tbl$exchanged == -1))
    pct_finished <- round(n_finished / 500, digits = 3)
    
    counter[which(counter$Start == tk),
            mdl] <- pct_finished
  }
}

saveRDS(counter, "./Results/Exchanged_metrics/Crash_counter.RDS")
