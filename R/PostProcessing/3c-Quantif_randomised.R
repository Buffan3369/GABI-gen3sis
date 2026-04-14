library(tidyverse)

mdl <- "M0"

NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Oscillayers/", mdl, 
                                  "/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                           header = T)
# Filter out simulations that crashed
NA_recap_tbl <- NA_recap_tbl %>% filter(!(exchanged == -1))
# Function to compute success probability on a subsample of a given table df with n rows
suc_prop_rand <- function(df, sample_size){
  subspl <- df[sample(1:nrow(df), size = sample_size, replace = F), ]
  n_success <- length(which(subspl$exchanged == 1)) / sample_size
}
# Randomise
sample_size <- 100
n_iter <- 100

success_prop <- c()
for(i in 1:n_iter){
  success_prop <- c(success_prop, 
                    suc_prop_rand(df = NA_recap_tbl, 
                                  sample_size = sample_size))
}
