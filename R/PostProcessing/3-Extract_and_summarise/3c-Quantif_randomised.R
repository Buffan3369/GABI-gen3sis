library(tidyverse)

# Function to compute success probability on a subsample of a given table df with n rows
suc_prop_rand <- function(df, sample_size){
  subspl <- df[sample(1:nrow(df), size = sample_size, replace = F), ]
  n_success <- length(which(subspl$exchanged == 1)) / sample_size
}

mdl <- "M1"

NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Oscillayers/", mdl, 
                                  "/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                           header = T)
SA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Oscillayers/", mdl, 
                                  "/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                           header = T)
# Filter out simulations that crashed
NA_recap_tbl <- NA_recap_tbl %>% filter(!(exchanged == -1))
SA_recap_tbl <- SA_recap_tbl %>% filter(!(exchanged == -1))

# Randomise
sample_size <- 100
n_iter <- 100

na_success_prop <- c()
sa_success_prop <- c()
for(i in 1:n_iter){
  na_success_prop <- c(na_success_prop,
                       suc_prop_rand(df = NA_recap_tbl,
                                     sample_size = sample_size))
  sa_success_prop <- c(sa_success_prop,
                       suc_prop_rand(df = SA_recap_tbl,
                                     sample_size = sample_size))
}

boxplot(na_success_prop, sa_success_prop)

