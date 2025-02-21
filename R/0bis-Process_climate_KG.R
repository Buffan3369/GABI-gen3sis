library(tidyverse)
library(raster)

temp_array <- array(NA, dim = c(149, 360, 12))
prec_array <- array(NA, dim = c(149, 360, 12))

for(i in 1:12){
  month <- i
  if(month < 10){
    month <- paste0("0", month)
  }
  # Temperature
  tmp <- read.table(paste0("./Data/PALEO_PGEM-bioclim/20379663/monthly_mean/monthly_temperature", month, "M.txt"),
                    header = T)
  tmp_5k <- tmp[,1:3]
  colnames(tmp_5k) <- c("Long", "Lat", "Temp")
  tmp_5k_wide <- tmp_5k %>% pivot_wider(names_from = Long, values_from = Temp)
  temp_array[,,i] <- as.matrix(tmp_5k_wide[,-c(1)])
  rm(tmp, tmp_5k, tmp_5k_wide)
  # Precipitation
  prec <- read.table(paste0("./Data/PALEO_PGEM-bioclim/20379663/monthly_mean/monthly_precipitation", month, "M.txt"),
                    header = T)
  prec_5k <- prec[,1:3]
  colnames(prec_5k) <- c("Long", "Lat", "Prec")
  prec_5k_wide <- prec_5k %>% pivot_wider(names_from = Long, values_from = Prec)
  prec_array[,,i] <- as.matrix(prec_5k_wide[,-c(1)])
  rm(prec, prec_5k, prec_5k_wide)
}


## Reclassify
source("~/Documents/KoppenGeiger_inR/kgreclass_Rfunction.R")
biomes_5k <- kg_reclass(Temp = temp_array,
                        Prec = prec_array,
                        type = "class")
  
  

precJan <- read.table("./Data/PALEO_PGEM-bioclim/20379663/monthly_mean/monthly_precipitation01M.txt", header = T)
prec_5000 <- precJan[,1:3]

prec_5000_wider <- prec_5000 %>% pivot_wider(names_from = Long, values_from = T5000)

prec_5000_longer <- prec_5000_wider %>%
  pivot_longer(cols = 2:ncol(prec_5000_wider))
prec_5000_longer <- prec_5000_longer %>% 
  filter(!is.na(value))



