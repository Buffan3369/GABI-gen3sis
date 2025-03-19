################################################################################
# Name: 7-Macroevol_rates.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Extract speciation and rates from simulations
################################################################################

## Assess rate of speciation and extinction within a time bin ------------------
rate_per_bin <- function(ps_bin, bin_size, what = c("sp", "ext"), na.rm = T){ #bin_size has to be in number of iterations
  if(what == "sp"){
    s <- ps_bin$speciations
    if(length(unique(s)) == 1 && is.na(unique(s))){
      return(rep(NA, bin_size))
    }
    else{
      n_spec <- sum(s, na.rm = T)
      return(rep(n_spec/bin_size, bin_size))
    }
  }
  if(what == "ext"){
    e <- ps_bin$extinctions
    if(length(unique(e)) == 1 && is.na(unique(e))){
      return(rep(NA, bin_size))
    }
    else{
      n_ext <- sum(e, na.rm = T)
      return(rep(n_ext/bin_size, bin_size))
    }
  }
}

## Define bin parameters -------------------------------------------------------
bin_size <- 100 # in nb of iterations => 1Myr here, as one step equals 10ky
maxT <- 500
n_bins <- as.integer(maxT/bin_size)

for(loc in c("North", "South")){
  SP <- c()
  EXT <- c()
  Time <- c()
  Bin <- c()
  Run <- c()
  for(run in 1:500){
    file <- paste0("./Results/M1_eq_area/", loc, "_America_start/recap_tbls/sgen3sis_run_", run, ".rds")
    if(file.exists(file)){
      # Get phylo_summary table
      s3 <- readRDS(file)
      ps <- data.frame(s3$summary$phylo_summary)
      ps$time <- c(as.numeric(rownames(ps)[2])+1, as.numeric(rownames(ps)[2:nrow(ps)]))
      row.names(ps) <- 1:nrow(ps)
      # Extend phylo_summary ds in case simulation aborpted before reaching the present
      if(nrow(ps) < maxT){
        ps[nrow(ps):maxT, ] <- NA
        ps$time <- seq(maxT, 1, -1)
      }
      # Assess Speciation and Extinction through time
      sp <- c()
      ext <- c()
      bin <- c()
      for(i in 0:(n_bins-1)){
        idx <- seq(from = i*bin_size+1, to = (i+1)*bin_size, by = 1)
        tmp_ps <- ps[idx, ]
        sp <- c(sp, rate_per_bin(tmp_ps, bin_size, what = "sp"))
        ext <- c(ext, rate_per_bin(tmp_ps, bin_size, what = "ext"))
        bin <- c(bin, rep((i+1), bin_size))
      }
      SP <- c(SP, sp)
      EXT <- c(EXT, ext)
      Time <- c(Time, seq(maxT, 1, -1))
      Bin <- c(Bin, bin)
      Run <- c(Run, rep(run, 500))
    }
  }
  div_rates <- data.frame(sp_rate = SP,
                          ex_rate = EXT,
                          Time = Time,
                          Bin = Bin,
                          Run = Run)
  saveRDS(div_rates, paste0("./Results/M1_eq_area/", loc, "_America_start/", loc, 
          "_America_div_rates_bin_", bin_size, ".RDS"))
}
