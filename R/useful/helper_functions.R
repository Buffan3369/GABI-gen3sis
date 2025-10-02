################################################################################
# Name: helper_functions.R
# Authors: Lucas Buffan
# Emails: lucas.l.buffan@gmail.com 
# Goal: Assistant function throughout our pipeline
################################################################################

## Standardised table writing --------------------------------------------------
write.tbl.std <- function(...){
  write.table(sep = "\t",
              na = "",
              row.names = FALSE,
              quote = FALSE,
              ...)
}

## Standardised theme for plots ------------------------------------------------
theme_lucas <- function(...){
  theme(panel.background = element_blank(),
        panel.border = element_rect(linewidth = .75, colour = "black", fill = NA),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 12, hjust = 0.5),
        ...)
}

## Assess whether a binary vector contains ones --------------------------------
ones <- function(vect){
  u_vect <- unique(vect)
  if(length(u_vect) == 2){return(TRUE)} # c(0, 1)
  else{return(FALSE)}
}

## Function mapping Sobol' sequences to a desired parameter range linearly -----
#   (adapted from Hagen et al. (2021), Skeels et al. (2023a,b), ...)
linMap <- function(x, from, to, rnd = F, dgts) {
  rescaled <- (x - min(x)) / max(x - min(x)) * (to - from) + from
  if(rnd){
    rescaled <- round(rescaled, digits = dgts)
  }
  return(rescaled)
}

## Rename broadly-classified biomes --------------------------------------------
biome_rename <- function(biome){
  if(biome == "1"){
    return("Tropical")
  }
  else if(biome == "2"){
    return("Arid")
  }
  else if(biome == "3"){
    return("Temperate")
  }
  else if(biome == "4"){
    return("Cold")
  }
  else if(biome == "5"){
    return("Polar")
  }
}

## Statistical facilities ------------------------------------------------------
  # Quantiles of an N(0,1)
normal_quantiles <- function(q){
  X <- rnorm(n = 1e6)
  qtl <- quantile(x = X, probs = q)
  return(round(as.numeric(qtl), digits = 2))
}
  # Binomial confidence interval
bino_CI <- function(prop,      # Estimated proportion 
                    n,         # Number of trials (500 here)
                    alpha,     # Error associated to the confidence level (0.05 for a 95% Conf lvl)
                    what = c("Lower", "Upper")){
  if(what == "Lower"){
    q_alpha <- normal_quantiles(alpha/2)
  }
  else if(what == "Upper"){
    q_alpha <- normal_quantiles(1-alpha/2)
  }
  value <- prop + q_alpha * sqrt(prop * (1-prop)) / sqrt(n)
  return(value)
}
  # Confidence interval associated to a quantity
Student_CI <- function(x_bar,
                       n,
                       sigma,      # Estimated standard deviation
                       alpha,
                       what = c("Lower", "Upper")){
  if(what == "Lower"){
    q_alpha <- normal_quantiles(alpha/2)
  }
  else if(what == "Upper"){
    q_alpha <- normal_quantiles(1-alpha/2)
  }
  value <- x_bar + q_alpha * sigma / sqrt(n)
  return(value)
}

