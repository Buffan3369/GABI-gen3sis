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
