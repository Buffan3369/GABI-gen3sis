################################################################################
# Name: helper_functions.R
# Authors: Lucas Buffan
# Emails: lucas.l.buffan@gmail.com 
# Goal: Assistant function throughout our pipeline
################################################################################

library(tidyverse)

## Standardised table writing --------------------------------------------------
write.tbl.std <- function(...){
  write.table(sep = "\t",
              na = "",
              row.names = FALSE,
              quote = FALSE,
              ...)
}