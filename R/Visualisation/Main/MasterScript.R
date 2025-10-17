################################################################################
# Name: MasterScript.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Run this script to produce all the figures of the main text of 
#       this study.
################################################################################

## Baseline for framework figure (further assembled in InkScape) ---------------
source("./R/Visualisation/Main/Figure_framework.R")

## Figure for proportion of successful exchange --------------------------------
rm(list = ls())
source("./R/Visualisation/Main/Figure_PropSuccess.R")

## Metrics panel ---------------------------------------------------------------
rm(list = ls())
source("./R/Visualisation/Main/Figure_metrics.R")

## Biome panel -----------------------------------------------------------------
rm(list = ls())
source("./R/Visualisation/Main/Figure_biomes.R")