#!/bin/bash

## This script executes the R script to run gen3sis experiments ##

Rscript Config_runner.R M0 40 500 # 40 threads, 500 replicates
Rscript Config_runner.R M1 40 500
Rscript Config_runner.R M2 40 500
Rscript Config_runner.R M3 40 500


