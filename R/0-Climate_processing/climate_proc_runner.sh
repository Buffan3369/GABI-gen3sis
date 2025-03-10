#!/bin/bash

for i in {1..12}
do
    echo "$i"
    Rscript ./0-Process_climate_monthly.R $i
#    Rscript ./0-Process_climate_monthly_noParallel.R $i
done
