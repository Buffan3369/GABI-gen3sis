#!/bin/bash

for i in {4..12}
do
    Rscript ./0-Process_climate_monthly.R $i
done