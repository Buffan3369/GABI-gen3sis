#!/bin/bash

for mdl in M0 M1 M2 M3
do
	for start_region in North South
	do
		Rscript 2-PostProcessing_MASTER.R ../Data/param_tables/${mdl} 500 500 ../Outputs/${mdl}/${start_region}_America_start ${mdl} ${start_region} ../Data/param_tables/${mdl}/${start_region}_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt
	done
done
