#!/bin/bash


for mdl in M0 M1 M2 M3
do
	scp -r bm10:~/GABI-Gen3sis/Data/param_tables/$mdl/*_biome.txt ./$mdl/
done
