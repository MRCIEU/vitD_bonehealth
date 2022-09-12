#!/bin/bash

#SBATCH --job-name=vitDextraction
#SBATCH --nodes=1 
#SBATCH --cpus-per-task=1
#SBATCH --time=10:00:00 
#SBATCH --mem=10G


## ADD QCTOOLs to environment
module add apps/qctool/2.2.0

## DEFINE COMMON VARIABLES
source parameters/pfile.sh

## LOOP OVER CHROMSOMES TO PERFORM EXTRACTION
for i in 04 11 20; do 
	BGEN=${DATADIR}data_${i}.bgen
	SAM=${DATADIR}data.sample
	OUT=${RESULTSDIR}data_${i}.dosage
	qctool -g ${BGEN} -s ${SAM} -og ${OUT} -ofiletype dosage -incl-rsids ${SNPS}
done


