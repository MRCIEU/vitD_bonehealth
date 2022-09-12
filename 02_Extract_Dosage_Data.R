################################
## Extracting Genotype Data
## from ALSPAC for VitD study
## By: David A Hughes
## Date: Aug 23rd
################################
## DATADIR (for HRC) and DATADIR1KG path
source parameters/pfile.sh

## add qctool to my environment
module add apps/qctool/2.0.7

####################################
## HRC IMPUTATION 
## The SNPs I want are on chromosome
## 4, 11, and 20
####################################
## Loop over the chromosomes we need 
## and extract SNPS
for i in 04 11 20; do echo ${i}
FILE=data_${i}.bgen
SAM=data.sample
OUT=chr${i}.dosage
qctool -g ${DATADIR}${FILE} -s ${DATADIR}${SAM} -incl-rsids snpids.txt -ofiletype dosage -og ${OUT}
done

####################################
## Concatenate
####################################
cp chr04.dosage vitd.dosage
tail -n +2 chr11.dosage >> vitd.dosage
tail -n +2 chr20.dosage >> vitd.dosage


####################################
## SNP-STATS for HRC IMPUTATION 
####################################
## Loop over the chromosomes we need 
## and estimate SNP-stats
for i in 04 11 20; do echo ${i}
FILE=data_${i}.bgen
SAM=data.sample
OUT=chr${i}_snpstats.txt
qctool -g ${DATADIR}${FILE} -s ${DATADIR}${SAM} -incl-rsids snpids.txt -snp-stats -osnp ${OUT}
done


#### ***********************************************************
#### ***********************************************************


####################################
## 1000 GENOMES IMPUTATION
## The SNPs I want are on chromosome
## 4, 11, and 20
####################################
## Redefine files of interest
for i in 04 11 20; do echo ${i}
FILE=data_chr${i}.bgen
SAM=1000genomes/released/2015-10-30/data/data.sample
OUT=Kgenomes_chr${i}.dosage
qctool -g ${DATADIR1KG}${FILE} -s ${SAM} -incl-rsids rsids.txt -ofiletype dosage -og ${OUT}
done

####################################
## Concatenate
####################################
cp Kgenomes_chr04.dosage vitd_1KG.dosage
tail -n +2 Kgenomes_chr11.dosage >> vitd_1KG.dosage
tail -n +2 Kgenomes_chr20.dosage >> vitd_1KG.dosage



