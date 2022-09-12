################################
## Build the PGS
##
## By: David A Hughes
## Date: Aug 23rd 2022
################################
## Verify imputation INFO scores are good
## did this by eye as there are so few and it is so fast
tail -n +11 chr04_snpstats.txt | column -t | less -S
tail -n +11 chr11_snpstats.txt | column -t | less -S
tail -n +11 chr20_snpstats.txt | column -t | less -S

## INFO scores ARE ALL GOOD !!!

############################# **************** #############################
## Load Parameter Data
source("parameters/pfile.txt")


## Read in the SNP weights file
f = paste0(datadir, snp_effects_file)
snpdata = read.table(f, header = TRUE, sep = "\t")

## Read in the dosage file
library(data.table)
f = paste0(datadir, dosage_file)
dosage = fread(f)

## match the dosage data to the snpdata
m = match(dosage$SNPID, snpdata$chrpos_hg19)

## Verify that the effect allele is always allele 2 or B
sum( dosage$alleleB == snpdata$ef_allele[m] ) / length(m)

## build the pgs
pgs = sapply(7:ncol(dosage), function(i){
	sum( dosage[,..i] * snpdata$beta[m] )
	})

## reformat into a data.frame
pgs = data.frame(id = colnames(dosage)[7:ncol(dosage)], pgs = pgs)

## write to file
f = paste0(datadir, "PGS.txt")
write.table(pgs, file = f, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)





