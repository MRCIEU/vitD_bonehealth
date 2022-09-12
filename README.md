# Vitamin D, Bone Fractures, and Femoral & Heel Bone Mineral Density - a Stratified MR Study

Does vitamin D causally influence bone mineral density or the occurance of bone fractures?

## ALSPAC project number B4110.

## Variables of interest

### exposure

	1. Vitamin D (nmol/L)
	2. a GRS built from 21 SNPs from 3 loci (chr 4, 11, and 20)
	
### outcomes
	
	1. ultra sound heel bone mineral density (BMD) (NOT AVAILABLE)
	2. DXA femoral or hip BMD
	3. osteoporosis events (N)
	3. the number of bone fracture events (N)
		- excluding fingers, toes, and skull

### covariables

	1. age
	2. sex (if not using just moms)
	3. date of VitD measurement to convert to "season"
	4. BMI
	5. smoking status (Never|Ever)

	
### Exposure Strata

	1. Broad Categories
		- <25 nmol/L
		- 25-49 nmol/L
		- 50-74 nmol/L
		- >=75 nmol/L
	2. Narrow Categories
		- <20  nmol/L
		- 20-29  nmol/L
		- 30-39  nmol/L
		- 40-49  nmol/L
		- 50-59  nmol/L
		- 60-69  nmol/L
		- >=70  nmol/L

### Location of data

#### Website with links to the actual questionnaires

	Website:
	https://www.bristol.ac.uk/alspac/researchers/our-data/questionnaires/carer-questionnaires/
	
## SCRIPTS

### 01_Extract_ALSPAC_Data.R

	Code used to extract all possible variables from the ALSPAC data files.

### 02_Extract_Dosage_Data.R

	Interactive code to:
		(1) extract needed SNPs from HRC and 1000Genomes ALSPAC data
		(2) run SNPstats on those SNP
		(3) Concatenate the data together
		(4) and move data to data warehouse
		
	* 02_snp_extraction_1kg.sh
			A bash script to extract needed SNPs from 1000Genomes data and concatenate
			Another way to do steps 1 and 3 above
			
	* 02_snp_extraction_hrc.sh
		A bash script to extract needed SNPs from HRC data and concatenate
		Another way to do steps 1 and 3 above
	
### 03_Build_PGS.R

	Code to build the GRS

### 04_pull_data_together.R
	
	Code to pull all of the different data resources data together into a single working data frame.

### 05_vitd_normalization.Rmd

	Code to perform the VitD season and clinic normalization

### 06_define_data_set.Rmd

	Code to:
		(1) extract just those variables needed for the analysis 
			- wide format data
		(2) build a single large dataset using data from each of the 4 FOM clinics.
			- long format data

### 07_analysis.Rmd

	Some analysis code


