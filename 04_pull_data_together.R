################################
## Pull phenotype and genotype data 
## together and make exclusions
##
## By: David A Hughes
## Date: Aug 23rd
################################

## Load Parameter Data
source("parameters/pfile.txt")

## Load the variable data
f = paste0(datadir, variable_file)
vardata = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)

## Load the phenotype data
f = paste0(datadir, data_file)
mydata = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)

## Load the data attended FOM clinic data
f = paste0(datadir, date_attend_file)
date_attend = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)

## add date_attend to mydata
m = match(mydata$alnqlet, date_attend$alnqlet )
mydata = cbind(mydata, date_attend[m, 4:15 ])

## Load the BMI data
f = paste0(datadir, bmi_file)
bmidata = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)

## add BMI data to mydata
m = match(mydata$alnqlet, bmidata$alnqlet )
mydata = cbind(mydata, bmidata[m, c(4:7, 9:12, 14:17, 19:22) ])


###########################
## REMOVE ALL TWIN B SAMPLES
###########################
w = which(mydata$qlet != "A")
mydata = mydata[-w, ]

##########################
## EXTRACT DATA
##########################
## an empty vector of variable to extract
vars2_extract = c("aln", "qlet", "alnqlet" )

## (1) FOM clinic: month and year attended
v = c("fm1a010a", "fm1a010b", 
	"fm2a010a", "fm2a010b", 
	"fm3a010a", "fm3a010b",
	"fm4a010a", "fm4a010b" )
vars2_extract = c(vars2_extract, v)

## (2) FOM clinic: Age at attendance variable names
v = c("fm1a011", "fm2a011", "fm3a011", "fm4a011")
vars2_extract = c(vars2_extract, v)

## (3) FOM clinic: Vitamin D-T (ng/ml)
v = c("VitDt_FOM1", "VitDt_FOM2", "VitDt_FOM3", "VitDt_FOM4")
vars2_extract = c(vars2_extract, v)

## (4) FOM clinic: Height
v = c("fm1ms100", "fm2ms100", "fm3ms100", "fm4ms100")
vars2_extract = c(vars2_extract, v)

## (5) FOM clinic: Weight
v = c("fm1ms110", "fm2ms110", "fm3ms110", "fm4ms110")
vars2_extract = c(vars2_extract, v)

## (6) FOM clinic: BMI
v = c("fm1ms111", "fm2ms112", "fm3ms112", "fm4ms112")
vars2_extract = c(vars2_extract, v)

## (7) Ever Smoked
## Questionairs B (1990-1992), R (2002-2004), T (2010),  V (2012-2014)
v = c("b650","r6010","t5560", "V5560")
vars2_extract = c(vars2_extract, v)

## (8) Ever had a fracture
## 		Questionaire (T in year 2010); "E40: Respondent has fractured wrist"
## 		Questionaire (T in year 2010); "E39: Respondent has fractured hip"
## 		Questionaire (V in year 2012-2014); "C19a: Respondent has ever fractured their wrist"
## 		Questionaire (V in year 2012-2014); "C18a: Respondent has ever fractured their hip"
## 		Questionaire (Y in year 2020); "B6: Ever had a fracture (broken a bone): Y"
v = c( "t5210", "t5200", 
	"V4210" , "V4200",
	"Y2140")
vars2_extract = c(vars2_extract, v)

## (9) Ever told you had osteoperosis
## Questionaire (Y in year 2020); "B7: Ever been told by doctor had osteoporosis: Y"
v = c("Y2150")
vars2_extract = c(vars2_extract, v)

## FIND the HIP BMD variables
w = grep("BMD", vardata$lab)
hipbmd_ids = vardata$name[w]
## keep only dxa measure, i.e. remove consent vars
w = grep("hdx", hipbmd_ids)
hipbmd_ids = hipbmd_ids[w]

vars2_extract = c(vars2_extract, hipbmd_ids)

## Make new column names for Hip BMD values
m = match(hipbmd_ids, vardata$name )
tempid = vardata$lab[m]
tempid = sapply(tempid, function(x){
	a = strsplit(x, split = " BMD")[[1]][1]
	a = gsub(" ","_",a)
	b = strsplit(x, split = ": ")[[1]][2]
	out = paste0(b, "_", a, "_BMD" )
	return(out)
	} )

Hip_BMD_colnames = as.character(tempid)


#########################
### DEFINE NEW DATA SET
#########################
new_data = mydata[, vars2_extract]


#########################
## DEFINE NEW COLUMN NAMES
#########################
new_col_names = c("aln","qlet","alnqlet",
	"month_attended_FOM1","year_attended_FOM1",
	"month_attended_FOM2","year_attended_FOM2",
	"month_attended_FOM3","year_attended_FOM3",
	"month_attended_FOM4","year_attended_FOM4",
	"age_at_FOM1", "age_at_FOM2", "age_at_FOM3", "age_at_FOM4", 
	"VitDt_FOM1", "VitDt_FOM2", "VitDt_FOM3", "VitDt_FOM4",
	"height_at_FOM1", "height_at_FOM2", "height_at_FOM3", "height_at_FOM4",
	"weight_at_FOM1", "weight_at_FOM2", "weight_at_FOM3", "weight_at_FOM4",
	"bmi_at_FOM1", "bmi_at_FOM2", "bmi_at_FOM3", "bmi_at_FOM4",
	"ever_smoked_B_1992", "ever_smoked_R_2004", "ever_smoked_T_2010", "ever_smoked_V_2014",
	"ever_fracture_wrist_2010","ever_fracture_hip_2010", 
	"ever_fracture_wrist_2014","ever_fracture_hip_2014",
	"ever_fracture_2020",
	"ever_told_osteoperosis_2020",
	Hip_BMD_colnames )


new_data_m = new_data
colnames(new_data_m) = new_col_names


#########################
## Add Genotype Data
#########################

## Load the PGS data
f = paste0(datadir, pgs_file)
pgsdata = read.table(f, header = TRUE, sep = "\t", as.is = TRUE)
## remove "M" from aln
pgsdata$id = gsub("M","",pgsdata$id)

## match to new_data
m = match(new_data$aln, pgsdata$id)
new_data$pgs = pgsdata$pgs[m]
new_data_m$pgs = pgsdata$pgs[m]


## Load the PCA data
f = paste0(datadir, pcs_file)
PCdata = read.table(f, header = FALSE, sep = " ", as.is = TRUE)
colnames(PCdata) = c("aln","aln2",paste0("PC", 1:20))
## remove "M" from aln
PCdata$aln = gsub("M","",PCdata$aln)

## match to new_data
m = match(new_data$aln, PCdata$aln)
new_data = cbind( new_data, PCdata[m, 3:22] )
new_data_m = cbind( new_data_m, PCdata[m, 3:22] )


##################################
##
## Write to file
##
##################################
f = paste0(datadir, "working_data/working_data_set_v0.txt")
write.table(new_data, file = f, 
	row.names = FALSE, 
	col.names = TRUE, sep = "\t", quote = FALSE)

f = paste0(datadir, "working_data/working_data_set_v0_new_colnames.txt")
write.table(new_data_m, file = f, 
	row.names = FALSE, 
	col.names = TRUE, sep = "\t", quote = FALSE)


