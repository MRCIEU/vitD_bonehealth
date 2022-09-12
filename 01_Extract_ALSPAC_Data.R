################################
## VitD and bone health study
## ----------------------
## Extracting ALSPAC DATA
##
## By: David A Hughes
## Date: July 15th 2022
################################
## set data directory for alspac R package
dd = "/Volumes/Data/"
library(alspac)
alspac::setDataDir(dd)
###
library(alspac)
###
library(haven)
library(tidyverse)

########################################
## Searching ALSPAC
## for desired variables
########################################
## I want to search the current directory
## so load the current data set
data(current)

## Look for the vitD variables I want from FOM clinics
vars <- findVars("vitamin d", "age", "DXA", "hip", "femoral", "bone", "osteo", "density", "fracture", "broken", "smoke",  logic="any", whole.word=FALSE, ignore.case=TRUE)
w = grep("FOM", vars$lab)
vars = vars[w, ]
## remove the pQCT data
w = grep("pQCT", vars$lab)
vars = vars[-w, ]

## Look Osteoperosis and Ever had a Fracture Data
	## Questionnaire Y
w = grep("Y_1a.dta", current$obj)
vars2 = current[w,]
vars2 = vars2[c(1,3,4,66,67,261:263),]

## Look for Ever Been a smoker data
vars3 <- findVars("ever", "smoke",  logic="all", whole.word=FALSE, ignore.case=TRUE)
## remove kids and fathers
w = c( grep("Child", vars3$cat3), grep("Partner", vars3$cat3), grep("Father", vars3$cat3) )
vars3 = vars3[-w, ]
w = which( rownames(vars3) %in% c(45, 54, 60, 63) )
vars3 = vars3[w,]

## wrist fracture
vars4 <- findVars("fracture", "wrist",   logic="all", whole.word=FALSE, ignore.case=TRUE)
## Limit to Mother
w = grep("Mother", vars4$cat3)
vars4 = vars4[w, ]

## hip fracture
vars5 <- findVars("fracture", "hip",   logic="all", whole.word=FALSE, ignore.case=TRUE)
## Limit to Mother
w = grep("Mother", vars5$cat3)
vars5 = vars5[w, ]

## Mother's age
vars6 <- findVars("Age at attendance", "respondent's date of birth",  logic="any", whole.word=FALSE, ignore.case=TRUE)
## Limit to Mother
w = grep("Partner", vars6$cat3)
vars6 = vars6[-w, ]

## Combine all extracted variables
myvars = rbind(vars6, vars3, vars, vars2, vars4, vars5)

## Extract Results
results <- extractVars(myvars)

####################################
## Write to file
####################################
f = "working/data/myvars_v2.txt"
write.table(myvars, file = f, row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)
###
f = "working/data/mydata_v2.txt"
write.table(results, file = f, row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)


##
w = grep("Y_1a.dta", current$obj)
v = current[w,]


####################################
## Extract date of attendance data
####################################
vars <- findVars("month of attendance", "year of attendance",   logic="any", whole.word=FALSE, ignore.case=TRUE)

## remove FOF
w = grep("FOF", vars$lab)
vars = vars[-w,]

## Extract Results
date_results <- extractVars(vars)


####################################
## Write to file
####################################
f = "working/data/mydata_date_attendance.txt"
write.table(date_results, file = f, row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)


####################################
## Extract BMI at FOM clinic
####################################
vars <- findVars("BMI", "Height", "Weight" ,  logic="any", whole.word=FALSE, ignore.case=TRUE)

## remove FOF
w = grep("FOM", vars$lab)
vars = vars[w,]

## Extract Results
date_results <- extractVars(vars)


####################################
## Write to file
####################################
f = "working/data/mydata_bmi.txt"
write.table(date_results, file = f, row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)


