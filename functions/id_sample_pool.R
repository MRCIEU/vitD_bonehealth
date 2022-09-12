########################################
## A Function to Identify the largest 
## sample size of unique individuals
## across the 4 FOM clinics that have
## data for both the exposure and outcome
## Function written by David A Hughes
## Date: Aug 26th 2022
########################################
id_sample_pool = function(wdata, 
                          exposure, 
                          outcome,
                          id = "aln",
                          clinic_prefix = c("FOM1","FOM2","FOM3","FOM4") ){
  
  ## Find all individuals in each clinic that have exposure and outcome data
  clinic_ids = lapply(clinic_prefix, function(clinic){
    vars = c(id, paste0(clinic, "_", c(exposure, outcome)))
    out = na.omit( wdata[, vars] )
    ## filter outcome negative values
    r = which( apply(out, 1, function(x){ sum(x<0, na.rm = TRUE)  }) > 0 )
    out = out[-r, ]
    return(out[, id])
  })
  names(clinic_ids) = clinic_prefix
  
  ## A unique list of study ids
  study_ids = sort( unique(unlist(clinic_ids)) )
  
  ## Sample a clinic for each aln (individual)
  clinic_samples = sapply(study_ids, function(n){
    f = lapply(clinic_ids, function(v){ n %in% v })
    f = unlist(f)
    ## Which clinic is it present in (TRUE | FALSE)
    f = which(f)
    ## sample among the clinics the aln is in
    sample( names( f ), 1 )
  })
  
  ## Define a data frame to return to user.
  out = data.frame(aln = study_ids, clinic = clinic_samples)
  
  return(out)
}
