########################################
## A Function to build a data set 
## including all covariables derived
## from the sample clinic data frame from
## the function id_sample_pool
## Function written by David A Hughes
## Date: Aug 26th 2022
########################################
build_data_set = function(wdata, 
                          id_sample_pool_df,
                          clinic_specific_vars = c( "vitD_season_and_clinic_centered", "Hip_Total_BMD", "age", "bmi"),
                          covariables = c( paste0("PC", 1:10), "pgs" )
                           ){
  new_data_set = c()
  clinics = sort(unique(id_sample_pool_df$clinic))
  for(x in clinics){
    tempdf = id_sample_pool_df %>% filter(clinic == x)
    alns = tempdf$aln
    ## Find Rows of data with samples
    new_out = wdata %>% filter(aln %in% alns)
    ## Colmns I need
    cols_2_extract = covariables
    if(length(clinic_specific_vars)>0){
      for(i in clinic_specific_vars){
        w = grep(i, colnames(new_out))
        q = grep(x, colnames(new_out)[w] )
        o = colnames(new_out)[w][q]
        cols_2_extract = c(o, cols_2_extract)
      }
    }
    new_out = new_out[, cols_2_extract]
    ## Add clinic info to new_out
    new_out$clinc = x
    ## Edit colnames to remove clinic
    colnames(new_out) = gsub(paste0(x, "_"), "", colnames(new_out) )
    colnames(new_out) = gsub(paste0("_", x), "", colnames(new_out) )
    ## add clinic data to new data set
    new_data_set = rbind(new_data_set,new_out )
  }
  return(new_data_set)
}
