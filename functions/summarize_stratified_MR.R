########################################
## A Function summarize and tabulate
## the summary statisitics proveded by
## the function stratified_MR()
## Function written by David A Hughes
## Date: Aug 26th 2022
########################################
summarize_stratified_MR = function(sMR_data){
  
  #########################################################
  ## (1.) Pull together Exposure Outcome Summary Statistics
  #########################################################
  ### Is the outcome binary ??
  is_binary = is.vector(sMR_data[[1]][[1]])
  
  ## Observational Exposure Outcome Summary
  if(is_binary){
    exposure_outcome_ss = c( sMR_data[[1]][[2]][1,1], 
                             sMR_data[[1]][[1]][c(1,2,4,5, 7,8)] )
    names(exposure_outcome_ss) = c("n","exp_mean","exp_sd","exp_lowerCI", "exp_upperCI", 
                                   "out_no", "out_yes")  
  } else{
    exposure_outcome_ss = c( sMR_data[[1]][[2]][1,1], 
                             sMR_data[[1]][[1]][1,c(1,2,4,5)], 
                             sMR_data[[1]][[1]][2,c(1,2,4,5)] )
    names(exposure_outcome_ss) = c("n","exp_mean","exp_sd","exp_lowerCI", "exp_upperCI", 
                                   "out_mean","out_sd","out_lowerCI", "out_upperCI")  
  }
  
  ## Stratified Exposure Outcome Summary
  for(i in 1:length(sMR_data[[2]]) ){
    d = sMR_data[[2]][[i]]
    ##########
    if(is_binary){
      temp_ss = c( d[[2]][1,1], 
                   d[[1]][[1]][2,c(1,2,4,5)],
                   d[[1]][[2]])
      names(temp_ss) = c("n","exp_mean","exp_sd","exp_lowerCI", "exp_upperCI", 
                                "out_no", "out_yes")  
    } else{
      temp_ss = c( d[[2]][1,1], 
                          d[[1]][2,c(1,2,4,5)], 
                          d[[1]][3,c(1,2,4,5)] )
      names(temp_ss) = c("n","exp_mean","exp_sd","exp_lowerCI", "exp_upperCI", 
                                "out_mean","out_sd","out_lowerCI", "out_upperCI")  
    }
    ##########
    exposure_outcome_ss = rbind(exposure_outcome_ss, temp_ss)
  }
  
  ## Define rownames
  rownames(exposure_outcome_ss) = c("obs", paste0("strata_", 1:(nrow(exposure_outcome_ss)-1) ) )
  
  
  #########################################################
  ## (2.) Effect Estimates
  #########################################################
  ee = c( sMR_data[[1]][[2]][1, c(3,4,6)],
         sMR_data[[1]][[2]][2, c(3,4,6)],
         sMR_data[[1]][[2]][3, c(3,4,6)])
  mname = c("beta","se","p")
  names(ee) = c( paste0("obs_",mname) , paste0("ei_bx_",mname) , paste0("oi_by_",mname)  )
  effect_ests = ee
  
  for(i in 1:length( sMR_data[[2]] )){
    d = sMR_data[[2]][[i]][[2]]
    ee = c( d[1, c(3,4,6)],
            d[2, c(3,4,6)],
            d[3, c(3,4,6)])
    mname = c("beta","se","p")
    names(ee) = c( paste0("obs_",mname) , paste0("ei_bx_",mname) , paste0("oi_by_",mname)  )
    effect_ests = rbind(effect_ests,ee)
  }
  
  ## Define rownames
  rownames(effect_ests) = c("complete", paste0("strata_", 1:(nrow(effect_ests)-1) ) )
  
  
  return(list(exposure_outcome_ss = exposure_outcome_ss,
              effect_ests = effect_ests))
}
