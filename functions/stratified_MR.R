########################################
## A Function to perform the Stratified
## MR for the VitD and Bone Health Study
## -- initial code shared by Steven Burgess
## Function written by David A Hughes
## Date: Aug 25th 2022
########################################
# wdata = mydata
# exposure = "FOM1_vitD_season_and_clinic_centered"
# outcome = "FOM1_Hip_Total_BMD"
# instrument = "pgs"
# covariables = c("age_at_FOM1", paste0("PC", 1:10))
# strata_breaks = c(0,25,50,75,Inf)
# strata_breaks_v2 = c(0,20,30,40,50,60,70,Inf)
###################################################
stratified_MR = function(wdata,
                         exposure, 
                         outcome,
                         instrument,
                         covariables = NULL,
                         strata_breaks,
                         strata_breaks_v2 = NULL){
  
  ## Redefine working data 
  wdata = wdata[, c(exposure, outcome, instrument, covariables)]
  wdata = na.omit(wdata)
  
  ## IS OUTCOME BINARY ??
  unique_outcome_count = length( unique( wdata[, outcome] ) ) 
  is_binary = unique_outcome_count == 2 
  
  ## to remove negative values (-1 & -10) any NAs
  w = which( wdata[, exposure] < 0 ); if(length(w)>0){ wdata[ w , exposure] = NA }
  if(!is_binary){
    w = which( wdata[, outcome] < 0 ); if(length(w)>0){ wdata[ w , outcome] = NA }  
  }
  wdata = na.omit(wdata)
  
  ## sample size 
  N = nrow(wdata)
  
  #########################################################
  ## DEFINE THE MODELS
  #########################################################
  ## outcome on exposure model
  if(length(covariables) == 0 ){
    OE_form = formula( paste0( outcome , " ~ " , exposure ) )  
  } else {
    OE_form = formula( paste0( outcome , " ~ " , paste0( covariables, collapse = " + ") , " + " ,  exposure ) )  
  }
  
  ## exposure on instrument model
  if(length(covariables) == 0 ){
    EI_form = formula( paste0( exposure , " ~ " , instrument ) )  
  } else {
    EI_form = formula( paste0( exposure , " ~ " , paste0( covariables, collapse = " + ") , " + " ,  instrument ) )  
  }
  
  ## outcome on instrument model
  if(length(covariables) == 0 ){
    OI_form = formula( paste0( outcome , " ~ " , instrument ) )  
  } else {
    OI_form = formula( paste0( outcome , " ~ " , paste0( covariables, collapse = " + ") , " + " ,  instrument ) )  
  }
  
  #########################################################
  ## ESTIMATE SUMMARY STATSITCS FOR THE COMPLETE DATA SET
  #########################################################
  ## 0)
  Exposure_ss = c( mean(wdata[,exposure]), 
                   sd(wdata[,exposure]),
                   quantile(wdata[, exposure], probs = c(0,0.025,0.975,1)) )
  if(is_binary){
    names(Exposure_ss) = paste0("exposure_all_", c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100") )
  } else{
    names(Exposure_ss) = c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100")   
  }
  
  
  if(is_binary){
    Outcome_ss = table(wdata[,outcome])
    names(Outcome_ss) = paste0("outcome_all_n_", names(Outcome_ss))
  } else {
    Outcome_ss = c( mean(wdata[,outcome]), 
                    sd(wdata[,outcome]),
                    quantile(wdata[, outcome], probs = c(0,0.025,0.975,1)) )
    # names(Outcome_ss) =  c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100") 
    names(Outcome_ss) = paste0("outcome_all_", c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100") )
  }
  
  if(is_binary){
    EO_sumstats = c(Exposure_ss, Outcome_ss)
  } else {
    EO_sumstats = rbind(exposure = Exposure_ss, outcome = Outcome_ss)
  }
  
  
  
  ## 1) outcome on exposure : THE OBSERVATIONAL ANALYSIS
  if(is_binary){
    fit_obs = glm( OE_form, data = wdata, family = binomial())  
  } else {
    fit_obs = lm( OE_form, data = wdata)  
  }
  s = summary(fit_obs)
  res = residuals(fit_obs)
  if(length(res)>5000){ res = sample(res, 5000) }
  W = shapiro.test(res)$statistic
  n = length(res)
  complete_dataset_OE_sumstats = c(n, W, s$coef[exposure, ])
  names(complete_dataset_OE_sumstats) = paste0("all_OE_", c( "n", "W","beta","se","t","p" ))
  
  
  ## 2) exposure on instrument model (S.B. calls the bx)
  fit_ei = lm( EI_form, data = wdata )
  s = summary(fit_ei)
  res = residuals(fit_ei)
  if(length(res)>5000){ res = sample(res, 5000) }
  W = shapiro.test(res)$statistic
  n = length(res)
  complete_dataset_EI_sumstats = c(n, W, s$coef[instrument, ])
  names(complete_dataset_EI_sumstats) = paste0("all_EI_", c( "n", "W","beta","se","t","p" ))
  
  
  ## 3) outcome on instrument model (S.B. calls the by)
  if(is_binary){
    fit_oi = glm( OI_form, data = wdata, family = binomial())  
    s = summary(fit_oi, data = wdata)
  } else {
    fit_oi = lm( OI_form, data = wdata )
  }
  s = summary(fit_oi)
  res = residuals(fit_oi)
  if(length(res)>5000){ res = sample(res, 5000) }
  W = shapiro.test(res)$statistic
  n = length(res)
  complete_dataset_OI_sumstats = c(n, W, s$coef[instrument, ])
  names(complete_dataset_OI_sumstats) = paste0("all_OI_", c("n", "W","beta","se","t","p" ))
  
  ## Pull the estiamtes together
  complete_dataset_estimates = rbind( complete_dataset_OE_sumstats,
                                      complete_dataset_EI_sumstats,
                                      complete_dataset_OI_sumstats)
  
  complete_dataset_estimates = list(exposure_outcome_sumstats = EO_sumstats,
                                    complete_model_estimates = complete_dataset_estimates)
  
  
  #########################################################
  ## Instrument free Exposure
  #########################################################
  ## model
  EI_form = formula( paste0( exposure , " ~ " , instrument ) )
  ## fit model
  fit0 = lm(EI_form, data = wdata)
  ## extract fitted values
  fitted_vals = fitted(fit0)
  ## mean exposure estimate
  mean_exposure = mean( wdata[, exposure], na.rm = TRUE)
  ## Instrument free exposure estimate
  wdata$Ifree_exposure = wdata[, exposure] - fitted_vals + mean_exposure
  
  #########################################################
  ## Stratify the data set
  #########################################################
  wdata$strata = as.numeric( cut( wdata$Ifree_exposure, breaks = strata_breaks, include.lowest=TRUE) )
  strataIDs = sort( as.vector( na.omit( unique(wdata$strata) ) ) )
  strata_data = lapply(strataIDs, function(i){
    w = which(wdata$strata == i)
    out = wdata[w,]
    return(out)
  })
  ## strata names
      # strata_names = sapply(1:(length(strata_breaks)-1), function(i) { 
      #   paste0(strata_breaks[i],"-to-",strata_breaks[i+1] ) 
      #   } )
      # names(strata_data) = paste0("strata_", strata_names[1:length(strata_data)])
  names(strata_data) = paste0("strata_", strataIDs )
  
  # ## IS there a second set of stratification breaks ?
  # if( length(strata_breaks_v2) > 0){
  #   wdata$strata_v2 = as.numeric( cut( wdata$Ifree_exposure, breaks = strata_breaks_v2, include.lowest=TRUE) )
  #   strataIDs = sort( as.vector( na.omit( unique(wdata$strata_v2) ) ) )
  #   strata_data_v2 = lapply(strataIDs, function(i){
  #     w = which(wdata$strata_v2 == i)
  #     out = wdata[w,]
  #     return(out)
  #   })
  #   names(strata_data_v2) = paste0("strata_", strataIDs )
  # }
  # 
  #########################################################
  ## ESTIMATE SUMMARY STATSITCS FOR THE STRATA
  #########################################################
  stratified_estimates = lapply(strata_data, function(sdata){
    
    ## 0) Exposure Outcome Sum Stats 
    strata_Exposure_ss = c( mean(sdata[,exposure]), 
                     sd(sdata[,exposure]),
                     quantile(sdata[, exposure], probs = c(0,0.025,0.975,1)) )
    names(strata_Exposure_ss) = c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100") 
    
    strata_Ifree_Exposure_ss = c( mean(sdata[,"Ifree_exposure"]), 
                            sd(sdata[,"Ifree_exposure"]),
                            quantile(sdata[, "Ifree_exposure"], probs = c(0,0.025,0.975,1)) )
    names(strata_Ifree_Exposure_ss) = c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100") 
    
    if(is_binary){
      strata_Outcome_ss = table(sdata[,outcome])
      names(strata_Outcome_ss) = paste0("outcome_n_", names(strata_Outcome_ss))
    } else {
      strata_Outcome_ss = c( mean(sdata[,outcome]), 
                      sd(sdata[,outcome]),
                      quantile(sdata[, outcome], probs = c(0,0.025,0.975,1)) )
      names(strata_Outcome_ss) =  c("mean","sd","percentile_0", "percentile_2.5","percentile_97.5","percentile_100") 
    }
    ## collect sum stats for exposure and outcome
    if(length(strata_Exposure_ss) == length(strata_Outcome_ss) ){
      strata_OE_SS = rbind(strata_Exposure_ss, strata_Ifree_Exposure_ss,  strata_Outcome_ss)  
      rownames(strata_OE_SS) = c("esposure","IFree_exposure","outcome")
    } else {
      strata_OE_SS = rbind(strata_Exposure_ss, strata_Ifree_Exposure_ss) 
      rownames(strata_OE_SS) = c("esposure","IFree_exposure")
      strata_OE_SS = list(strata_OE_SS, strata_Outcome_ss )
      names(strata_OE_SS) = c("exposure","outcome")
    }
    
    
    
    ## 1) outcome on exposure : THE OBSERVATIONAL ANALYSIS
    if(is_binary){
      fit_obs = glm( OE_form, data = sdata, family = binomial())  
    } else {
      fit_obs = lm( OE_form, data = sdata)  
    }
    s = summary(fit_obs)
    res = residuals(fit_obs)
    if(length(res)>5000){ res = sample(res, 5000) }
    W = shapiro.test(res)$statistic
    n = length(res)
    OE_sumstats = c(n, W, s$coef[exposure, ])
    names(OE_sumstats) = paste0("OE_", c( "n", "W","beta","se","t","p" ))
    
    
    ## 2) exposure on instrument model (S.B. calls the bx)
    fit_ei = lm( EI_form, data = sdata )
    s = summary(fit_ei)
    res = residuals(fit_ei)
    if(length(res)>5000){ res = sample(res, 5000) }
    W = shapiro.test(res)$statistic
    n = length(res)
    EI_sumstats = c(n, W, s$coef[instrument, ])
    names(EI_sumstats) = paste0("EI_", c( "n", "W","beta","se","t","p" ))
    
    
    ## 3) outcome on instrument model (S.B. calls the by)
    if(is_binary){
      fit_oi = glm( OI_form, data = sdata, family = binomial())  
    } else {
      fit_oi = lm( OI_form, data = sdata )
    }
    s = summary(fit_oi)
    res = residuals(fit_oi)
    if(length(res)>5000){ res = sample(res, 5000) }
    W = shapiro.test(res)$statistic
    n = length(res)
    OI_sumstats = c(n, W, s$coef[instrument, ])
    names(OI_sumstats) = paste0("OI_", c("n", "W","beta","se","t","p" ))
    
    ## Pull the estimates together
    strata_dataset_estimates = rbind( OE_sumstats,
                                        EI_sumstats,
                                        OI_sumstats)
    ## return data estimate
    out = list(strata_OE_SS, strata_dataset_estimates)
    names(out) = c("exposure_outcome_sumstats","model_effect_estimates")
    return(out )
  })
  
  
  ## Compile All Results
  results_out = list(complete_dataset_estimates = complete_dataset_estimates, 
                     stratified_estimates = stratified_estimates)
  
  return(results_out)
}
