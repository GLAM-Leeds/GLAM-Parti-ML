########### Script to fit s-curves to observed biomass and yield values during the training period
#### We used the beta growth function from paper: A Flexible Sigmoid Function of Determinate Growth ...

# For every experiment, the goal is to find the day of maximum crop growth (tm_opt) based on the s-curve and the observed data
tm_opt_table <- NULL
biom_yield_opt_list <- list()

for (iscurv in 1:length(Training_treat_f)){
  itreat_scurv = Training_treat_f[iscurv]
  
  # Measurements for specific treatment
  Within_season_crop_data_treatment <- subset(Within_season_crop_data, Within_season_crop_data$Treatment_number == itreat_scurv)
  End_of_season_crop_data_treatment <- subset(End_of_season_crop_data, End_of_season_crop_data$Treatment_number == itreat_scurv)
  
  # For biomass and yield s_curves
  variables_for_s_curve = c('biom', 'yield')
  for (vars in 1:length(variables_for_s_curve)){
    ivar = variables_for_s_curve[vars]
    variable = paste('max_', ivar, sep = '')
    variable_max = End_of_season_crop_data_treatment[, variable]
    te_treatment  <- ifelse(ivar == 'biom', End_of_season_crop_data_treatment$Emer_Matur_days,
                            End_of_season_crop_data_treatment$Emer_Matur_days - End_of_season_crop_data_treatment$Emer_Anth_days)
    t_var = c(1:te_treatment)
    
    ##### Iterate to find optimal tm for biomass and yield 
    # Start with very high RMSE 
    RMSE_opt_var = 10^6
    for (istep_scurv in seq(0.05, 0.95, by=0.05)){
      
      # biomass and yield time series with s-curve
      tm_var = istep_scurv*te_treatment
      time_series_variable = variable_max*(1+(te_treatment-t_var)/(te_treatment-tm_var))*(t_var/te_treatment)^(te_treatment/(te_treatment-tm_var))
      
      # Make biomass table with time series values and days after emergence
      if (ivar == 'yield'){time_series_variable = c(rep(0, End_of_season_crop_data_treatment$Emer_Anth_days), time_series_variable)}
      var_table <- data.frame(c(1:End_of_season_crop_data_treatment$Emer_Matur_days), time_series_variable)
      names(var_table) <- c('DAE', 'Variable_fitted')
      
      # Merge tables of observed and fitted biomass 
      Obs_fitted_var_table <- merge(Within_season_crop_data_treatment, var_table, by = 'DAE')
      
      # Find RMSE between biomass and yield observations and simulations
      obs_var <- Obs_fitted_var_table[, paste(ivar, '_season', sep='')]
      
      # The ISGHE experiments did not include within season grain mass measurements and we use the biomass tm_opt for the yield s-curve too.
      if (ivar == 'yield' & sum(!is.na(Within_season_crop_data_treatment$yield_season)) <= 1){
        tm_opt_yield <- tm_opt_var
        tm_var = tm_opt_yield*te_treatment
        yield_opt = c(rep(0, End_of_season_crop_data_treatment$Emer_Anth_days), variable_max*(1+(te_treatment-t_var)/(te_treatment-tm_var))*(t_var/te_treatment)^(te_treatment/(te_treatment-tm_var)))
        
      }else{
        RMSE_istep_var <- RMSE(Obs_fitted_var_table$Variable_fitted, obs_var)
        
        # If RMSE lower than previous replace optimal tm with new value
        if (RMSE_istep_var < RMSE_opt_var) {
          RMSE_opt_var = RMSE_istep_var
          tm_opt_var <- istep_scurv
          best_time_series_variable <- paste(ivar, '_opt', sep = "")
          assign(best_time_series_variable, time_series_variable)
        }
      }
    }
    tm_opt_table <- rbind(tm_opt_table, data.frame(tm_opt_var, RMSE_opt_var, ivar, itreat_scurv))
  }
  ## Make list with optimal biomass and yield time series
  treat_name <- paste('treat', itreat_scurv, sep='')
  biom_yield_opt <- list(biom_opt, yield_opt)
  biom_yield_opt_list[[treat_name]] <- biom_yield_opt
}
