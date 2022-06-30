############# Develop machine learning algorithms for prediction of RUE, HI and phenology
Weather_ML_table_for_phen <- NULL
Weather_ML_table_for_HI <- NULL
Weather_ML_table_for_RUE <- NULL 
for (iml_treat in 1:length(Training_treat_f)){
  
  #### Select treatment and save total days from emergence to anthesis and maturity
  itreat_for_ML <- Training_treat_f[iml_treat]
  
  # Subset crop data for treatment 
  Crop_dates_treatment <- subset(End_of_season_crop_data, End_of_season_crop_data$Treatment_number==itreat_for_ML)
  
  # Import weather data for specific treatment
  weath_treat <- paste('Treatment', Crop_dates_treatment$Treatment_number, '.wth', sep='')
  Weather_file_treatment <- read.table(paste('./Input_data/Weather_data/', weath_treat, sep=''), header=TRUE)
  
  Weather_file_treatment <- Weather_file_treatment[,c('DATE', 'SRAD', 'TMAX', 'TMIN', 'RAIN', 'WIND', 'TDEW')]
  
  # Emergence to anthesis and maturity total number of days
  Emerg_anth_treat <- Crop_dates_treatment$Emer_Anth_days
  Emerg_matur_treat <- Crop_dates_treatment$Emer_Matur_days
  
  # Add new column with day number starting with 1 on first day after emergence
  Weather_file_treatment$Day_number <- seq(1:nrow(Weather_file_treatment))
  
  for (iday in 1:nrow(Weather_file_treatment)){
    if (Weather_file_treatment$Day_number[iday] < Emerg_anth_treat){
      Weather_file_treatment$iphen[iday] = 0 
    }else if (Weather_file_treatment$Day_number[iday] >= Emerg_anth_treat & Weather_file_treatment$Day_number[iday] < Emerg_matur_treat){
      Weather_file_treatment$iphen[iday] = 1
    }else{
      Weather_file_treatment$iphen[iday] = 2
    }
  }

  # Calculate cumulative sum of SRAD
  Weather_file_treatment$SRAD_ACCU <- cumsum(Weather_file_treatment$SRAD)
  
  ### Calculate thermal time accumulation for treatment
  Tav <- (Weather_file_treatment$TMAX + Weather_file_treatment$TMIN)/2
  Weather_file_treatment$TACCU <- cumsum(Tav)
  
  # Calculate VPD   
  saturation_vapor_pressure_leaf = (0.61078 * exp(17.27 * Weather_file_treatment$TMAX/(Weather_file_treatment$TMAX + 237.3)) + 
                                      0.61078 * exp(17.27 * Weather_file_treatment$TMIN/(Weather_file_treatment$TMIN + 237.3)))/2
  
  saturation_vapor_pressure_dew_point = 0.61078 * exp(17.27 * Weather_file_treatment$TDEW/(Weather_file_treatment$TDEW + 237.3))
  Weather_file_treatment$VPD = saturation_vapor_pressure_leaf - saturation_vapor_pressure_dew_point
  
  # Add cultivar feature
  if (itreat_for_ML <= 12){
    Weather_file_treatment$Cultivar <- 1
    
  }else if(itreat_for_ML > 12 & itreat_for_ML <= 26){
    Weather_file_treatment$Cultivar <- 2
    
  }else {
    Weather_file_treatment$Cultivar <- 3
  }

  ######## Build matrices with target variables (RUE, dHI/dt, iphen) and covariates 
  # Import optimal biomass and yield time series from s-curve fitting (script: Biomass_and_yield_s_curves.R)
  w_treat_train <- unlist(biom_yield_opt_list[[iml_treat]][1])*100
  yield_treat_train <- unlist(biom_yield_opt_list[[iml_treat]][2])*100
  
  ###### Calculate HI 
  HI_vec_train <- yield_treat_train/w_treat_train
  
  ##### Calculate RUE and partition W to MP and MS
  MP_vec_train <- NULL
  MS_vec_train <- NULL
  MY_vec_train <- NULL
  RUE_vec_train <- NULL
  for (l in 1:length(w_treat_train)){
    BMASS_train = w_treat_train[l]
    HI_train = HI_vec_train[l]
    MP_train = as.numeric(newton.raphson(func_train, 0.1, 1000)[1])
    Qo_train = Weather_file_treatment$SRAD[l]/2
    if (l==1){
      dW_train <- w_treat_train[l]
    }else{
      dW_train <- w_treat_train[l] - w_treat_train[l-1]
    }
    RUE_train <- dW_train/(Qo_train*(1-exp(-kfac*SLA*10^(-4)*MP_train)))
    RUE_vec_train <- c(RUE_vec_train, RUE_train)
    MS_train = h*(MP_train^g)
    MY_train = HI_train*BMASS_train
    
    # Update MP, SM, MY vectors
    MP_vec_train <- c(MP_vec_train, MP_train)
    MS_vec_train <- c(MS_vec_train, MS_train)
    MY_vec_train <- c(MY_vec_train, MY_train)
  }
  
  # Calculate MS_ratio, dHI/dt and MP_ratio_lag for use as features in ML algorithms 
  
  ###### Weather files for the 3 target variables:
  # For RUE prediction
  MP_ratio_train <- MP_vec_train / (MP_vec_train + MS_vec_train + MY_vec_train)
  MP_ratio_lag <- c(0, MP_ratio_train[1:length(MP_ratio_train) - 1])
  Weather_file_treatment_RUE <- subset(Weather_file_treatment, Weather_file_treatment$Day_number <= Emerg_matur_treat)
  Weather_ML_table_for_RUE <- rbind(Weather_ML_table_for_RUE, data.frame(Weather_file_treatment_RUE, RUE_vec_train, MP_ratio_lag, itreat_for_ML))
  
  ##### For HI prediction (data points start at anthesis i.e. iphen >0)
  HI_vec_train_f <- HI_vec_train[HI_vec_train>0]
  HI_lag <- c(0, 0, HI_vec_train_f[1:length(HI_vec_train_f) - 1])
  dhdt_train <- c(0, diff(HI_vec_train))
  dhdt_train_for_ML <- c(0, dhdt_train[dhdt_train>0])
  Weather_file_treatment_HI <- Weather_file_treatment[(Weather_file_treatment$iphen != 0) & (Weather_file_treatment$Day_number<= Emerg_matur_treat), ]
  Weather_ML_table_for_HI <- rbind(Weather_ML_table_for_HI, data.frame(Weather_file_treatment_HI, HI_lag, dhdt_train_for_ML, itreat_for_ML))   
  
  ##### Weather file for phenology prediction
  Weather_file_treatment_phen <- Weather_file_treatment[1:(sum(Weather_file_treatment$iphen==0) + 2*sum(Weather_file_treatment$iphen==1)),]
  MS_ratio_train <- MS_vec_train / (MP_vec_train + MS_vec_train + MY_vec_train)
  MS_ratio_train_for_ML <- c(MS_ratio_train, rep(tail(MS_ratio_train, 1), (nrow(Weather_file_treatment_phen) - length(MS_ratio_train))))
  Weather_ML_table_for_phen <- rbind(Weather_ML_table_for_phen, data.frame(Weather_file_treatment_phen, MS_ratio_train_for_ML, itreat_for_ML))
}
  
# Rename columns
names(Weather_ML_table_for_phen)[names(Weather_ML_table_for_phen) == 'MS_ratio_train_for_ML'] <- 'MS_ratio'
names(Weather_ML_table_for_RUE)[names(Weather_ML_table_for_RUE) == 'RUE_vec_train'] <- 'RUE'
names(Weather_ML_table_for_HI)[names(Weather_ML_table_for_HI) == 'dhdt_train_for_ML'] <- 'dhdt'

############ Final ML matrices for prediction of three target variables
Weather_ML_table_for_RUE_f <- Weather_ML_table_for_RUE[, RUE_inputs]
Weather_ML_table_for_phen_f <- Weather_ML_table_for_phen[, iphen_inputs]
Weather_ML_table_for_phen_f$iphen <- as.factor(Weather_ML_table_for_phen_f$iphen)
Weather_ML_table_for_HI_f <- Weather_ML_table_for_HI[, dhdt_inputs]

############# Apply ML algorithms for prediction of RUE, dHI/dt and iphen
xgb_best_hyperparams_table <- rf_best_hyperparams_table <- NULL

# Call script to optimize hyperparameters of ML models and save them in table
for (ialg in 1:length(target_var)){
  algor_for_opt <- target_var[ialg]
  source("./ML_optimization_options.R")
  source("./Optimize_ML_model_hyperparameters.R")
}

##### Use the optimized hyperparameters to develop ML models 
for (ialg in 1:length(target_var)){
  algor_for_opt <- target_var[ialg]
  ML_name <- paste("ML_", algor_for_opt, sep = "")
  source("./ML_optimization_options.R")
  
  if (ML_model == 'Random_forests'){
    assign(ML_name, rand_forest(trees = rf_best_hyperparams_table$trees[rf_best_hyperparams_table$algor_for_opt==algor_for_opt], 
                                mtry = rf_best_hyperparams_table$mtry[rf_best_hyperparams_table$algor_for_opt==algor_for_opt], 
                                min_n = rf_best_hyperparams_table$min_n[rf_best_hyperparams_table$algor_for_opt==algor_for_opt]) %>%
             set_engine("ranger", importance = "impurity") %>% set_mode(mode_ML) %>%
             fit(as.formula(form), data = ML_table_for_opt))
    
  }else if (ML_model == 'XG_boost') {
  assign(ML_name, boost_tree(trees = xgb_best_hyperparams_table$trees[xgb_best_hyperparams_table$algor_for_opt==algor_for_opt],
                               mtry = xgb_best_hyperparams_table$mtry[xgb_best_hyperparams_table$algor_for_opt==algor_for_opt], 
                               tree_depth = xgb_best_hyperparams_table$tree_depth[xgb_best_hyperparams_table$algor_for_opt==algor_for_opt]) %>% 
                               set_engine("xgboost")%>% set_mode(mode_ML) %>% 
                               fit(as.formula(form), data = ML_table_for_opt))
  }
}
