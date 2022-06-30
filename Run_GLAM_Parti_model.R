################## Run GLAM-Parti model
for (irun in 1:length(Evaluation_treat_f)){
  itreat_number <- Evaluation_treat_f[irun]
  itreat_name <- End_of_season_crop_data$Treatment_name[End_of_season_crop_data$Treatment_number==itreat_number]
  
  # Import input weather data for specific treatment
  Input_weather_for_treat <- read.table(paste('./Input_data/Weather_data/Treatment', itreat_number, '.wth', sep=''), header=TRUE)
  Input_weather_for_treat$Day <- seq(1:nrow(Input_weather_for_treat))
  
  # Calculate SRAD_ACCU and TACCU
  Input_weather_for_treat$SRAD_ACCU <- cumsum(Input_weather_for_treat$SRAD)
  Input_weather_for_treat$TAV <- (Input_weather_for_treat$TMAX + Input_weather_for_treat$TMIN)/2
  Input_weather_for_treat$TACCU <- cumsum(Input_weather_for_treat$TAV)
  
  # Calculate VPD   
  saturation_vapor_pressure_leaf_treat = (0.61078 * exp(17.27 * Input_weather_for_treat$TMAX/(Input_weather_for_treat$TMAX + 237.3)) + 
                                            0.61078 * exp(17.27 * Input_weather_for_treat$TMIN/(Input_weather_for_treat$TMIN + 237.3)))/2
  
  saturation_vapor_pressure_dew_point_treat = 0.61078 * exp(17.27 * Input_weather_for_treat$TDEW/(Input_weather_for_treat$TDEW + 237.3))
  Input_weather_for_treat$VPD = saturation_vapor_pressure_leaf_treat - saturation_vapor_pressure_dew_point_treat
  
  # Define cultivar
  if (itreat_number <= 12){
    Input_weather_for_treat$Cultivar <- 1
    
  }else if(itreat_number > 12 & itreat_number <= 26){
    Input_weather_for_treat$Cultivar <- 2
    
  }else {
    Input_weather_for_treat$Cultivar <- 3
  }
  
  # Start with values of anthesis and maturity dates that are deliberately out of range (these will be updated later)
  Anthesis_day = 0
  Maturity_day = 366
  iday = 0
  icounter = 0 
  
  # Start empty vectors of state and rate variables
  W_vec <- NULL
  MP_vec <- NULL
  MS_vec <- NULL
  YIELD_vec <- NULL
  RUE_vec <- NULL
  HI_vec <- NULL
  
  # Initialize variables (used in first day after emergence)
  W = 0
  HI = 0
  dhdt = 0
  MP_ratio_lag <- 0
  
  while (iday < Maturity_day){
    iday = iday + 1
    
    # Import daily solar radiation
    Qo = Input_weather_for_treat$SRAD[Input_weather_for_treat$Day==iday]/2
    
    # Predict RUE with ML
    ML_table_for_RUE <- data.frame(Input_weather_for_treat[iday, RUE_inputs[1:(length(RUE_inputs)-2)]], MP_ratio_lag)
    RUE <- as.numeric(predict(ML_RUE, ML_table_for_RUE))
    
    # Predict harvest index with ML
    if (Anthesis_day > 0){
      SRAD_ACCU_HI <- Input_weather_for_treat$SRAD_ACCU[iday] - Input_weather_for_treat$SRAD_ACCU[Anthesis_day - 1]
      TACCU_HI <- Input_weather_for_treat$TACCU[iday] - Input_weather_for_treat$TACCU[Anthesis_day - 1]
      
      ML_table_for_HI <- data.frame(Input_weather_for_treat[iday, dhdt_inputs[1:(length(dhdt_inputs)-2)]], HI) 
      names(ML_table_for_HI)[names(ML_table_for_HI) == "HI"] <- "HI_lag"
      dhdt <- as.numeric(predict(ML_dhdt, ML_table_for_HI))
      HI <- HI + dhdt
    }
    
    ### Run GLAM-Parti to estimate variables
    # Use SEMAC to predict MP (Mass of photosynthetic organs)
    MP = as.numeric(newton.raphson(func_test, 400, 1000)[1])
    
    # Update biomass growth (dW), total above-ground biomass (W), grain yield (YIELD), Mass of Stems (MS), stem:biomass ratio (MS_ratio), photosynthetic mass:biomass ratio (MP_ratio)
    dW = RUE*Qo*(1-exp(-kfac*SLA*10^(-4)*MP))
    W = W + dW
    MS = h*(MP^g)
    YIELD = HI*W
    MS_ratio = MS/W
    MP_ratio_lag = MP / W
    
    ### Update vectors
    W_vec = c(W_vec, W)
    MP_vec = c(MP_vec, MP)
    MS_vec = c(MS_vec, MS)
    YIELD_vec = c(YIELD_vec, YIELD)
    HI_vec <- c(HI_vec, HI)
    RUE_vec = c(RUE_vec, RUE)
    
    # Predict crop phenology with ML (iphen = 0 (pre-anthesis), iphen = 1 (anthesis-maturity), iphen = 2 (harvest time))
    ML_table_for_phenology <- data.frame(Input_weather_for_treat[iday, iphen_inputs[1:(length(iphen_inputs)-2)]], MS_ratio) 
    phenol_day <- predict(ML_Phenology, ML_table_for_phenology) 
    
    if (phenol_day=='1' & icounter == 0){
      Anthesis_day = iday
      icounter = 1
    }else if (phenol_day=='2'){
      Maturity_day = iday
    }
  }
  # Save model output to folder
  Daily_model_output <- rbind(Daily_model_output, data.frame(itreat_name, itreat_number, c(1:length(W_vec)), MP_vec, MS_vec, W_vec, YIELD_vec, HI_vec, RUE_vec, no_experiments_ML_training, irep,  paste(Training_treat, collapse = ","), ML_model))
  
  End_of_season_model_output <- rbind(End_of_season_model_output, data.frame(itreat_name, itreat_number, tail(MP_vec, 1), tail(MS_vec, 1), tail(W_vec, 1), tail(YIELD_vec, 1), 
                                                                             tail(HI_vec, 1), tail(RUE_vec, 1), Anthesis_day, Maturity_day, no_experiments_ML_training, irep, paste(Training_treat, collapse = ","), ML_model))
}