library(gridExtra)
library(ggpubr)
library(pracma) 
library(ggplot2) 
library(randomForest)
library(dplyr)
library(hydroGOF)
library(stringr)
library(ggpmisc)
library(xgboost) 
library(numDeriv)
library(tdr)
library(reshape2)
library(tidymodels)
library(ranger)
library(grid)
library(xtable)
library(doParallel)
library(usemodels) 

#rm(list=ls())

# First set the working directory which is where the GLAM-Parti model folder is stored
setwd('Desktop/Model/GLAM_Parti_new')

Daily_model_output <- NULL
End_of_season_model_output <- NULL

# Set seed to replicate the runs when you re-run the model
set.seed(5) 

####### GLAM-Parti model parameters
kfac = 0.7 # Light extinction coefficient
SLA = 180  # Specific leaf area (cm2 g-1)

##### Define datasets which give the training and testing experiments
Training_dataset <- 'HSC'
Testing_dataset <- 'HSC'

##### Load crop data
End_of_season_crop_data <- read.csv('./Input_data/Crop_data/End_of_season_crop_data.csv')
Within_season_crop_data <- read.csv('./Input_data/Crop_data/Within_season_crop_data.csv')

# Vector with all treatments of the HSC study
All_treat <- End_of_season_crop_data$Treatment_number[1:12]

# Vector with the available ML models
ML_models_table <- c('XG_boost', 'Random_forests')

##### Vector with target variables 
target_var <- c('Phenology', 'RUE', 'dhdt')

#### Choose input features for ML model
# For phenology prediction
iphen_inputs <- c('SRAD', 'TMIN', 'TMAX', 'SRAD_ACCU', 'TACCU', 'MS_ratio', 'iphen')

# For RUE prediction
RUE_inputs <- c('SRAD', 'TMIN', 'TMAX', 'VPD', 'MP_ratio_lag', 'RUE')

# For harvest index prediction
dhdt_inputs <- c('SRAD', 'TMIN', 'TMAX', 'HI_lag', 'dhdt')

# SEMAC function to calculate MP during ML training period
func_train <- function(MP_train) {
  (1/(1-HI_train))*(MP_train + h*(MP_train^g)) - BMASS_train
}

# SEMAC function to calculate MP during model testing period
func_test <- function(MP) {
  (1/(1-HI))*(MP + h*(MP^g)) - RUE*Qo*(1-exp(-kfac*SLA*10^(-4)*MP)) - W
}

# Newton - Raphson method for optimization of SEMAC equation
newton.raphson <- function(f, a, b, tol = 10^(-2), n = 1000) {
  require(numDeriv)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}

# RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2, na.rm = TRUE))
}

#### First train RF and XGBoost on 3-9 treatments and then run GLAM-Parti and test model skill against the unseen treatments.
#### We repeat the process 10 times (i.e. we sample ten times in each training setting (3-9 treatments))
for (iexp in 3:9){
  no_experiments_ML_training <- iexp
  
  for (irep in 1:10){
    
    # Sample treatments to be used for ML training  
    Training_treat <- sample(End_of_season_crop_data$Treatment_name[1:12], no_experiments_ML_training)
    Training_treat_f <- match(Training_treat, End_of_season_crop_data$Treatment_name)
    
    # Choose which treatments for the GLAM-Parti evaluation period
    Evaluation_treat <- setdiff(End_of_season_crop_data$Treatment_name[1:12], Training_treat)
    Evaluation_treat_f <- seq(1:12)
    
    #### Subset within-season crop data table only for treatments in training period 
    Allometry_train_period <- subset(Within_season_crop_data, Treatment_number %in% Training_treat_f)
    
    # Calculate mass of leaves and chaffs for allometric relationship
    Allometry_train_period$Phot_mass <- rowSums(Allometry_train_period[,c("ML_season", "MEAR_season")], na.rm=TRUE)
    Allometry_train_period$Phot_mass[Allometry_train_period$Phot_mass == 0] <- NA 
    
    # Change units from t/ha to g/m2
    Allometry_train_period$Phot_mass <- Allometry_train_period$Phot_mass*100
    Allometry_train_period$MS_season <- Allometry_train_period$MS_season*100
    
    #### Extract slope and intercept from linear regression between log(MS) vs. log(MP) 
    h <- exp(as.numeric(coef(lm(log(Allometry_train_period$MS_season) ~ log(Allometry_train_period$Phot_mass)))[1]))
    g <- as.numeric(coef(lm(log(Allometry_train_period$MS_season) ~ log(Allometry_train_period$Phot_mass)))[2])
    
    # Call script to find optimal biomass and yield time series for each treatment in training period based on observed data (we use s-curve formula of Yin et al. 2003)
    source("./Biomass_and_yield_s_curves.R")
      
    # Choose Machine learning model for GLAM-Parti runs
    for (imodel in 1:length(ML_models_table)){
      ML_model <- ML_models_table[imodel]
      
      # Call script to build three ML models for the prediction of RUE, HI and Phenology
      source("./ML_models_for_RUE_HI_and_Phenology.R")

      ### Call script to run GLAM-Parti with optimized ML algorithms for all treatments of the study
      source("./Run_GLAM_Parti_model.R")
    }
      print(paste0("Done with repetition,", irep, sep = ''))
  }
  print(paste0("Done with training experiment number,", no_experiments_ML_training, sep = ''))
}

### Raname columns in model output
names(Daily_model_output) <- c('Treatment_name', 'Treatment_number', 'Day', 'Leaf_Ear_mass', 'Stem_mass', 'Biomass', 'Yield', 'HI', 'RUE', 'No_exper', 'Repetition', 'Training_treatments', 'ML_model')
names(End_of_season_model_output) <- c('Treatment_name', 'Treatment_number', 'Leaf_Ear_mass', 'Stem_mass', 'Sim_biom', 'Sim_yield', 'Sim_HI', 'RUE', 'Sim_Emer_Anth_days', 'Sim_Emer_Matur_days', 'No_exper', 'Repetition', 'Training_treatments', 'ML_model')

#write.csv(Daily_model_output, "./Special_scripts/Daily_output.csv", row.names = FALSE)
#write.csv(End_of_season_model_output, "./Special_scripts/End_of_season_output.csv", row.names = FALSE)

source("./Special_scripts/Evaluate_sens_analysis.R")

