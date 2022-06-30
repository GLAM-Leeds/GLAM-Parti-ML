################# Script to define GLAM-Parti model options and parameters

# Set seed to replicate the runs when you re-run the model
set.seed(5) 

##### Define datasets which give the training and testing experiments
Training_dataset <- 'HSC_and_ISGHE'
Testing_dataset <- 'HSC_and_ISGHE'

##### Load crop data
End_of_season_crop_data <- read.csv('./Input_data/Crop_data/End_of_season_crop_data.csv')
Within_season_crop_data <- read.csv('./Input_data/Crop_data/Within_season_crop_data.csv')

# Vector with all treatments of the HSC study
All_treat <- End_of_season_crop_data$Treatment_number

# Sample treatments to be used for ML training  
Random_treat_HSC <- sample(seq(1,12), 6)
treat_HSC_train <- sample(Random_treat_HSC, 3)

All_treat_ISGHE <- subset(Within_season_crop_data, Within_season_crop_data$Treatment_number > 12)
Train_sample_ISGHE <- sample(unique(All_treat_ISGHE$Treatment_number), 14)

Training_treat <- Training_treat_f <- c(treat_HSC_train, Train_sample_ISGHE)
no_experiments_ML_training <- length(Training_treat_f)
irep <- 1

# Choose which treatments for the GLAM-Parti evaluation period
#Evaluation_treat <- setdiff(All_treat, Training_treat_f)
Evaluation_treat_f <- c(Random_treat_HSC, seq(13, 40))

####### GLAM-Parti model parameters
kfac = 0.7 # Light extinction coefficient
SLA = 180  # Specific leaf area (cm2 g-1)

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

#### Choose ML model for training of the data
#ML_model <- 'Random_forests'
ML_model <- 'XG_boost'

##### Vector with target variables 
target_var <- c('Phenology', 'RUE', 'dhdt')

#### Choose input features for ML model
# For phenology prediction
iphen_inputs <- c('SRAD', 'TMIN', 'TMAX', 'SRAD_ACCU', 'TACCU', 'Cultivar', 'MS_ratio', 'iphen')

# For RUE prediction
RUE_inputs <- c('SRAD', 'TMIN', 'TMAX', 'VPD', 'Cultivar', 'MP_ratio_lag', 'RUE')

# For harvest index prediction
dhdt_inputs <- c('SRAD', 'TMIN', 'TMAX', 'Cultivar', 'HI_lag', 'dhdt')

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

