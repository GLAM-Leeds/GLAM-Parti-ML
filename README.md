# GLAM_parti

**Overview**

GLAM-Parti-ML is a new framework which integrates machine learning algorithms into the process-based crop model, GLAM-Parti, for the prediction of crop biomass, grain yield and the crop phenological stage at daily time step. The incorporation of ML into the model eliminates the need of stress factors and reduces the physiological model parameters down to four. GLAM-Parti-ML is written in R and the steps for downloading and running the model to reproduce the results and figures of the manuscript are given below.  

**Steps to run GLAM-Parti-ML and reproduce the results of the paper**

1. Download all folders and R scripts

2. Make sure that you've installed all R packages required. These can be found in the script 'Required_packages.R'.

3. Go to line 5 of the script 'Main_ML_and_GLAM_Parti_script.R' and set your own GLAM-Parti directory with the setwd command. This should be the path where you store the model.

4. In 'Main_ML_and_GLAM_Parti_script.R' - line 15 - the default option is the config file 'Config_file_HSC_data.R'. Inside this config file, there is the option to use two different ML models, Random Forests (RF) and XGBoost. Initially, please use RF (i.e. you mute line 51: XGBoost).  

5. Run Main_ML_and_GLAM_Parti_script.R and go to the folder 'Results'. The model should have produced Fig. 3 of the manuscript.

6. Next, change 'Main_ML_and_GLAM_Parti_script.R' - line 15 - to the config file 'Config_file_HSC_and_ISGHE_data.R'.

7. Open 'Config_file_HSC_and_ISGHE_data.R' and choose RF as ML model. This is done by unmuting line 52 (RF) and muting line 53 (XGBoost).

8. Run 'Main_ML_and_GLAM_Parti_script.R' and go to the folder 'Results'. The model should have produced Fig. 5 and Fig. S6 of the manuscript.

9. Open 'Config_file_HSC_and_ISGHE_data.R' and choose XGBoost as ML model. This is done by muting line 52 (RF) and unmuting line 53 (XGBoost).

10. Run 'Main_ML_and_GLAM_Parti_script.R' and go to the folder 'Results'. The model should have produced Fig. S7 of the manuscript.

11. Run Special/Scripts/Compare_GLAM_and_GParti.R. The moodel should have produced Table 2 and Fig. S5 of the manuscript.

6. Run the script 'Evaluate_GParti_50_train.R' and if successful, the model should produce a figure (Figure3) in the following path: Results/Figures/Figure3.pdf. In my personal laptop, this script takes around 5 minutes to complete. If everything goes well, Figure3 should be identical to Fig. 3 of the GLAM-Parti/ML paper.

6. Go to the script 'Evaluate_GParti_various_train.R' and set your home directory using the setwd command of the first line. This is the same as step 4 but for the second model evaluation script.

7. Run the script 'Evaluate_GParti_various_train.R' and if successful, the model should produce the figures 4  and 4S in the following path: Results/Figures/. This script also produces the tables ‘Anthesis’, ‘Maturity’, ‘Biomass’ and ‘Yield’ in the following path: Results/Tables/. This script takes a long time to complete - around 10 hours in my laptop. If everything goes well, the derived figures should be identical to Fig.4 and 4S of the GLAM-Parti/ML paper and the Tables ‘Anthesis’, ‘Maturity’, ‘Biomass’ and ‘Yield’ should reproduce the values in Tables S1-S4 of the paper.

**Explanation of GLAM-Parti/ML scripts**

`Import_crop_data_and_set_functions.R:` This script imports the observed crop data (biomass, yield, phenology) that will be used in the training process of the ML algorithms and also derives the Newton-Raphson method of SEMAC, as well as any functions that will be used in the model evaluation process (e.g. RMSE).

`Model_parameters.R:` This script sets the values of the 4 physiological model parameters of GLAM-Parti: light extinction coefficient (k); canopy Specific Leaf Area (SLA); a, b coefficients of the allometric relationship between log-mass of stems (MS) vs. log-photosynthetic organ mass (MP).

`Biomass_and_yield_s_curves.R:` This script uses the measured above-ground biomass and grain yield of all data points in the ML training set to fit sigmoid time-series curves according to the method described in Yin et al., 2003 (A Flexible Sigmoid Function of Determinate Growth). This step cannot be undertaken if there is not at least the end-of-season biomass and grain yield measurements, as well as one more mid-season measurement of both variables.

`Machine_learning_for_RUE_HI_and_Phenology.R:` This script loads the input weather data (Tmin, Tmax, Srad, VPD etc.) of each data point in the training period, as well as the time series of biomass and yield (which have been  previously computed in the script Biomass_and_yield_s_curves.R). This allows the derivation of the target variables RUE, dHI/dt and iphen. Following suitable feature selection, the three ML algorithms (corresponding to the 3 target variables) are trained. Currently, the available ML models are Random Forests and XGboost. 

`Optimize_ML_model_hyperparameters.R:` This script optimizes the ML model hyperparameters of both RF and XGBoost using Bayesian search with 10-fold cross validation in 20 iteration setting.

`Run_GLAM_Parti.R:` This script imports the weather data of the given data point and year and calls the GLAM-Parti model.

`GLAM_Parti_model.R:` This script runs the GLAM-Parti crop model and produces daily estimations of above-ground biomass and individual organ mass (stems, photosynthetic organs, grains e.t.c.). It also estimates the phenological stage (days to anthesis, maturity e.t.c.), and predicts the end-of-season biomass and grain yield.

`Evaluate_Gparti_50_train.R` and `Evaluate_Gparti_various_train.R:` These scripts are specific to the evaluation of the GLAM-Parti/ML model against the ‘Hot Serial Cereal Experiment’ for wheat. The script ‘Evaluate_Gparti_50_train.R’ produces Fig. 3 of the GLAM-Parti/ML paper. The script ‘Evaluate_Gparti_various_train.R’ produces Fig. 4 and 4S, as well as the Tables S1 – S4.     
