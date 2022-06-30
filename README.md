# GLAM-Parti-ML

**Overview**

GLAM-Parti-ML is a new framework which integrates machine learning algorithms into the process-based crop model, GLAM-Parti, for the prediction of crop biomass, grain yield and the crop phenological stage at daily time step. The incorporation of ML into the model eliminates the need of stress factors and reduces the physiological model parameters down to four. GLAM-Parti-ML is written in R and the steps for downloading and running the model to reproduce the results and figures of the manuscript are given below.  

**Steps to run GLAM-Parti-ML and reproduce the results of the paper**

1. Download all folders and R scripts in your working directory.

2. Make sure that you've installed all R packages required. These can be found in the script 'Required_packages.R'.

3. Go to line 5 of the script 'Main_ML_and_GLAM_Parti_script.R' and set your own GLAM-Parti working directory with the setwd command. This should be the path where you store the model.

4. In 'Main_ML_and_GLAM_Parti_script.R' - line 15 - the default option is the config file 'Config_file_HSC_data.R'. Inside this config file, there is the option to use two different ML models, Random Forests (RF) and XGBoost. Initially, please use RF (i.e. you mute line 51: XGBoost).  

5. Run Main_ML_and_GLAM_Parti_script.R and go to the folder 'Results'. The model should have produced Fig. 3 of the manuscript.

6. Next, change 'Main_ML_and_GLAM_Parti_script.R' - line 15 - to the config file 'Config_file_HSC_and_ISGHE_data.R'.

7. Open 'Config_file_HSC_and_ISGHE_data.R' and choose RF as ML model. This is done by unmuting line 52 (RF) and muting line 53 (XGBoost).

8. Run 'Main_ML_and_GLAM_Parti_script.R' and go to the folder 'Results'. The model should have produced Fig. 5 and Fig. S6 of the manuscript.

9. Open 'Config_file_HSC_and_ISGHE_data.R' and choose XGBoost as ML model. This is done by muting line 52 (RF) and unmuting line 53 (XGBoost).

10. Run 'Main_ML_and_GLAM_Parti_script.R' and go to the folder 'Results'. The model should have produced Fig. S7 of the manuscript.

11. Open the script 'Compare_GLAM_and_GParti.R', which is located inside the folder 'Special_scripts', go to line 2 and set your own working directory (the directory where you store the model) .

11. Run the script 'Special_scripts/Compare_GLAM_and_GParti.R'. The moodel should produce Table 2 and Fig. S5 of the manuscript in the folder 'Results'.

12. Open the script 'GParti_sens_analysis.R', which is located inside the folder 'Special_scripts', go to line 24 and set your own working directory (the directory where you store the model). 

13. Run the script 'Special_scripts/GParti_sens_analysis.R'. The moodel should produce Fig. 4, Fig. S4 and Tables S2, S3, S4, S5 of the manuscript in the folder 'Results'.



**Explanation of GLAM-Parti-ML scripts**

`Main_ML_and_GLAM_Parti_script.R:` This is the core script of the model, which calls all other scripts to import the weather and crop data, fit the time-series of biomass and grain mass in the training set, train and optimize the ML models, run GLAM-Parti-ML and evaluate the model performance.

`Required_packages.R:` This script loads all packages required for the GLAM-Parti-ML model runs.

`Biomass_and_yield_s_curves.R:` This script uses the above-ground biomass and grain mass measurements (both within season and the end-of-season values) of the experiments in the training set to fit sigmoid time-series curves according to the method described in Yin et al., 2003 (A Flexible Sigmoid Function of Determinate Growth).

`ML_models_for_RUE_HI_and_Phenology.R:` This script loads the input weather data (Tmin, Tmax, Srad, VPD etc.) of each experiment in the training period, as well as the time series of biomass and yield (which have been previously computed in the script Biomass_and_yield_s_curves.R). This allows the derivation of the target variables RUE, dHI/dt and iphen. Following suitable feature selection, the three ML algorithms (corresponding to the 3 target variables) are trained. Currently, the available ML models are Random Forests and XGboost. 

`Optimize_ML_model_hyperparameters.R:` This script optimizes the ML model hyperparameters of both RF and XGBoost using Bayesian search with 10-fold cross validation in 10 iteration setting.

`ML_optimization_options.R:` This script prepares the hyperparameter optimization by choosing the correct options based on the target variable (e.g. regression vs. classification, choice of evaluation metric etc.)  

`Run_GLAM_Parti_model.R:` This script runs the GLAM-Parti crop model and produces daily estimations of above-ground biomass and individual organ mass (stems, photosynthetic organs, grains e.t.c.). It also estimates the phenological stage (days to anthesis, maturity e.t.c.), and predicts the end-of-season biomass and grain yield.

`Evaluate_GLAM_Parti_skill.R:` This scripts evaluates the performance of GLAM-Parti-ML against the ‘Hot Serial Cereal Experiment’ for wheat (HSC) and the International Heat Stress Genotype Experiment (IHSGE). 
