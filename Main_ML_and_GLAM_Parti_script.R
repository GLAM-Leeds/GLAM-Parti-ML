###### Evaluate GLAM-Parti with 50% of data used for ML training (i.e. six experiments) and 50% for model testing (i.e. six remaining treatments)
#rm(list=ls())

# First set the working directory which is where the GLAM-Parti model folder is stored
setwd('Desktop/Model/GLAM_Parti_new')

# Initialize tables where model output will be stored
Daily_model_output <- NULL
End_of_season_model_output <- NULL

# Call script to load required packages for GLAM-Parti model runs
source("./Required_packages.R")

# Call script to select model options and parameter values (g, h, SLA and kc)
source("./Config_files/Config_file_HSC_data.R")

# Call script to find optimal biomass and yield time series for each treatment in training period based on observed data (we use s-curve formula of Yin et al. 2003)
source("./Biomass_and_yield_s_curves.R")

# Call script to build three ML models for the prediction of RUE, HI and Phenology
if (ML_model == 'Random_forests') {doParallel::registerDoParallel(4)}
source("./ML_models_for_RUE_HI_and_Phenology.R")
if (ML_model == 'Random_forests') {doParallel::stopImplicitCluster()}

### Call script to run GLAM-Parti with optimized ML algorithms for all treatments of the study
source("./Run_GLAM_Parti_model.R")

### Raname columns in model output
names(Daily_model_output) <- c('Treatment_name', 'Treatment_number', 'Day', 'Leaf_Ear_mass', 'Stem_mass', 'Biomass', 'Yield', 'HI', 'RUE', 'No_exper', 'Repetition', 'Training_treatments', 'ML_model')
names(End_of_season_model_output) <- c('Treatment_name', 'Treatment_number', 'Leaf_Ear_mass', 'Stem_mass', 'Sim_biom', 'Sim_yield', 'Sim_HI', 'RUE', 'Sim_Emer_Anth_days', 'Sim_Emer_Matur_days', 'No_exper', 'Repetition', 'Training_treatments', 'ML_model')

### Call script to evaluate GLAM-Parti skill
source("./Evaluate_GLAM_Parti_skill.R")

# Save results in different pdf according to the datasets used for model training and testing
if (Training_dataset == 'HSC' & ML_model == 'Random_forests'){
  ggsave('./Results/Figures/Figure3.pdf', GParti_eval_with_RF_fig2,  width = 10, height = 9, dpi = 300)
  
} else if (Training_dataset == 'HSC_and_ISGHE' & ML_model == 'Random_forests'){
  ggsave('./Results/Figures/Figure5.pdf', GParti_eval_with_RF_fig1, width = 10, height = 9, dpi = 300)
  ggsave('./Results/Figures/FigureS6.pdf', GParti_eval_with_RF_fig2, width = 10, height = 9, dpi = 300)

} else if (Training_dataset == 'HSC_and_ISGHE' & ML_model == 'XG_boost'){
  ggsave('./Results/Figures/FigureS7.pdf', GParti_eval_with_RF_fig2, width = 10, height = 9, dpi = 300)
}
