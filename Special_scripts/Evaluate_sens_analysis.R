##### Calculate evaluation metrics for GLAM-Parti sensitivity analysis and produce Fig. 4 and Fig. S4 of manuscript 

All_treat_names <- c("1C", "1H", "2C", "7C", "7H", "8C", "9C", "9H", "10C", "14C", "14H", "15C")

# Change units of simulated biomass and yield from g*m-2 to t*ha-1  
Daily_model_output$Biomass=Daily_model_output$Biomass/100
Daily_model_output$Yield=Daily_model_output$Yield/100
End_of_season_model_output$Sim_biom = End_of_season_model_output$Sim_biom/100
End_of_season_model_output$Sim_yield = End_of_season_model_output$Sim_yield/100

# Change names of simulated biomass and yield  
colnames(End_of_season_model_output)[colnames(End_of_season_model_output) == 'Sim_biom'] <- 'pred_biom'
colnames(End_of_season_model_output)[colnames(End_of_season_model_output) == 'Sim_yield'] <- 'pred_yield'

# Change names of observed biomass and yield  
colnames(End_of_season_crop_data)[colnames(End_of_season_crop_data) == 'max_biom'] <- 'obs_biom'
colnames(End_of_season_crop_data)[colnames(End_of_season_crop_data) == 'max_yield'] <- 'obs_yield'

## Change column names of Within_season_crop_data matrix to match Daily_model_output matrix
names(Within_season_crop_data)[names(Within_season_crop_data) == 'DAE'] <- 'Day'
names(Within_season_crop_data)[names(Within_season_crop_data) == 'Treatments'] <- 'Treatment'

# Calculate metrics (R2, RMSE, NRMSE, MBE) for each GLAM-Parti run (i.e. for every sample in every fraction of training treatments and for both ML models) 
Metrics_table_eos <- Metrics_table_ws <- NULL
for (iexp in 1:length(unique(End_of_season_model_output$No_exper))){ # for all numbers of training treatments (3-9)
  train_exp <- unique(End_of_season_model_output$No_exper)[iexp]
  
  for (imodel in 1:length(ML_models_table)){ # for both ML models (RF and XGBoost)
    ML_mdl <- ML_models_table[imodel]
    
    for (irep in 1:length(unique(End_of_season_model_output$Repetition))){ # For all samples (1-10)
      repetit <- unique(End_of_season_model_output$Repetition)[irep]
      
      Obs_sim_end_of_season_all_treat <- NULL
      Daily_output_biom_yield_all_treats <- NULL
      for (itreat_var in 1:length(All_treat_names)){ # For each crop treatment, subset model output and if it was not used for training then compare to observations
        treatm <- All_treat_names[itreat_var]
        
        ### Table of daily simulations for given treatment used for within-season biomass and yield evaluation  
        Sim_daily_output_treat <- subset(Daily_model_output, Daily_model_output$Treatment_name==treatm & Daily_model_output$Repetition== repetit & 
                                           Daily_model_output$No_exper==train_exp & Daily_model_output$ML_model==ML_mdl) 
        colnames(Sim_daily_output_treat)[colnames(Sim_daily_output_treat) == 'Biomass'] <- 'pred_biom'
        colnames(Sim_daily_output_treat)[colnames(Sim_daily_output_treat) == 'Yield'] <- 'pred_yield'
        
        ## Table of within-season measurement of biomass and yield for given treatment
        Within_season_crop_data_treat <- subset(Within_season_crop_data, Within_season_crop_data$Treatment_name==treatm) 
        Within_season_crop_data_treat <- Within_season_crop_data_treat[!is.na(Within_season_crop_data_treat$yield_season), ]
        Within_season_crop_data_treat_sorted <- Within_season_crop_data_treat[order(-Within_season_crop_data_treat$Day) , ]
        Within_season_crop_data_treat_f <- Within_season_crop_data_treat_sorted[-1,]
        colnames(Within_season_crop_data_treat_f)[colnames(Within_season_crop_data_treat_f) == 'biom_season'] <- 'obs_biom'
        colnames(Within_season_crop_data_treat_f)[colnames(Within_season_crop_data_treat_f) == 'yield_season'] <- 'obs_yield'
        
        ### Table of end-of-season simulations for model evaluation
        # Subset simulated variables for the particular treatment
        Sim_end_season_output_treat <- subset(End_of_season_model_output, End_of_season_model_output$Treatment_name==treatm & End_of_season_model_output$Repetition== repetit &
                                                End_of_season_model_output$No_exper==train_exp & End_of_season_model_output$ML_model==ML_mdl)
        
        # See if the treatment belongs to training or testing period (we calculate evaluation metrics only for treatments not in the ML training process)
        train_treats <- unlist(strsplit(as.character(Sim_end_season_output_treat$Training_treatments[1]),","))
        Eval_treats <- setdiff(All_treat_names, train_treats)
        
        
        if (treatm %in% Eval_treats){ # If the treatment was not used for ML training in the specific sample, then use for model evalaution
          
          ####### For evaluation of within-season predictions of biomass and yield 
          ### Merge observed and modelled biomass and yield
          Daily_output_treat_obs_sim <- merge(Within_season_crop_data_treat_f, Sim_daily_output_treat, by = c('Day', 'Treatment_name', 'Treatment_number'))
          
          ### Add observed and modelled biomass and yield of the given treatment to the matrix with all treatments
          Daily_output_biom_yield_all_treats <- rbind(Daily_output_biom_yield_all_treats, Daily_output_treat_obs_sim)
          
          ### For evaluation of end-of-season predictions of biomass, yield, anthesis and maturity 
          #Subset observations only for given treatment
          Obs_end_of_season_treat <- subset(End_of_season_crop_data, End_of_season_crop_data$Treatment_name==treatm)
          
          # Merge observed and simulated variables for the specific treatment
          Obs_sim_end_of_season_treat <- merge(Obs_end_of_season_treat, Sim_end_season_output_treat, by = 'Treatment_name')
          
          # Add observed and simulated variables of the given treatment to the table with all other treatments
          Obs_sim_end_of_season_all_treat <- rbind(Obs_sim_end_of_season_all_treat, Obs_sim_end_of_season_treat)
        }
      }
      
      ####### Calculate evaluation metrics 
      #### For end-of-season biomass, grain yield, anthesis and maturity
      # R2
      Biom_R2_eos <- summary(lm(obs_biom ~ pred_biom, data=Obs_sim_end_of_season_all_treat))$r.squared
      Yield_R2_eos <- summary(lm(obs_yield ~ pred_yield, data=Obs_sim_end_of_season_all_treat))$r.squared
      Anth_R2_eos <- summary(lm(Emer_Anth_days ~ Sim_Emer_Anth_days, data=Obs_sim_end_of_season_all_treat))$r.squared
      Matur_R2_eos <- summary(lm(Emer_Matur_days ~ Sim_Emer_Matur_days, data=Obs_sim_end_of_season_all_treat))$r.squared
      
      #RMSE
      Biom_RMSE_eos <- RMSE(Obs_sim_end_of_season_all_treat$obs_biom, Obs_sim_end_of_season_all_treat$pred_biom)
      Yield_RMSE_eos <- RMSE(Obs_sim_end_of_season_all_treat$obs_yield, Obs_sim_end_of_season_all_treat$pred_yield)
      Anth_RMSE_eos <- RMSE(Obs_sim_end_of_season_all_treat$Emer_Anth_days, Obs_sim_end_of_season_all_treat$Sim_Emer_Anth_days)
      Matur_RMSE_eos <- RMSE(Obs_sim_end_of_season_all_treat$Emer_Matur_days, Obs_sim_end_of_season_all_treat$Sim_Emer_Matur_days)
      
      #nRMSE
      Biom_NRMSE_eos <- Biom_RMSE_eos / mean(Obs_sim_end_of_season_all_treat$obs_biom)*100
      Yield_NRMSE_eos <- Yield_RMSE_eos / mean(Obs_sim_end_of_season_all_treat$obs_yield)*100
      Anth_NRMSE_eos <- Anth_RMSE_eos  / mean(Obs_sim_end_of_season_all_treat$Emer_Anth_days)*100
      Matur_NRMSE_eos <- Matur_RMSE_eos / mean(Obs_sim_end_of_season_all_treat$Emer_Matur_days)*100
      
      # MBE
      Biom_MBE_eos <- as.numeric(tdStats(Obs_sim_end_of_season_all_treat$pred_biom, Obs_sim_end_of_season_all_treat$obs_biom,
                                         functions = c("mbe")))
      Yield_MBE_eos <- as.numeric(tdStats(Obs_sim_end_of_season_all_treat$pred_yield, Obs_sim_end_of_season_all_treat$obs_yield,
                                          functions = c("mbe")))
      Anth_MBE_eos <- as.numeric(tdStats(Obs_sim_end_of_season_all_treat$Sim_Emer_Anth_days, Obs_sim_end_of_season_all_treat$Emer_Anth_days,
                                         functions = c("mbe")))
      Matur_MBE_eos <- as.numeric(tdStats(Obs_sim_end_of_season_all_treat$Sim_Emer_Matur_days, Obs_sim_end_of_season_all_treat$Emer_Matur_days,
                                          functions = c("mbe")))
      
      ####### Evaluation metrics for mid-season biomass and grain yield
      ### R2
      Biom_R2_ws <- summary(lm(pred_biom ~ obs_biom, data=Daily_output_biom_yield_all_treats))$r.squared
      Yield_R2_ws <- summary(lm(pred_yield ~ obs_yield, data=Daily_output_biom_yield_all_treats))$r.squared
      
      ####RMSE
      Biom_RMSE_ws <- RMSE(Daily_output_biom_yield_all_treats$obs_biom, Daily_output_biom_yield_all_treats$pred_biom)
      Yield_RMSE_ws <- RMSE(Daily_output_biom_yield_all_treats$obs_yield, Daily_output_biom_yield_all_treats$pred_yield)
      
      ####nRMSE
      Biom_NRMSE_ws <- Biom_RMSE_ws / mean(Daily_output_biom_yield_all_treats$obs_biom)*100
      Yield_NRMSE_ws <- Yield_RMSE_ws / mean(Daily_output_biom_yield_all_treats$obs_yield)*100
      
      #### MBE
      Biom_MBE_ws <- as.numeric(tdStats(Daily_output_biom_yield_all_treats$pred_biom, Daily_output_biom_yield_all_treats$obs_biom,
                                        functions = c("mbe")))
      Yield_MBE_ws <- as.numeric(tdStats(Daily_output_biom_yield_all_treats$pred_yield, Daily_output_biom_yield_all_treats$obs_yield,
                                         functions = c("mbe")))
      
      # All metrics in table (Metrics_table_eos is table for end-of-season variables - Metrics_table_ws is table for within-season variables)
      Metrics_table_eos <- rbind(Metrics_table_eos, data.frame(Biom_R2_eos, Yield_R2_eos, Anth_R2_eos, Matur_R2_eos, 
                                                               Biom_RMSE_eos, Yield_RMSE_eos, Anth_RMSE_eos, Matur_RMSE_eos,
                                                               Biom_NRMSE_eos, Yield_NRMSE_eos, Anth_NRMSE_eos, Matur_NRMSE_eos,
                                                               Biom_MBE_eos, Yield_MBE_eos, Anth_MBE_eos, Matur_MBE_eos,
                                                               train_exp, repetit, ML_mdl))
      
      Metrics_table_ws <- rbind(Metrics_table_ws, data.frame(Biom_R2_ws, Yield_R2_ws, Biom_RMSE_ws, Yield_RMSE_ws,
                                                             Biom_NRMSE_ws, Yield_NRMSE_ws, Biom_MBE_ws, Yield_MBE_ws, 
                                                             train_exp, repetit, ML_mdl))
      
    }
  }
}

# Change column to factor for plotting
Metrics_table_eos$train_exp <- as.factor(Metrics_table_eos$train_exp)

######### Plot GLAM-Parti skill with RF and XGboost against observed end-of-season biomass, yield, anthesis and maturity 
# Prepare metric table for plotting
Metrics_table_eos_f <- melt(Metrics_table_eos, c('train_exp', 'repetit', 'ML_mdl'))
Metrics_table_eos_f$Metric <- as.factor(rep(c('Rsq', 'RMSE', 'nRMSE(%)', 'MBE'), each=4*length(Metrics_table_eos_f$variable)/16))
Metrics_table_eos_f$variable <- rep(c('Biomass', 'Yield', 'Anthesis', 'Maturity'), each=length(Metrics_table_eos_f$variable)/16)
Metrics_table_eos_f$variable <- as.factor(Metrics_table_eos_f$variable)

# Relevel for plotting
Metrics_table_eos_f$variable <- factor(Metrics_table_eos_f$variable, levels = c("Biomass", "Yield", "Anthesis", 'Maturity'))
Metrics_table_eos_f$Metric <- factor(Metrics_table_eos_f$Metric, levels = c("Rsq", "RMSE", "nRMSE(%)", 'MBE'))

# For horizontal line in plot of MBE
Metrics_table_eos_MBE <- subset(Metrics_table_eos_f, Metrics_table_eos_f$Metric=='MBE')

#### Plot all metrics of all variables
metrics_eos_boxplot <- ggplot(Metrics_table_eos_f, aes(train_exp, value, fill = ML_mdl)) +
  geom_boxplot() + xlab('Number of experiments used for ML training') +
  #  stat_summary(fun=mean, geom="line", aes(group=ML_mdl, col=ML_mdl), show.legend = FALSE, size=0.9) +
  geom_hline(data = Metrics_table_eos_MBE, aes(yintercept = 0)) +
  theme(text = element_text(size=26),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=26),
        axis.title.x = element_text(size = 26)) +
  facet_wrap(variable ~ Metric, scales = 'free', ncol = 4) +
  theme(axis.title.y = element_blank()) +
  scale_fill_discrete(name = "Model", labels = c("Random forests", "XGboost"))

# Export Fig. 4 of GLAM-Parti paper to file 
ggsave('./Results/Figure4.pdf', metrics_eos_boxplot, width = 1920/72, height = 1580/72, dpi = 72)

#ggplot(Metrics_table_eos_f, aes(train_exp, value, fill = ML_mdl)) +
#  geom_boxplot() + xlab('Number of experiments used for training') +
#  #stat_summary(fun=mean, geom="line", aes(group=ML_mdl, col=ML_mdl), show.legend = FALSE, size=0.9) +
#  geom_hline(data = Metrics_table_eos_MBE, aes(yintercept = 0)) +
#  facet_wrap(variable ~ Metric, scales = 'free', ncol = 4) +
#  theme(axis.title.y = element_blank()) +
#  scale_fill_discrete(name = "Model", labels = c("Random forests", "XGboost"))

######### Create tables with summary statistics (Min., 1st Qu., Median, Mean, 3rd Qu., Max.) of each evaluation metric (R2, RMSE, nRMSE, MBE) for each variable (biomass, yield, anthesis, maturity)  
######### The derived tables are the tables S1-S4 of the GLAM-Parti/ML paper and their graphical illustration are the boxplots of Fig. 4 of the same paper

for (ivar in 1:4){  # For all output variables (biomass, yield, anthesis, maturity)
  Variab <- levels(Metrics_table_eos_f$variable)[ivar]
  All_metrics_and_ML_models_single_variable_table <- subset(Metrics_table_eos_f, Metrics_table_eos_f$variable == Variab)
  
  Summary_all_metrics_single_variable_both_ML_models <- NULL
  for (imetric in 1:4){ # For all evaluation metrics (R2, RMSE, nRMSE, MBE)
    Metr <- levels(All_metrics_and_ML_models_single_variable_table$Metric)[imetric]
    Single_metric_both_ML_models_single_variable_table <- subset(All_metrics_and_ML_models_single_variable_table, 
                                                                 All_metrics_and_ML_models_single_variable_table$Metric == Metr)
    
    Summary_single_metric_single_variable_RF <- Summary_single_metric_single_variable_XGB <- 
      Summary_single_metric_single_variable_both_ML_models <- NULL
    
    for (imodel in 1:2){ # For each ML model (RF and XGBoost)
      ML_model <- ML_models_table[imodel]
      Single_metric_single_ML_model_single_variable_table <- subset(Single_metric_both_ML_models_single_variable_table, 
                                                                    Single_metric_both_ML_models_single_variable_table$ML_mdl == ML_model)
      
      
      # Calculate summary statistics (Min., 1st Qu., Median, Mean, 3rd Qu., Max.) of each evaluation metric (R2, RMSE, nRMSE, MBE) and each ML model
      Summary_single_metric_single_ML_model_single_variable <- tapply(Single_metric_single_ML_model_single_variable_table$value, Single_metric_single_ML_model_single_variable_table$train_exp,
                                                                      function(x) format(summary(x), scientific = TRUE))
      
      # Convert from list to dataframe and leave only two decimal places
      Summary_single_metric_single_ML_model_single_variable_2 <- round(data.frame(t(sapply(Summary_single_metric_single_ML_model_single_variable, function(x) as.numeric(as.character(x))))), 2)
      
      # Save values of evaluation metrics in different tables for Random Forest and XGBoost 
      if (ML_model == 'Random_forests'){
        Summary_single_metric_single_variable_RF <- data.frame(Summary_single_metric_single_ML_model_single_variable_2, 'RF')
        names(Summary_single_metric_single_variable_RF)[ncol(Summary_single_metric_single_variable_RF)] <- c('Model')
      }else{
        Summary_single_metric_single_variable_XGB <- data.frame(Summary_single_metric_single_ML_model_single_variable_2, 'XGBoost')
        names(Summary_single_metric_single_variable_XGB)[ncol(Summary_single_metric_single_variable_XGB)] <- c('Model')
      }
    }
    # Paste evaluation metrics values of Random Forest and XGBoost in the same cell
    for (ipaste in 1:ncol(Summary_single_metric_single_variable_RF)){
      paste_col <- paste(Summary_single_metric_single_variable_RF[, ipaste], '/', Summary_single_metric_single_variable_XGB[, ipaste])
      Summary_single_metric_single_variable_both_ML_models <- cbind(Summary_single_metric_single_variable_both_ML_models, paste_col)
    }
    # Row-bind the values of the four evaluation metrics (R2, RMSE, nRMSE, MBE) for each variable
    Summary_all_metrics_single_variable_both_ML_models <- rbind(Summary_all_metrics_single_variable_both_ML_models, Summary_single_metric_single_variable_both_ML_models)
    
    # Export table with evaluation metrics for given variable (biomass, yield, anthesis, maturity) in the latex format (the 4 tables exported correspond to S1-S4 of the GLAM-Parti/ML paper) 
    latex_table_name <- paste('./Results/', 'TableS',ivar+1, sep='')
    print(xtable(Summary_all_metrics_single_variable_both_ML_models, type = "latex"), file = latex_table_name, row.names=F)
  }
} 

########## Plot GLAM-Parti evaluation with RF and XGboost against observed within-season variables 
#### Prepare metric table for plotting
Metrics_table_ws$train_exp <- as.factor(Metrics_table_ws$train_exp)
Metrics_table_ws_f <- melt(Metrics_table_ws, c('train_exp', 'repetit', 'ML_mdl'))
Metrics_table_ws_f$Metric <- as.factor(rep(c('Rsq', 'RMSE', 'nRMSE(%)', 'MBE'), each=2*length(Metrics_table_ws_f$variable)/8))
Metrics_table_ws_f$variable <- rep(c('Biomass', 'Grain mass'), each=length(Metrics_table_ws_f$variable)/8)
Metrics_table_ws_f$variable <- as.factor(Metrics_table_ws_f$variable)

#### Relevel for plotting
Metrics_table_ws_f$Metric <- factor(Metrics_table_ws_f$Metric, levels = c("Rsq", "RMSE", "nRMSE(%)", 'MBE'))

#### For horizontal line in plot of MBE
Metrics_table_ws_MBE <- subset(Metrics_table_ws_f, Metrics_table_ws_f$Metric=='MBE')

#### Plot all metrics of all variables
metrics_ws_boxplot <- ggplot(Metrics_table_ws_f, aes(train_exp, value, fill = ML_mdl)) +
  geom_boxplot() + xlab('Number of experiments used for ML training') +
  geom_hline(data = Metrics_table_ws_MBE, aes(yintercept = 0)) +
  theme(text = element_text(size=26),
        legend.title=element_text(size=26), 
        legend.text=element_text(size=26),
        axis.title.x = element_text(size = 26)) +
  facet_wrap(variable ~ Metric, scales = 'free', ncol = 4) +
  theme(axis.title.y = element_blank()) +
  scale_fill_discrete(name = "Model", labels = c("Random forests", "XGboost"))

### Export Fig. 4 of GLAM-Parti paper to file 
ggsave('./Results/FigureS4.pdf', metrics_ws_boxplot, width = 1320/72, height = 1080/72, dpi = 72)

#ggplot(Metrics_table_ws_f, aes(train_exp, value, fill = ML_mdl)) +
#  geom_boxplot() + xlab('Number of experiments used for training') +
#  geom_hline(data = Metrics_table_ws_MBE, aes(yintercept = 0)) +
#  facet_wrap(variable ~ Metric, scales = 'free', ncol = 4) +
#  theme(axis.title.y = element_blank()) +
#  scale_fill_discrete(name = "Model", labels = c("Random forests", "XGboost"))
