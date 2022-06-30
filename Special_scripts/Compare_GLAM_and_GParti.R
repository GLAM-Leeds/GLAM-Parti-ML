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
names(Daily_model_output) <- c('Treatment_name', 'Treatment_number', 'Day', 'Leaf_Ear_mass', 'Stem_mass', 'Biomass', 'Yield', 'HI', 'RUE', 'No_exper', 'Repetition', 'ML_model')
names(End_of_season_model_output) <- c('Treatment_name', 'Treatment_number', 'Leaf_Ear_mass', 'Stem_mass', 'Sim_biom', 'Sim_yield', 'Sim_HI', 'RUE', 'Sim_Emer_Anth_days', 'Sim_Emer_Matur_days', 'No_exper', 'Repetition', 'ML_model')

# Change GLAM-Parti biomass and yield from g/m2 to tn/ha 
End_of_season_model_output$Sim_biom <- End_of_season_model_output$Sim_biom/100
End_of_season_model_output$Sim_yield <- End_of_season_model_output$Sim_yield/100

# Import GLAM runs where the model was calibrated for the Maiorano et al., 2017 study 
glam_runs <- read.csv('./Special_scripts/GLAM_runs/HSC_AgMIP_summary_simulations.txt', sep='')

# Subset only the GLAM runs
glam_runs_f <- subset(glam_runs, glam_runs$Model=='GL' & glam_runs$Version=='Improved')

# Remove the runs where the crop died early
glam_runs_f <- glam_runs_f[!is.na(glam_runs_f$Biom.an),]

# Add a treatment name column identical to GLAM-Parti 
glam_runs_f$Treatment_name <- paste(glam_runs_f$PNO, glam_runs_f$Trt, sep='')

# Convert GLAM days of emergence, anthesis and maturity from dates to days of year
glam_runs_f$Emergence.date2 <- yday(glam_runs_f$Emergence.date)
glam_runs_f$Ant.date2 <- yday(glam_runs_f$Ant.date)
glam_runs_f$Mat.date2 <- yday(glam_runs_f$Mat.date)

# Calculate days from emergence to anthesis and maturity for GLAM
glam_runs_f$Ant.date2[2] <- glam_runs_f$Ant.date2[2] + 366
glam_runs_f$Ant.date2[3] <- glam_runs_f$Ant.date2[3] + 366
glam_runs_f$Mat.date2[2] <- glam_runs_f$Mat.date2[2] + 366
glam_runs_f$Mat.date2[3] <- glam_runs_f$Mat.date2[3] + 366
glam_runs_f$Emerg_anth_glam <- glam_runs_f$Ant.date2 - glam_runs_f$Emergence.date2 + 1
glam_runs_f$Emerg_matur_glam <- glam_runs_f$Mat.date2 - glam_runs_f$Emergence.date2 + 1

# Table with GLAM,  GLAM-Parti runs and observations 
df_list <- list(End_of_season_model_output, glam_runs_f, End_of_season_crop_data)
Obs_sim_table <- df_list %>% reduce(left_join, by='Treatment_name')

Obs_sim_table_f <- Obs_sim_table[,  c('Biom.ma', 'Sim_biom', 'max_biom', 'Yield', 'Sim_yield', 'max_yield', 
                                     'Emerg_anth_glam', 'Sim_Emer_Anth_days', 'Emer_Anth_days', 
                                     'Emerg_matur_glam', 'Sim_Emer_Matur_days', 'Emer_Matur_days')]

names(Obs_sim_table_f) <- c('GLAM_biomass',  'GParti_biomass', 'Obs_biomass', 'GLAM_yield', 'GParti_yield', 'Obs_yield',
                            'GLAM_anthesis', 'GParti_anthesis', 'Obs_anthesis', 'GLAM_maturity',  'GParti_maturity', 'Obs_maturity')

# Compute evaluation metrics (R2, RMSE, nRMSE, MBE) for GLAM and GLAM-Parti based on the observations of the HSC treatment
models <- c('GLAM', 'GLAM-Parti')
vars <- c('biomass', 'yield', 'anthesis', 'maturity')
Eval_table <- NULL
for (ivar in 1:length(vars)){
  Obs_sim_table_var <- Obs_sim_table_f %>% select(contains(vars[ivar]))
  for (imodel in 1:2){
    R2_var <- round(summary(lm(Obs_sim_table_var[, 3] ~ Obs_sim_table_var[,imodel]))$r.squared, 2)
    RMSE_var <- round(RMSE(Obs_sim_table_var[,imodel], Obs_sim_table_var[,3]), 2)
    nRMSE_var <- round(RMSE_var/mean(Obs_sim_table_var[,3]), 2)*100
    MBE_var <- round(tdStats(Obs_sim_table_var[,imodel], Obs_sim_table_var[,3], functions = c("mbe")), 2)
    Eval_table <- rbind(Eval_table, data.frame(R2_var, RMSE_var, nRMSE_var, MBE_var, models[imodel], vars[ivar]))
  }
}

names(Eval_table) <- c('R2', 'RMSE', 'nRMSE', 'MBE', 'Model', 'Variable')

# Export evaluation table for paper
Eval_table_GLAM <- subset(Eval_table, Eval_table$Model == 'GLAM')
Eval_table_GParti <- subset(Eval_table, Eval_table$Model == 'GLAM-Parti')

Eval_table_for_latex <- cbind(GLAM_R2=Eval_table_GLAM[, 1], GParti_R2 = Eval_table_GParti[, 1], GLAM_RMSE=Eval_table_GLAM[, 2], GParti_RMSE=Eval_table_GParti[, 2],
                              GLAM_nRMSE=Eval_table_GLAM[, 3], GParti_nRMSE=Eval_table_GParti[, 3], GLAM_MBE=Eval_table_GLAM[, 4], GParti_MBE=Eval_table_GParti[, 4])

print(xtable(Eval_table_for_latex, type = "latex"), file = "./Results/Table2.tex")


###Plot Predicted vs Observed variables for paper (Figure S5)
model_compare_plot_list <- NULL

xlab_expressions_for_plot <- c(expression(paste("Predicted Biomass", ~ (t~ ha^-1), sep="")), expression(paste("Predicted Yield", ~ (t~ ha^-1), sep="")),
                               'Predicted days to anthesis', 'Predicted days to maturity')

ylab_expressions_for_plot <- c(expression(paste("Observed Biomass", ~ (t~ ha^-1), sep="")), expression(paste("Observed Yield", ~ (t~ ha^-1), sep="")),
                               'Observed days to anthesis', 'Observed days to maturity')

j = 0
for (iplot in seq(1, 12, 3)){
  Obs_sim_table_for_plot <- Obs_sim_table_f[,c(iplot, iplot + 1, iplot+2)]
  Obs_sim_table_for_plot_f <- melt(Obs_sim_table_f[,c(iplot, iplot + 1, iplot+2)], id = colnames(Obs_sim_table_for_plot)[ncol(Obs_sim_table_for_plot)])
  colnames(Obs_sim_table_for_plot_f)[1] <- "Obs_var"
  
  j =j + 1
  model_compare_plot_list[[j]] <- Obs_sim_table_for_plot_f %>%
    ggplot(aes(x=value, y=Obs_var, color=variable))+
    geom_point() + geom_smooth(method="lm", se=F) +
    geom_abline(intercept = 0, slope = 1) +
    labs(y=ylab_expressions_for_plot[j], x=xlab_expressions_for_plot[j]) +
    scale_color_discrete(name="Model", labels=c("GLAM", "GLAM-Parti")) +
    theme(legend.text=element_text(size=12), legend.title = element_text(size=14), 
          axis.text.x = element_text(size = 14, colour="black"), axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 14, colour="black"), axis.title.y = element_text(size = 14)) +
    theme_bw()
}

GLAM_GParti_comparison <- ggarrange(plotlist=model_compare_plot_list, 
                            ncol=2, nrow=2, common.legend = TRUE, legend = 'right', 
                            labels = c('A', 'B', 'C', 'D'), font.label = list(size = 14))
ggsave('./Results/Figures/FigureS5.pdf', GLAM_GParti_comparison,  width = 10, height = 9, dpi = 300)

  





