############### Evaluate the GLAM-Parti model skill 
table_for_p1_all <- NULL
# Merge tables of observations and simulations
Obs_Sim_end_of_season_output <- left_join(End_of_season_crop_data, End_of_season_model_output, by=c('Treatment_number', 'Treatment_name'))
Obs_Sim_end_of_season_output$Sim_biom <- Obs_Sim_end_of_season_output$Sim_biom/100
Obs_Sim_end_of_season_output$Sim_yield <- Obs_Sim_end_of_season_output$Sim_yield/100

# Remove treatments that were not simulated by the model
Obs_Sim_end_of_season_output_f <- Obs_Sim_end_of_season_output %>% drop_na(Sim_yield)

# Add new column to indicate if treatment was used in ML training or not
Obs_Sim_end_of_season_output_f$Period <- ifelse(Obs_Sim_end_of_season_output_f$Treatment_number %in% Training_treat_f, 'Training', 'Evaluation')

# Define expressions for x and y label of ggplots
expressions_for_ggplot1a <- c(expression(paste("Observed Biomass", ~ (t~ ha^-1), sep="")), expression(paste("Observed Yield", ~ (t~ ha^-1), sep="")),
                                        expression(paste("Observed time to anthesis (Days)")), expression(paste("Observed time to maturity (Days)")))
                              
expressions_for_ggplot1b <- c(expression(paste("Predicted Biomass", ~ (t~ ha^-1), sep="")), expression(paste("Predicted Yield", ~ (t~ ha^-1), sep="")),
                                        expression(paste("Predicted time to anthesis (Days)")), expression(paste("Predicted time to maturity (Days)")))

expressions_for_ggplot2 <- c(expression(paste("Biomass", ~ (t~ ha^-1), sep="")), expression(paste("Grain yield", ~ (t~ ha^-1), sep="")),
                            'Emergence to anthesis (days)', 'Emergence to maturity (days)')

#### Evaluation metrics for each variable
# Define variables for evaluation
Obs_vars_list <- c('biom', 'yield', 'Emer_Anth_days', 'Emer_Matur_days')
Sim_vars_list <- c('Sim_biom', 'Sim_yield', 'Sim_Emer_Anth_days', 'Sim_Emer_Matur_days')

# Calculate evaluation metrics
plot_list1 = plot_list2 = list()
for (ivar_eval in 1:length(Obs_vars_list)){
  obs_var_name <- Obs_vars_list[ivar_eval]
  obs_var_name_f <- ifelse(obs_var_name=='biom' | obs_var_name=='yield', paste('max_', obs_var_name, sep=''), obs_var_name)
  sim_var_name <- Sim_vars_list[ivar_eval]

  # Restructure table to use in ggplot
  Obs_Sim_end_of_season_var_table <-  melt(Obs_Sim_end_of_season_output_f, id.vars = c("Treatment_number", "Period"),
                                           measure.vars = c(obs_var_name_f, sim_var_name))
  
  # Add method column ('observed' and 'predicted') for ggplot
  Obs_Sim_end_of_season_var_table$Method <- ifelse(Obs_Sim_end_of_season_var_table$variable==obs_var_name_f, 'Observed', 'Predicted')
  
  # Calculate evaluation metrics only for treatments in the evaluation period
  obs_var <- Obs_Sim_end_of_season_var_table$value[Obs_Sim_end_of_season_var_table$variable== obs_var_name_f & 
                                                     Obs_Sim_end_of_season_var_table$Period=='Evaluation']
  
  sim_var <- Obs_Sim_end_of_season_var_table$value[Obs_Sim_end_of_season_var_table$variable== sim_var_name & 
                                                     Obs_Sim_end_of_season_var_table$Period=='Evaluation']
  
  ### Make barplots
  # Add metrics on top right corner and make plot
  text = paste(paste("Rsq=", round(summary(lm(obs_var~sim_var))$r.squared, 2), sep=''),
               paste("RMSE=", round(RMSE(sim_var, obs_var), 2), sep=''),
               paste("nRMSE(%)=", round(RMSE(sim_var, obs_var)/mean(obs_var)*100, 2), sep=''),
               paste("MBE=", round(tdStats(sim_var, obs_var, functions = c("mbe")), 2), sep=''), 
               sep = "\n")
  
  grob1 <- grobTree(textGrob(text, x=0.05,  y=0.85, hjust=0,
                            gp=gpar(col="black", fontsize=9)))
  
  # Add standard error to table for plotting
  if (obs_var_name=='biom' | obs_var_name == 'yield'){
    Obs_Sim_end_of_season_var_table_with_se <- left_join(Obs_Sim_end_of_season_var_table, Obs_Sim_end_of_season_output_f %>% select(Treatment_number, Treatment_name, paste('sd_', obs_var_name, sep='')), by='Treatment_number') 
    names(Obs_Sim_end_of_season_var_table_with_se)[length(names(Obs_Sim_end_of_season_var_table_with_se))]<-"sd_var"
    Obs_Sim_end_of_season_var_table_with_se$sd_var[Obs_Sim_end_of_season_var_table_with_se$variable==sim_var_name] <- NA
    
  }else{
    Obs_Sim_end_of_season_var_table_with_se <- left_join(Obs_Sim_end_of_season_var_table, Obs_Sim_end_of_season_output_f %>% select(Treatment_number, Treatment_name), by='Treatment_number') 
    Obs_Sim_end_of_season_var_table_with_se$sd_var <- NA
  }
  
  Obs_Sim_end_of_season_var_table_with_se$Treatment_number <- factor(Obs_Sim_end_of_season_var_table_with_se$Treatment_number, 
                                                                   levels=c(End_of_season_crop_data$Treatment_number[Training_treat_f], 
                                                                            End_of_season_crop_data$Treatment_number[setdiff(All_treat, Training_treat_f)]))
  
  #### Make observed vs. simulated barplots 
  # Observed vs simulated plot
  xaxis_p1 <- Obs_Sim_end_of_season_var_table_with_se$value[Obs_Sim_end_of_season_var_table_with_se$Method=='Predicted']
  yaxis_p1 <- Obs_Sim_end_of_season_var_table_with_se$value[Obs_Sim_end_of_season_var_table_with_se$Method=='Observed']
   
  table_for_p1 <- data.frame(xaxis_p1, yaxis_p1, Obs_Sim_end_of_season_output_f[,c(3, ncol(Obs_Sim_end_of_season_output_f)-1, ncol(Obs_Sim_end_of_season_output_f))]) 
  table_for_p1$Period <- factor(table_for_p1$Period, levels = c("Training", "Evaluation"))
  
  # Add regression line to plots
  table_for_regr <- subset(table_for_p1, table_for_p1$Period=='Evaluation')
  inter <- coef(lm(yaxis_p1~xaxis_p1, data = table_for_regr))[1]
  slop <- coef(lm(yaxis_p1~xaxis_p1, data = table_for_regr))[2]
  
  plot_list1[[ivar_eval]] <- ggplot(data = table_for_p1, mapping = aes(x=xaxis_p1, y=yaxis_p1, col=Period)) +
    geom_point(aes(shape=Location), size=2.5) + 
    scale_color_manual(values=c("grey60", "black"), guide = "none") + 
    labs(y=expressions_for_ggplot1a[ivar_eval]) + labs(x=expressions_for_ggplot1b[ivar_eval]) + 
    geom_abline(linetype='dashed') + geom_abline(slope = slop, intercept = inter) +
    theme_bw() +
    theme(legend.title = element_blank(), legend.text=element_text(size=14), 
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +      
    annotation_custom(grob1) 
  
  #### Observed and simulated boxplot
  
  if (Training_dataset ==  'HSC'){

    grob2 <- grobTree(textGrob(text, x=0.75,  y=0.9, hjust=0,
                               gp=gpar(col="black", fontsize=9)))

    plot_list2[[ivar_eval]] = ggplot(data = Obs_Sim_end_of_season_var_table_with_se, mapping = aes(x=Treatment_number, y=value, fill=Method)) +
      geom_bar(stat="identity", position = 'dodge')+  scale_fill_brewer(palette="Paired") +
      geom_vline(xintercept = no_experiments_ML_training + 0.5, col='red', lwd=1.5) +
      labs(y=expressions_for_ggplot2[ivar_eval]) +
      geom_errorbar(aes(ymin = value-sd_var, ymax = value+sd_var), 
                    data = Obs_Sim_end_of_season_var_table_with_se, width = 0.3, 
                    position = position_dodge(width = 0.9)) +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.text=element_text(size=14), axis.title.x=element_blank(), 
            axis.title.y = element_text(size = 14)) + 
      scale_x_discrete(labels= unique(Obs_Sim_end_of_season_var_table_with_se$Treatment_name)[as.numeric(levels(Obs_Sim_end_of_season_var_table_with_se$Treatment_number))])+
      annotation_custom(grob2) 
    
  }else{
    
    grob2 <- grobTree(textGrob(text, x=0.7,  y=0.9, hjust=0,
                               gp=gpar(col="black", fontsize=9)))
    
    plot_list2[[ivar_eval]] = ggplot(data = Obs_Sim_end_of_season_var_table_with_se, mapping = aes(x=Treatment_number, y=value, fill=Method)) +
      geom_bar(stat="identity", position = 'dodge')+  scale_fill_brewer(palette="Paired") +
      geom_vline(xintercept = no_experiments_ML_training + 0.5, col='red', lwd=1.5) +
      labs(y=expressions_for_ggplot2[ivar_eval]) +
      geom_errorbar(aes(ymin = value-sd_var, ymax = value+sd_var), 
                    data = Obs_Sim_end_of_season_var_table_with_se, width = 0.3, 
                    position = position_dodge(width = 0.9)) +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.text=element_text(size=14), axis.title.x=element_blank(), 
            axis.text.x = element_text(size=7, angle=45), axis.title.y = element_text(size = 14)) +      
      annotation_custom(grob2) 
  }
}

####### Merge plots
GParti_eval_with_RF_fig1 <- ggarrange(plotlist=plot_list1, 
                                     ncol=2, nrow=2, common.legend = TRUE, legend = 'bottom', 
                                     labels = c('A', 'B', 'C', 'D'), font.label = list(size = 14))

GParti_eval_with_RF_fig2 <- ggarrange(plotlist=plot_list2, 
                                     ncol=2, nrow=2, common.legend = TRUE, legend = 'bottom', 
                                     labels = c('A', 'B', 'C', 'D'), font.label = list(size = 14))
