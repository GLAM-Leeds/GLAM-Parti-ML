#### Options in ML hyperparameter tuning according to which of the three algorithms (iphen, RUE, dHI/dt) we're optimizing
if (algor_for_opt == 'Phenology'){
  ML_table_for_opt <- Weather_ML_table_for_phen_f
  target_variable <- names(ML_table_for_opt)[ncol(ML_table_for_opt)]
  mode_ML <- 'classification'  
  eval_metric <- 'roc_auc'
  form <- iphen~.
  upper_range_mtry = length(iphen_inputs) - 1
  
}else if(algor_for_opt == 'RUE'){
  ML_table_for_opt <- Weather_ML_table_for_RUE_f
  target_variable <- names(ML_table_for_opt)[ncol(ML_table_for_opt)]
  mode_ML <- 'regression'  
  eval_metric <- 'rmse'
  form <- RUE~.
  upper_range_mtry = length(RUE_inputs) - 1
  
}else if(algor_for_opt == 'dhdt'){
  ML_table_for_opt <- Weather_ML_table_for_HI_f
  target_variable <- names(ML_table_for_opt)[ncol(ML_table_for_opt)]
  mode_ML <- 'regression'  
  eval_metric <- 'rmse'
  form <- dhdt~.
  upper_range_mtry = length(dhdt_inputs) - 1
}
