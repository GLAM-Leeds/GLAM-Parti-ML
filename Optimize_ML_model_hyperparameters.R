######## Optimize hyperparameters in ML models
# 10-fold cross-validation
set.seed(1234)
ncores <- 4
folds <- vfold_cv(ML_table_for_opt, v=10, strata = as.name(target_variable)) 

if (ML_model == 'Random_forests'){
  # Define hyperparameters that will be tuned in RF model
  RF_hyper_tune <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>% 
    set_engine("ranger") %>% 
    set_mode(mode_ML)
  
  # Set seed for reproduction of runs and define workflow
  set.seed(2345)
  rf_tune_wf <- workflow() %>% 
    add_model(RF_hyper_tune) %>% 
    add_formula(form)
  
  # Command to do parallel programming
#  doParallel::registerDoParallel(ncores)
  
  # Optimize ML parameters with Bayesian Optimization (UCB) method
  RF_tuned_UCB <- rf_tune_wf %>% 
    tune_bayes(resamples = folds,
               param_info=parameters(mtry(range = c(1, upper_range_mtry)),
                                     trees(range = c(1, 500)),
                                     min_n(range = c(1, 10)))) 
#               control = control_bayes(verbose = TRUE))
  
#  doParallel::stopImplicitCluster()
  
  # Store otpimal values of ML hyperparameters
  rf_hyperparam_table <- RF_tuned_UCB%>% show_best(eval_metric)
  rf_mtry <- rf_hyperparam_table$mtry[1]
  rf_trees <- rf_hyperparam_table$trees[1]
  rf_min_n <- rf_hyperparam_table$min_n[1]
  
  # Save all hyperparameters in table
  rf_best_hyperparams_table <- rbind(rf_best_hyperparams_table, 
                                      data.frame(rf_hyperparam_table[1,], algor_for_opt))
  
} else if (ML_model == 'XG_boost'){

  ## Define hyperparameters that will be tuned in XGB model
  xgb_hyper_tune <- boost_tree(trees = tune(), mtry = tune(), tree_depth = tune()) %>% 
    set_engine("xgboost") %>% 
    set_mode(mode_ML)
  
  ## Set seed for reproduction of runs and define workflow
  set.seed(2345)
  xgb_tune_wf <- workflow() %>% 
    add_model(xgb_hyper_tune) %>% 
    add_formula(form)  
  
  ## Command to do parallel programming
#  doParallel::registerDoParallel(ncores)
  
  ## Optimize ML parameters with Bayesian Optimization (UCB) method
  xgb_tuned_UCB <- xgb_tune_wf %>% 
    tune_bayes(resamples = folds,
               param_info=parameters(
               trees(range = c(1, 200)),
               mtry(range = c(1, upper_range_mtry)),
            tree_depth(range = c(1, 30))))

#  doParallel::stopImplicitCluster()
  
  ## Store otpimal values of ML hyperparameters
  xgb_best_hyperparams <- select_best(xgb_tuned_UCB, eval_metric)
  xgb_mtry <- xgb_best_hyperparams$mtry
  xgb_tree_depth <- xgb_best_hyperparams$tree_depth
  xgb_trees <- xgb_best_hyperparams$trees
  
  ## Save all hyperparameters in table
  xgb_best_hyperparams_table <- rbind(xgb_best_hyperparams_table, 
                                      data.frame(xgb_best_hyperparams, algor_for_opt))
}
