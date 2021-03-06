# Global variables -----------------------------------------------------------------------------------------
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# Random forest model---------------------------------------------------------------------------------------
# Create a recipe for the model
ranger_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name,
          disposals) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # down sample
  step_downsample(brownlow_votes, under_ratio = 1, seed = 345) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors())

# Create a model specification
ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

# Create a work flow
ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec)

# Check the accuracy on the bootstrap samples to tune hyper parameters
set.seed(567)
ranger_tune <-
  tune_grid(ranger_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = 25,
            control = control_resamples(save_pred = TRUE)
            )

# Select best model
ranger_best <- ranger_tune %>%
  select_best("roc_auc")

# Fit on entire training data
ranger_fit <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(finalize_model(ranger_spec, ranger_best)) %>%
  last_fit(split, metrics = metric_set(roc_auc, accuracy, sensitivity, specificity))

# Check out of sample accuracy
ranger_metrics <- ranger_fit %>%
  collect_metrics()

# Check roc graphs
ranger_roc_curve <- ranger_fit %>%
  roc_curve_fun()

# Fit the final model
ranger_final_model <- workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(finalize_model(ranger_spec, ranger_best)) %>%
  fit(model_data)

# Generate the predictions
ranger_results <-
  predict(ranger_final_model, new_data = new_data, type = "prob") %>%
  rename_with( ~ paste0("ranger", .x))

# XGBoost model ------------------------------------------------------------------------------------------
# Create a recipe for the model
xgboost_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name,
          disposals) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # down sample
  step_downsample(brownlow_votes, under_ratio = 1, seed = 345) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_dummy(aflvotes, game_outcome) # xgboost cannot take factor variables

# Create a model specification
xgboost_spec <- 
  boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune(), trees = 1000) %>% 
  set_engine("xgboost") %>%
  set_mode("classification")

# Create a work flow
xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec)

# Check the accuracy on the bootstrap samples to tune hyper parameters
set.seed(678)
xgboost_tune <-
  tune_grid(xgboost_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = 25,
            control = control_resamples(save_pred = TRUE)
            )

# Select best model
xgboost_best <- xgboost_tune %>%
  select_best("roc_auc")

# Fit on entire training data
xgboost_fit <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(finalize_model(xgboost_spec, xgboost_best)) %>%
  last_fit(split, metrics = metric_set(roc_auc, accuracy, sensitivity, specificity))

# Check out of sample accuracy
xgboost_metrics <- xgboost_fit %>%
  collect_metrics()

# Check roc graphs
xgboost_roc_curve <- xgboost_fit %>%
  roc_curve_fun()

# Fit the final model
xgboost_final_model <- workflow() %>%
  add_recipe(xgboost_recipe) %>%
  add_model(finalize_model(xgboost_spec, xgboost_best)) %>%
  fit(model_data)

# Generate the predictions
xgboost_results <-
  predict(xgboost_final_model, new_data = new_data, type = "prob") %>%
  rename_with( ~ paste0("xgboost", .x))

# Neural net model -------------------------------------------------------------------------------------
# Recipe  for NN model
nn_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name,
          disposals) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # down sample
  step_downsample(brownlow_votes, under_ratio = 1, seed = 345) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors())

# Create nn specification
nn_spec <- mlp(mode = "classification", hidden_units = tune(), penalty = tune()) %>%
  set_engine("nnet")

# Create work flow for tuning the nn
nn_workflow <- workflow() %>%
  add_recipe(nn_recipe) %>%
  add_model(nn_spec)

# Check the accuracy on the bootstrap samples to tune hyper parameters
set.seed(789)
nn_tune <-
  tune_grid(nn_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = 25,
            control = control_resamples(save_pred = TRUE)
  )

# Select best model
nn_best <- nn_tune %>%
  select_best("roc_auc")

# Fit on entire training data
nn_fit <- 
  workflow() %>% 
  add_recipe(nn_recipe) %>% 
  add_model(finalize_model(nn_spec, nn_best)) %>%
  last_fit(split, metrics = metric_set(roc_auc, accuracy, sensitivity, specificity))

# Check out of sample accuracy
nn_metrics <- nn_fit %>%
  collect_metrics()

# Check roc graphs
nn_roc_curve <- nn_fit %>%
  roc_curve_fun()

# Fit the final model
nn_final_model <- workflow() %>%
  add_recipe(nn_recipe) %>%
  add_model(finalize_model(nn_spec, nn_best)) %>%
  fit(model_data)

# Generate the predictions
nn_results <-
  predict(nn_final_model, new_data = new_data, type = "prob") %>%
  rename_with( ~ paste0("nn", .x))



