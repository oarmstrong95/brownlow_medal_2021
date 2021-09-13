#------------------------------------------------------------------------------
# GLOBAL INPUTS
#------------------------------------------------------------------------------
# myCluster <- parallel::makeCluster(2, setup_strategy = "sequential")
# registerDoParallel(myCluster)

#------------------------------------------------------------------------------
# RANDOM FOREST MODEL
#------------------------------------------------------------------------------
# Create a recipe for the model to downsample
first_ranger_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name,
          disposals, aflvotes, game_outcome) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # down sample
  step_nearmiss(brownlow_votes, under_ratio = 1, seed = 345) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors())

# Get the data out after down sampling by numeric features
downsample_data <- prep(first_ranger_recipe) %>%
  juice() %>%
  left_join(model_data)

# Create final recipe for the model
ranger_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = downsample_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name,
          disposals, aflvotes, game_outcome) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
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

# Set up a grid for the tuning process
rf_grid <- grid_regular(
  # The number of predictors that will be randomly sampled at
  # each split when creating the tree models.
  mtry(range = c(5, 31)),
  # The minimum number of data points in a node that are
  # required for the node to be split further
  min_n(range = c(3, 7)),
  levels = 10
)

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
            grid = rf_grid,
            control = control_resamples(save_pred = TRUE)
            )
