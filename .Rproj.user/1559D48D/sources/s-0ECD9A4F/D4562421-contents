## Import data -----------------------------------------------------------------------------------------
import_data <- function(years) {
  
  "
  
  About:
    this function imports the afl player data from the fitzRoy package
  
  Input:
    integer:    number vector for the years
  
  Output:
    tibble:     player statistics for each game for each year
  
  "
  
  match_data <- tibble()
  
  for (i in 1:length(YEARS)){
    
    match_data <- match_data %>%
      bind_rows(fetch_player_stats(season = YEARS[i], source = "fryzigg"))
    
  }
  
  return(match_data)
  
}

### Clean data ------------------------------------------------------------------------------------------
clean_data <- function(data, afl_com_au_votes, coaches_votes, fantasy_scores,
                       brownlow_votes_2020) {
  
  data <- match_data
  
  "
  
  About:
    this function cleans the data and removes any missing observations
  
  Input:
    multiple tibbles:    player data as well as imported csv's
  
  Output:
    tibble:             data ready for model pipeline
  
  "
  
  # Define function to normalise data
  normalise_fun <- function(x){
    return((x-min(x)) / (max(x)-min(x)))
  }
  
  # Create a clean data set 
  df1 <- data %>%
    mutate(
      # create a season variable
      season = year(match_date),
      # create a player name as a concat of first and last name
      player_name = paste(player_first_name, player_last_name),
      # create a player game outcome variable
      game_outcome = if_else(match_winner == player_team, "Win", "Loss"),
      # calc the game margin
      game_margin = if_else(player_team == match_home_team, match_margin,
                            -match_margin)) %>%
    select(
      # select game identifiers
      season, match_round, match_id, match_date, match_home_team, 
      match_away_team, game_outcome, game_margin,
      # select player identifiers
      player_id, player_name, player_team,
      # select player stats
      kicks:spoils
    ) %>%
    # cast variables to correct data formats
    mutate(season = as.integer(season),
           match_round = as.integer(match_round),
           match_home_team = as.factor(match_home_team),
           match_away_team = as.factor(match_away_team),
           player_id = as.integer(player_id),
           match_round = as.integer(match_round),
           brownlow_votes = as.factor(brownlow_votes)
    ) %>%
    # only keep rounds in the home and away season
    filter(!is.na(match_round))
    
  # Create lag feature
  lag_brownlow_feature <- df1 %>%
    group_by(season, player_name, player_id) %>%
    summarize(total = sum(as.numeric(as.character(brownlow_votes))),
               n = n(),
               total = total / n) %>%
    ungroup() %>%
    select(-n) %>%
    arrange(player_name, player_id, season) %>%
    group_by(player_id) %>%
    mutate(lag_brownlow_votes = lag(total)) %>%
    ungroup() %>%
    filter(season >= 2017 & season <= 2021) %>%
    mutate(lag_brownlow_votes = if_else(is.na(lag_brownlow_votes), 0, lag_brownlow_votes)) %>%
    select(-total)
  
  output <- df1 %>%
    filter(season >= 2017) %>%
    left_join(lag_brownlow_feature) %>%
    # drop irrelevant variables
    select(-c(contest_def_losses, time_on_ground_percentage,
              behinds, bounces, free_kicks_against,
              free_kicks_for, tackles_inside_fifty,
              disposal_efficiency_percentage,
              intercept_marks, contested_marks,
              contest_off_one_on_ones, contest_def_one_on_ones,
              spoils, one_percenters, rebounds, intercepts,
              def_half_pressure_acts, contest_off_wins, marks_on_lead,
              goal_assists, f50_ground_ball_gets, goals, handballs,
              marks_inside_fifty, centre_clearances,
              score_launches)) %>%
    select(!contains("ruck")) %>%
    select(!contains("hitout")) %>%
    left_join(afl_com_au_votes) %>%
    left_join(coaches_votes) %>%
    left_join(fantasy_scores) %>%
    mutate(supercoach_score = as.numeric(supercoach_score),
           afl_fantasy_score = as.numeric(afl_fantasy_score)) %>%
    mutate(supercoach_score = if_else(is.na(supercoach_score), sc_new, supercoach_score),
           afl_fantasy_score = if_else(is.na(afl_fantasy_score), af_new, afl_fantasy_score)) %>%
    select(-af_new, -sc_new, -total) %>%
    drop_na() %>%
    left_join(brownlow_votes_2020) %>%
    mutate(brownlow_votes = as.numeric(as.character(brownlow_votes))) %>%
    mutate(brownlow_votes = if_else(season == 2020 & !is.na(votes), votes, brownlow_votes)) %>%
    select(-votes) %>%
    mutate(brownlow_votes = factor(brownlow_votes, levels = c(0, 1, 2, 3))) %>%
    mutate(aflvotes = factor(aflvotes, levels = c('L0', 'L1', 'L2', 'L3', 'W0', 'W1', 'W2', 'W3'))) %>%
    select(season:player_team, brownlow_votes, lag_brownlow_votes, aflvotes, everything()) %>%
    group_by(season, match_id) %>%
    mutate_at(vars(kicks:cc_votes), normalise_fun) %>%
    ungroup()
  
  return(output)
  
}

### Other functions ------------------------------------------------------------------------------------------
var_importance <- function(model = c('random_forest', 'xgboost')) {
  
  if (model == 'random_forest') {
    
    best <- ranger_tune %>%
      select_best("roc_auc")
    
    data_vip <- ranger_recipe %>% prep() %>% juice()
    
    graph <- finalize_model(ranger_spec, best) %>%
      set_engine("ranger", importance = "permutation") %>%
      fit(brownlow_votes ~ .,
          data = data_vip) %>%
      vip(geom = "point")
    
  } else {
    
    best <- xgboost_tune %>%
      select_best("roc_auc")
    
    data_vip <- xgboost_recipe %>% prep() %>% juice()
    
    graph <- finalize_model(xgboost_spec, best) %>%
      set_engine("xgboost", importance = "permutation") %>%
      fit(brownlow_votes ~ .,
          data = data_vip) %>%
      vip(geom = "point")
    
  }
  
  return(graph)
  
}

# Get accuracy on test set
out_of_sample_accuracy <- function(model = c('random_forest', 'xgboost')) {
  
  if (model == 'random_forest') {
  
  best <- ranger_tune %>%
    select_best("roc_auc")
  
  # Fit on entire training data
  ranger_test_check <- 
    workflow() %>% 
    add_recipe(ranger_recipe) %>% 
    add_model(finalize_model(ranger_spec, best)) %>%
    last_fit(split, metrics = metric_set(roc_auc, accuracy, sensitivity, specificity))
  
  # Check out of sample accuracy
  metrics <- ranger_test_check %>%
    collect_metrics()
  
  # Check roc graphs
  roc_curve <- ranger_test_check %>%
    roc_curve_fun()
  
  metrics <- list(metrics, roc_curve)
  
  } else {
    
    best <- xgboost_tune %>%
      select_best("roc_auc")
    
    # Fit on entire training data
    xgboost_test_check <- 
      workflow() %>% 
      add_recipe(xgboost_recipe) %>% 
      add_model(finalize_model(xgboost_spec, best)) %>%
      last_fit(split, metrics = metric_set(roc_auc, accuracy, sensitivity, specificity))
    
    # Check out of sample accuracy
    metrics <- xgboost_test_check %>%
      collect_metrics()
    
    # Check roc graphs
    roc_curve <- xgboost_test_check %>%
      roc_curve_fun()
    
    metrics <- list(metrics, roc_curve)
    
    
  }
  
  return(metrics)
  
}

# Define function to check tuning parameters to optimize the grid search
# on the second tuning iteration
tuning_parameters_fun <- function(){
  
  graph <- ranger_tune %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    select(mean, min_n, mtry) %>%
    pivot_longer(min_n:mtry,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "AUC")
  
  return(graph)
  
}


# Define function to check accuracy through roc curve
roc_curve_fun <- function(data) {
  
  curve <- data %>%
    collect_predictions() %>%
    group_by(id) %>%
    roc_curve(brownlow_votes, .pred_0:.pred_3) %>%
    ggplot(aes(1 - specificity, sensitivity, color = id)) +
    geom_abline(lty = 2, color = "gray80", size = 1.5) +
    geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
    facet_wrap(~.level, ncol = 5) +
    coord_equal()
  
  return(curve)
  
}


# Define function to get the predictions
predict_function <- function() {
  
  ### Random forest model
  
  # Select the best tuning parameters, optimizing the roc_auc
  ranger_best <- ranger_tune %>%
    select_best("roc_auc")
  
  # Fit the final model
  ranger_final_model <- workflow() %>%
    add_recipe(ranger_recipe) %>%
    add_model(finalize_model(ranger_spec, ranger_best)) %>%
    fit(model_data)
  
  # Generate the predictions
  ranger_results <-
    predict(ranger_final_model, new_data = new_data, type = "prob") %>%
    rename_with( ~ paste0("ranger_", .x))
  
  ### XGBoost model
  
  # Select the best tuning parameters, optimizing the roc_auc
  best_xgboost <- xgboost_tune %>%
    select_best("roc_auc")
  
  # Fit the final model
  xgboost_final_model <- workflow() %>%
    add_recipe(xgboost_recipe) %>%
    add_model(finalize_model(xgboost_spec, best_xgboost)) %>%
    fit(model_data)

  # Generate the predictions
  xgboost_results <-
    predict(xgboost_final_model, new_data = new_data, type = "prob") %>%
    rename_with( ~ paste0("xgboost_", .x))
  
  # Number of rows
  n_rows <- 198
  
  ### Combine predictions
  results <- ranger_results %>%
    bind_cols(xgboost_results) %>%
    mutate(pred_0 = (ranger_.pred_0 + xgboost_.pred_0) / 2,
           pred_1 = (ranger_.pred_1 + xgboost_.pred_1) / 2,
           pred_2 = (ranger_.pred_2 + xgboost_.pred_2) / 2,
           pred_3 = (ranger_.pred_3 + xgboost_.pred_3) / 2) %>%
    select(pred_0:pred_3) %>%
    bind_cols(new_data) %>%
    select(match_id, player_id, player_name, player_team, pred_0:pred_3) %>%
    mutate(expected_votes = (pred_0 * 0) + (pred_1 * 1) + (pred_2 * 2) + (pred_3 * 3)) %>%
    group_by(match_id) %>%
    slice_max(order_by = expected_votes, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(match_id, desc(expected_votes)) %>%
    mutate(predicted_votes = rep(c(3, 2, 1), n_rows))

  return(results)
  
}


# Define function to get top 20
totals_table <- function() {
  
  suppressMessages(
    append_data <- new_data %>%
      group_by(player_id, player_name, player_team) %>%
      summarize("Average Disposals" = mean(disposals),
                "Average Supercoach" = mean(supercoach_score))
  )
  
  suppressMessages(
    totals <- predicted_votes %>%
      group_by(player_id, player_name, player_team) %>%
      summarize(total = sum(predicted_votes)) %>%
      arrange(desc(total)) %>%
      left_join(append_data) %>%
      ungroup() %>%
      select(-player_id) %>%
      rename("Player Name" = player_name,
             "Team" = player_team, 
             "Total Predicted Votes" = total) %>%
      mutate(logo = case_when(
        Team == "Melbourne" ~ "https://upload.wikimedia.org/wikipedia/en/4/4e/Melbournefc.svg",
        Team == "Port Adelaide" ~ "https://upload.wikimedia.org/wikipedia/en/3/36/Port_Adelaide_Football_Club_logo.svg",
        Team == "Western Bulldogs" ~ "https://upload.wikimedia.org/wikipedia/en/0/09/Western_Bulldogs_logo.svg",
        Team == "Essendon" ~ "https://upload.wikimedia.org/wikipedia/en/8/8b/Essendon_FC_logo.svg",
        Team == "St Kilda" ~ "https://upload.wikimedia.org/wikipedia/en/5/58/St_Kilda_FC_logo.svg",
        Team == "Geelong" ~ "https://upload.wikimedia.org/wikipedia/en/5/5f/Geelong_Cats_logo.svg",
        Team == "Collingwood" ~ "https://upload.wikimedia.org/wikipedia/en/a/a6/Collingwood_Football_Club_Logo_%282017%E2%80%93present%29.svg",
        Team == "Sydney" ~ "https://upload.wikimedia.org/wikipedia/en/a/af/Sydney_Swans_Logo_2020.svg",
        Team == "Greater Western Sydney" ~ "https://upload.wikimedia.org/wikipedia/en/0/07/GWS_Giants_logo.svg",
        Team == "Hawthorn" ~ "https://upload.wikimedia.org/wikipedia/en/6/62/Hawthorn-football-club-brand.svg",
        Team == "Carlton" ~ "https://upload.wikimedia.org/wikipedia/en/5/58/Carlton_FC_Logo_2020.svg",
        Team == "Gold Coast" ~ "https://upload.wikimedia.org/wikipedia/en/7/7d/Gold_Coast_Suns_AFL_Logo.svg",
        Team == "West Coast" ~ "https://upload.wikimedia.org/wikipedia/en/b/b5/West_Coast_Eagles_logo_2017.svg",
        Team == "Fremantle" ~ "https://upload.wikimedia.org/wikipedia/en/c/ca/Fremantle_FC_logo.svg",
        Team == "Adelaide" ~ "https://upload.wikimedia.org/wikipedia/en/c/ca/Fremantle_FC_logo.svg",
        Team == "North Melbourne" ~ "https://upload.wikimedia.org/wikipedia/en/f/fc/North_Melbourne_FC_logo.svg",
        Team == "Brisbane Lions" ~ "https://upload.wikimedia.org/wikipedia/en/c/c7/Brisbane_Lions_logo_2010.svg",
        TRUE ~ "https://upload.wikimedia.org/wikipedia/en/3/35/Richmond_Tigers_logo.svg")) %>%
      select(`Player Name`, logo, `Team`, `Average Disposals`:`Average Supercoach`, `Total Predicted Votes`) %>%
      head(15)
  )
  
  table <- totals %>%
    gt() %>%
    tab_spanner(
      label = "Season Averages",
      columns = 4:5
    ) %>%
    tab_header(
      title = md("2021 Brownlow Medal Predictions"),
      subtitle = md("The top 15 players are shown - their predicted votes have been added up for for each game to give a total across the season")
    ) %>%
    fmt_percent(columns = 4:5,
                decimals = 0
    ) %>%
    data_color(
      columns = vars(`Total Predicted Votes`),
      colors = scales::col_numeric(
        palette = c("white", "#3fc1c9"),
        domain = NULL
      )
    ) %>%
    tab_source_note(
      source_note = md("**Data**: fitzroy | **Table**: @oarmstrong95")
    ) %>%
    # tab_footnote(
    #   footnote = "Note: percentile average per game.", 
    #   locations = cells_column_labels(columns = 4:5)
    # ) %>%
    gt_theme_538()
  
  return(table)
  
}


# Define function to customise aesthics of table
gt_theme_538 <- function(data,...) {
  data %>%
    text_transform(
      locations = cells_body(vars(logo)),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    # Relabel columns
    cols_label(
      logo = ""
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}


