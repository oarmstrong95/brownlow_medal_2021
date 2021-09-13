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
    list of tibbles:    player data as well as imported csv's
  
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

