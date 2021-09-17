
### Load packages and functions -------------------------------------------------------------------------

# Load and install packages locally
pacman::p_load(tidyverse, here, fitzRoy, janitor, lubridate, naniar, tidymodels, 
               readr, themis, ranger, parallel, doParallel, tictoc, rvest, vip, gt)

setwd(paste0(here::here(), "/01 Scripts"))
source('00_functions.R', echo=FALSE)

### Global variables ------------------------------------------------------------------------------------

# Defined range of interest for the modeling
YEARS <- 2016:2021

### Import and clean data -------------------------------------------------------------------------------

# Import data
setwd(paste0(here::here(), "/00 Data Assets"))
match_data <- import_data(YEARS)
afl_com_au_votes <- read_csv("afl_com_au_votes.csv")
coaches_votes <- read_csv("coaches_votes.csv")
fantasy_scores <- read_csv("fantasy_scores.csv")
brownlow_votes_2020 <- read_csv("2020_brownlow_votes.csv")

# Clean data
clean_all_data <- clean_data(match_data, afl_com_au_votes, 
                             coaches_votes, fantasy_scores, brownlow_votes_2020)

### Split the data --------------------------------------------------------------------------------------

# Split for new data
new_data <- clean_all_data %>%
  filter(season == 2021) %>%
  select(-brownlow_votes)

# Split for model data
model_data <- clean_all_data %>%
  filter(season != 2021)

### Clean data ------------------------------------------------------------------------------------------

# Split the historical data into training and test sets
set.seed(123)
split <- initial_split(model_data, strata = brownlow_votes)
train <- training(split)
test <- testing(split)

# Split the training data into bootstrap samples
set.seed(234)
bootstrap_folds <- 
  bootstraps(train, strata = brownlow_votes)

### Train and fit model ---------------------------------------------------------------------------------

setwd(paste0(here::here(), "/01 Scripts"))
source('01_models.R', echo = TRUE)

# See how the model performs on the test set using optimal hyper parameters
ranger_metrics
xgboost_metrics
nn_metrics

### Get predictions for 2021 ----------------------------------------------------------------------------

# Fit the final model to all historic data and predict on the new data
# Use a straight average of all the model predictions
predicted_votes <- predict_function()

totals_table()


### Export results  --------------------------------------------------------------------------------------
setwd(paste0(here::here(), "/02 Outputs"))

predicted_votes %>%
  write_csv('2021_predicted_votes.csv')






