# Simple model to estimate parturition dates
defaultW = getOption("warn") #nolint

options(tidyverse.quiet = TRUE,
        tidymodels.quiet = TRUE,
        warn=-1)
library(RMySQL)
library(tidyverse)
library(lubridate)
library(tidymodels)

###########################################
# This script fit a ML model to estimate part
# dates from n1 weights
# It is a simple regression model so it is by
# no means SOTA
###########################################

connect_to_krsp <- function() {
  require(RMySQL)
  out <- tryCatch({
    message <- "Connecting to KRSP . . . "
    DBI::dbConnect(RMySQL::MySQL(), group = "krsp-aws")
  },
  error = function(cond) {
    message("Unable to connect to KRSP database")
    message("See error below:")
    message(cond)
},
  warning = function(cond) {
  message(cond)
}
)
  return(out)
}


train <- function(df) {
  require(tidymodels)
  df_split <- rsample::initial_split(df, prop = 0.8)
  training <- training(df_split)
  testing <- testing(df_split)

  resamples <- rsample::bootstraps(training, times = 25)

  recipe <- recipe(part ~ ., data = training) %>%
    update_role(litter_id, new_role = "ID") %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(c(grid, year)) %>%
    step_interact(~ contains("grid"):contains("year"))
  #nolint
  regression_mod <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

  grid <- dials::grid_regular(dials::penalty(), levels = 25)

  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(regression_mod)

  doParallel::registerDoParallel()
  wf_grid <- tune_grid(
    wf,
    grid = grid,
    resamples = resamples
  )

  # Finalize Model ----------------------------------------------------------

  best_regression <- wf_grid %>%
    select_best("rmse")

  regression_fit <- wf %>%
    finalize_workflow(best_regression)

  last_fit <- regression_fit %>%
    last_fit(df_split)

  final_regression <- regression_fit %>%
    fit(training)
  #nolint
  return(final_regression)
}

save_and_export_model <- function(model) {
  saveRDS(model, file = "output/model/part_model.rds")
}

if (!interactive()) {
  con <- connect_to_krsp()
  dat <- dbGetQuery(con, read_file("scripts/sql/model-data.sql")) %>% #nolint
    tibble() %>%
    mutate(across(c(grid, year), as_factor))
  df <- dat %>%
    filter(!is.na(part))

  model <- tryCatch({
    message("Fitting model to data . . .")
    train(df)
    },
    error =  function(cond) {
      message("error fitting model to data")
      message(cond)
    }
    )
  tryCatch({
    message("Saving model to local directory . . .")
    save_and_export_model(model)
  },
    error = function(cond) {
      message("Error exporting model")
      message(cond)
  }
  )
  dbDisconnect(con)
}
options(warn = defaultW)