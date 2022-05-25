options(tidyverse.quiet = TRUE,
        tidymodels.quiet = TRUE,
        warn = -1)
        
library(tidyverse)
library(tidymodels)
library(lubridate)
library(RMySQL)

train = TRUE #if TRUE, this will train/fit the part date model

######################################################
# This code cleans the personality data and then imputes
# missing part dates using a very simple ML model
# The code for training the model is in model-training.R
# Happy trails - M
######################################################

con = dbConnect(MySQL(), group = 'krsp-aws') #nolint


# Create Personality file
personality = read_csv('data/personality-master.csv', show_col_types = FALSE) %>% # nolint
  janitor::clean_names() %>%
  filter(ageclass == "J",
         exclude_unless_video_reanalyzed == "N",
         proceed_with_caution == "N",
         # remove due to GC experiment
         !observer == "SWK") %>% 
  group_by(sq_id, year) %>% 
  filter(
    # remove the samples run by April in 2017 and 2018
    if( (year %in% 2017:2018) && (observer == "ARM")) trialnumber == 1 else TRUE
  ) %>% 
  select(squirrel_id = sq_id
         , sex
         , year
         , observer
         , cohort
         , year
         , grid
         , trialnumber
         , trialdate
         , taglft
         , tagrt
         , colours
         , walk
         , jump
         , hole
         , hang
         , chew
         , groom
         , still
         , oft_duration
         , front
         , back
         , attack
         , attacklatency
         , approachlatency
         , mis_duration
         , collar
         , comments) %>%
  ungroup() %>%
  # Join to db data
  left_join(
    .,
    dbGetQuery(con, read_file("scripts/sql/temp.sql")),
    by = "squirrel_id"
) %>%
  # Format for the PART prediction model
  mutate(part = yday(fieldBDate),
         n1_date = yday(n1_date),
         trialdate = as_date(trialdate, format = "%m/%d/%y"),
         julian_trialdate = yday(trialdate))


# Generate predictions from model
full_data = personality %>%
  filter(!is.na(part))

missing_parts = personality %>%
  filter(is.na(part)) %>%
  select(-part) %>%
  mutate(across(c(grid, year), as_factor))

if (train) {
  system("Rscript scripts/r/model-training.R")
}

part_model = readRDS("output/model/part_model.rds")

predictions = predict(part_model, missing_parts) %>% 
  bind_cols(missing_parts, .) %>% 
  rename(part = .pred) %>% 
  mutate(across(c(grid, year), as.character),
         year = as.integer(year),
         part = as.integer(part))

# write file
personality = bind_rows(full_data, predictions)

write_csv(personality, file = 'data/personality-mrw-imputed.csv')
