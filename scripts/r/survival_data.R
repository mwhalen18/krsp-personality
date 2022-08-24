library(tidyverse)
library(RMySQL)
# script to get survival to 200d

# TODO:
# Need to get survival to fall census


con = dbConnect(MySQL(), group = 'krsp-aws')

personality = read_csv('data/personality-mrw-imputed.csv', show_col_types = FALSE)

# Load in file created from data-cleaning.R and calculate ages and growth rates

censi = tbl(con, "census") %>% 
  filter(MONTH(census_date) == 8,
         YEAR(census_date) < 2021) %>% 
  mutate(census_year = YEAR(census_date), made_it = 1) %>% 
  select(squirrel_id, census_year, made_it) %>% 
  collect()

query = "
select squirrel_id
   , 2021 AS census_year
   , 1 AS made_it
   , gr as grid
from krsp2022.census
where YEAR(census_date) = 2022 
"
census_2022 = dbGetQuery(con, query)
censi = bind_rows(censi, census_2022)

personality = tbl(con, 'flastall2') %>% 
  select(squirrel_id, dates, f1, byear, litter_id, dam_id, datee, f2) %>% 
  collect() %>% 
  left_join(personality, .,  by = c('squirrel_id', 'litter_id', 'dam_id'), suffix = c('.per', '.fla')) %>% 
  left_join(censi, by = c('squirrel_id' = 'squirrel_id', 'byear' = 'census_year')) %>% 
  replace_na(list(made_it = 0)) %>% 
  # This logic just ensures that for individuals born in 2021 we use the census 'made it' column rather than longevity
  mutate(age_at_trial = julian_trialdate - part,
         longevity = as.integer(difftime(datee, fieldBDate, units = 'days')),
         survived_200d = case_when(
           (byear == 2021) & (made_it == 1) ~ TRUE,
           (byear < 2021) & (longevity >= 200) ~ TRUE,
           TRUE ~ FALSE
           ),
         nest_days = as.numeric(difftime(n2_date, fieldBDate, units = 'days')),
         growth = (n2_weight - n1_weight)/nest_days)

# Well also pull in grid densities here as well

query = read_file('scripts/sql/grid_density.sql')
grids_density = dbGetQuery(con, query) %>% 
  select(grid, year = Year, grid_density = spr_density)

# Write the new file
personality = left_join(personality, grids_density, by = c("grid", "year"))
write_csv(personality, file = 'data/personality-mrw-survival.csv')
