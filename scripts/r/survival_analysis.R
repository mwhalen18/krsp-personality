# Analysis of survival
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(lme4)
library(car)
library(DHARMa)

#TODO:
  # Add 2021 litter data from krsp temp db


personality = read_csv('mrw-scripts/data/personality-mrw-survival.csv', show_col_types = FALSE) %>% 
  mutate(survival = as.integer(survived_200d)) %>% 
  group_by(grid, year) %>% 
  mutate(growth_sc = scale(growth, scale = T, center = T)[,1],
         part_sc = scale(part, scale = T, center = T)[,1]) %>% 
  ungroup() 


aggression = personality %>% 
  select(front, back, attack, attacklatency, approachlatency) %>% 
  mutate(across(everything(), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x))) 

pca_agg = prcomp(aggression, scale = TRUE, center = TRUE)
mis1 = predict(pca_agg)[,1]

activity = personality %>% 
  select(walk, still, hang, jump, chew, hole) %>% 
  mutate(across(everything(), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))

pca_act = prcomp(activity, scale = TRUE, center = TRUE)
oft1 = predict(pca_act)[,1]

personality$oft1 = unlist(oft1)
personality$mis1 = unlist(mis1)

ggplot(personality, aes(oft1, mis1)) +
  geom_point() +
  stat_ellipse(level = 0.95) +
  labs(x = "Activity", y = "Aggression") +
  theme_minimal() +
  theme(plot.margin = margin(20,20,20,20))



ggplot(personality, aes(mis1, survival)) +
  geom_point()


# Survival to fall census -------------------------------------------------
dat = personality %>% 
  mutate(across(c(year, dam_id, litter_id, grid), as_factor)) %>% 
  filter(year != 2019)

survival_to_autumn = glmer(made_it ~ sex + 
                             scale(age_at_trial) + 
                             part_sc*scale(grid_density) +
                             growth_sc*scale(grid_density) + 
                             oft1*mis1*scale(grid_density) + 
                             grid +
                             (1|year) + 
                             (1|dam_id) + 
                             (1|litter_id), 
                           data = dat,
                           na.action = 'na.omit',
                           family = binomial(link = "logit"),
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

car::Anova(survival_to_autumn)

summary(survival_to_autumn)

# Diagnostics
simulationOutput = simulateResiduals(survival_to_autumn, plot = F)

residuals(simulationOutput, quantileFunction = qnorm)

plot(simulationOutput)
# Looks pretty good



# Model 2 Survival to 200 days --------------------------------------------
dat = personality %>% 
  mutate(across(c(year, dam_id, litter_id, grid), as_factor))

survival_to_200d = glmer(survival ~ sex + 
                           scale(age_at_trial) + 
                           part_sc*scale(grid_density) +
                           growth_sc*scale(grid_density) + 
                           oft1*mis1*scale(grid_density) + 
                           grid +
                           (1|year) + 
                           (1|dam_id) + 
                           (1|litter_id),
                         data = dat,
                         na.action = 'na.omit',
                         family = 'binomial',
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

#allFit(mod)
car::Anova(survival_to_200d)

# Diagnostics
simulationOutput = simulateResiduals(survival_to_200d, plot = F)

residuals(simulationOutput, quantileFunction = qnorm)

plot(simulationOutput)
# Theres some wonkiness here with one of the random effects deviating
# But it seems to not be a massive issue