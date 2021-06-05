
## The goal of this R script is to compare the estimates of kids on P-EBT 
## from the household pulse survey (i.e. those with kids whose income 
## qualifies them for FRL) and the states' estimates of kids on P-EBT. 

## We find that the states' estimates are 1.8 times that of the 
## household pulse-based estimate. 

# Set Up  -----------------------------------------------------------------

## Loading Libraries 
library(tidyverse)

## Loading Data 
hps <- read_csv("data/datasets/hps_data.csv")
pebt <- read_csv("data/datasets/pebt_data.csv")

## Loading Poverty Income Guide 
pov_guide <- read_csv("data/datasets/poverty_thresholds_guide.csv")

# Income Markers -------------------------------------------------------

## Low-Income, FRL (and thus P-EBT eligible) 

pov_guide <- pov_guide %>% 
  crossing(tibble(income = seq(1, 8))) %>% 
  mutate(
    poverty = ifelse(income <= poverty_line, 1, 0),
    pebt_eligible = ifelse(income <= frl_line, 1, 0), 
    low_income = ifelse(income <= low_income_line, 1, 0)
  ) %>% 
  select(state, household_size, income, 
         poverty, pebt_eligible, low_income) 


full_data <- left_join(hps, pov_guide)


# Comparing P-EBT Estimates -----------------------------------------------

## Why is this so bad 
## We have that the state estimates of kids on P-EBT is 1.8 times the 
## household pulse based estimate of people on P-EBT. Unsure where 
## this discrepancy is, but this relationship has an R squared of 0.98. 

comp_data <- full_data %>% 
  left_join(pebt %>% select(state, est_kids) %>% unique()) %>% 
  mutate(
    # number of households = people/household_size
    # times the number of kids in those household types 
    # is the number of kids total in that observation 
    num_kids = household_kids*people/household_size
  ) %>% 
  filter(
    pebt_eligible == 1, # just those that are p-ebt eligible 
    with_kids == "yes" # among just those with kids. 
  ) %>% 
  # summing across time periods t 
  group_by(t, state, est_kids) %>%
  summarize(
    num_kids = sum(num_kids),
    people = sum(people)
  ) %>% 
  # averaging across time periods t 
  group_by(state, est_kids) %>% 
  summarize(
    num_kids = mean(num_kids),
    people = mean(people)
  ) 

# Articulates the relationship between the two estimates 
lm(est_kids ~ num_kids, data = comp_data) %>% 
  summary()

# Illustrates the fit of the relationship
ggplot(data = comp_data, aes(x = est_kids, y = 1.8*num_kids)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, size = 0.2) + 
  theme_minimal() +
  labs(
    x = "State Estimate", 
    y = "1.8*HPS Estimate"
  )

