
## This document is to write a data set that can be combined with the 
## household pulse data to articulate whether a household meets the 
## "low income" threshold and qualifies for FRL 

# Set Up  -----------------------------------------------------------------

# Libraries
library(tidyverse)
library(readxl)
library(janitor)

# Loading in the Data 

pov_guide <- read_csv("data/guides/fed_income_poverty.csv")

# State Naming Doc 
guide <- read_xlsx("data/guides/state_abbreviations.xlsx") %>% 
  clean_names()

# negate in 
`%notin%` <- negate(`%in%`)

## State Names
st_names <- guide %>%
  select(states)

# Poverty Guidelines ------------------------------------------------------


## Poverty guidelines 

### The goal here is to get a dummy variable: 
### do you qualify for FRL and thus P-EBT? --> 1.85 * poverty guide 
### are you considered low-income? 

pov <- pov_guide %>% 
  pivot_wider(names_from = household_members, values_from = income)

translate_line <- function(x){
  y <- case_when(
    x < 10 ~ x, 
    x %in% 10:25000 ~ 1, 
    x %in% 25000:34999 ~ 2, 
    x %in% 35000:49999 ~ 3, 
    x %in% 50000:74999 ~ 4,
    x %in% 75000:99999 ~ 5, 
    x %in% 100000:149999 ~ 6, 
    x %in% 150000:199999 ~ 7, 
    x %in% 200000 ~ 8
  )
  
  return(y)
  
}


pov_income_guide <- st_names %>%
  filter(states != "Alaska" & states != "Hawaii")  %>% 
  cbind(pov %>% 
          filter(state == "all") %>% 
          select(-state)) %>% 
  rbind(st_names %>%
          filter(states == "Alaska") %>% 
          cbind(pov %>% 
                  filter(state == "alaska") %>% 
                  select(-state))) %>% 
  rbind(st_names %>%
          filter(states == "Hawaii") %>% 
          cbind(pov %>% 
                  filter(state == "hawaii") %>% 
                  select(-state))) %>% 
  pivot_longer(cols = 2:9, names_to = "thhld_numper", values_to = "poverty_line") %>% 
  mutate(
    thhld_numper = as.numeric(thhld_numper),
    # this is the free and reduced lunch threshold 
    frl_line = 1.85*poverty_line, 
    # this is the threshold for being counted as "low income" 
    low_income_line = 1.5*poverty_line, 
    poverty_line = poverty_line
  ) 

pov_income_guide <- pov_income_guide %>% 
  # translating the numbers into income brackets 
  mutate_if(is.numeric, translate_line)  %>% 
  filter(states %notin% c("Alaska", "Hawaii") |  
         thhld_numper %notin% seq(2, 8, 2)) %>% 
  # Hacking out the weird NAs for Alaska and Hawaii
  rbind(pov_income_guide %>% 
          filter(states %in% c("Alaska", "Hawaii"), 
                 thhld_numper %in% seq(2, 8, 2)) %>% 
          mutate(
            poverty_line = translate_line(poverty_line), 
            low_income_line = translate_line(low_income_line), 
            frl_line = case_when(
              frl_line < 25000 ~ 1,
              frl_line >= 25000 & frl_line < 34999 ~ 2,
              frl_line >= 35000 & frl_line < 49999 ~ 3,
              frl_line >= 50000 & frl_line < 74999 ~ 4,
              frl_line >= 75000 & frl_line < 99999 ~ 5,
              frl_line >= 100000 & frl_line < 149999 ~ 6,
              frl_line >= 150000 & frl_line < 199999 ~ 7,
              frl_line >= 200000 ~ 8
            )
          )) %>% 
  rename("state" = "states", "household_size" = "thhld_numper")


## join this with the rest of the data after all other analysis 
## in order to include estimates for missing income values 


write.csv(pov_income_guide, "data/datasets/poverty_thresholds_guide.csv", row.names = F)

