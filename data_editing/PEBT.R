

## The goal here is to read in the P-EBT Data and make it addable to the
## household pulse data set as well as create a document/data set that 
## allows us to compare HPS estimates of P-EBT eligibility with 
## state estimates of children benefitting.

## GOAL:state, t, snap recipient (y/n), receiving p-ebt rn (y/n), 
## how much p-ebt, how many kids 

# Set Up  -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
library(janitor)

pebt <- read_excel("data/guides/PEBT.xlsx", sheet = "Final") %>% clean_names()

`%notin%` <- negate(`%in%`)


# When: Organizing P-EBT Release Dates  --------------------------------------

## P-EBT Dates Guide
pebt_dates <- pebt %>% 
  select(state, snap1, snap2, snap3, 
         nonsnap1, nonsnap2, nonsnap3, 
         sept_pebt)

# Data Set with state, groups, and dates in biweekly t identifier form
pebt_t_dates <- pebt_dates %>% 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "date") %>% 
  na.omit(date) %>% 
  mutate(t = case_when(
    date <=  as.Date("04/19/20", format = "%m/%d/%y") ~ 0, 
    date >= as.Date("04/20/20", format = "%m/%d/%y") & 
      date <= as.Date("05/05/20", format = "%m/%d/%y") ~ 1, 
    date >= as.Date("05/05/20", format = "%m/%d/%y") & 
      date <= as.Date("05/19/20", format = "%m/%d/%y") ~ 2, 
    date >= as.Date("05/20/20", format = "%m/%d/%y") & 
      date <= as.Date("06/02/20", format = "%m/%d/%y") ~ 3, 
    date >= as.Date("06/02/20", format = "%m/%d/%y") & 
      date <= as.Date("06/16/20", format = "%m/%d/%y") ~ 4, 
    date >= as.Date("06/16/20", format = "%m/%d/%y") & 
      date <= as.Date("06/30/20", format = "%m/%d/%y") ~ 5, 
    date >= as.Date("06/30/20", format = "%m/%d/%y") & 
      date <= as.Date("07/14/20", format = "%m/%d/%y") ~ 6, 
    date >= as.Date("07/14/20", format = "%m/%d/%y") & 
      date <= as.Date("07/21/20", format = "%m/%d/%y") ~ 7, 
    date >= as.Date("07/21/20", format = "%m/%d/%y") & 
      date <= as.Date("08/14/20", format = "%m/%d/%y") ~ 8, 
    date >= as.Date("08/14/20", format = "%m/%d/%y") & 
      date <= as.Date("08/31/20", format = "%m/%d/%y") ~ 9, 
    date >= as.Date("09/01/20", format = "%m/%d/%y") & 
      date <= as.Date("9/14/20", format = "%m/%d/%y") ~ 10, 
    date >= as.Date("09/15/20", format = "%m/%d/%y") & 
      date <= as.Date("9/28/20", format = "%m/%d/%y") ~ 11, 
    date >= as.Date("09/29/20", format = "%m/%d/%y") & 
      date <= as.Date("10/12/20", format = "%m/%d/%y") ~ 12,
    date >= as.Date("10/13/20", format = "%m/%d/%y") & 
      date <= as.Date("10/26/20", format = "%m/%d/%y") ~ 13,
    date >= as.Date("10/27/20", format = "%m/%d/%y") & 
      date <= as.Date("11/09/20", format = "%m/%d/%y") ~ 14,
    date >= as.Date("11/10/20", format = "%m/%d/%y") & 
      date <= as.Date("11/23/20", format = "%m/%d/%y") ~ 15,
    date >= as.Date("11/24/20", format = "%m/%d/%y") &
      date <= as.Date("12/07/20", format = "%m/%d/%y") ~ 16,
    date >= as.Date("12/08/20", format = "%m/%d/%y") & 
      date <= as.Date("12/21/20", format = "%m/%d/%y") ~ 17,
  )) %>% 
  select(-date) %>% 
  pivot_wider(names_from = group, values_from = t) 



# All States, all t, binary pebt for each group 
pebt_base <- tibble(
  state = pebt %>% 
  pluck("state") %>% 
  unique() %>% 
  rep(17), 
  t = seq(1, 17) %>% rep(each = 51)
  )

snap_ts <- pebt_t_dates %>%
  select(state, snap1, snap2, snap3) %>% 
  pivot_longer(cols = 2:4, 
               names_to = "snap", values_to = "t") %>% 
  mutate(snap = substring(snap, 5, 5)) %>% 
  na.omit() 

nonsnap_ts <- pebt_t_dates %>% 
  select(state, nonsnap1, nonsnap2, nonsnap3) %>% 
  pivot_longer(cols = 2:4, names_to = "nonsnap", values_to = "t") %>% 
  mutate(nonsnap = substring(nonsnap, 8, 8)) %>% 
  na.omit() 

sept_ts <- pebt_t_dates %>% select(state, sept_pebt) %>% 
  mutate("sept_ben" = 1) %>% 
  rename("t" = "sept_pebt") %>% 
  na.omit()

  
full_pebt_dates <- left_join(pebt_base, snap_ts) %>% 
  left_join(nonsnap_ts) %>% 
  left_join(sept_ts) 


# Getting Just "Is PEBT Released This Week?"  -----------------------------

pebt_new <- full_pebt_dates %>% 
  mutate(sept_ben = as.character(sept_ben)) %>% 
  pivot_longer(cols = c(snap, nonsnap, sept_ben), names_to = "group", values_to = "pebt") %>% 
  mutate(pebt = ifelse(is.na(pebt), 0, pebt)) %>% 
  select(state, t, pebt) %>% 
  unique()


final_pebt <- left_join(
  pebt_new, 
  pebt %>% select(state, est_beneficiaries, application)
)


# Getting a Kids Count ----------------------------------------------------

write.csv(final_pebt, "data/datasets/pebt_data.csv", row.names = F) 
