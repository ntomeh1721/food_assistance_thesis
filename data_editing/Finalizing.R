
## The goal of this document is to create a final data set for 
## determining the impact of SNAP and P-EBT on food insecurity. 
## 
## To do so, this document: 
## 1. Adds Low Income and P-EBT eligibility markers to the Household
##    Pulse Survey (HPS) data using the poverty guide. 
## 2. Gets rates of hunger, food non-confidence, food insecurity,
##    and monthly SNAP participation from the HPS. 
## 3. Combines the HPS data with the P-EBT release timing data. 

# Set Up  -----------------------------------------------------------------

## Loading Libraries 
library(tidyverse)
library(tidymodels)

## Loading Data 
pebt <- read_csv("data/datasets/pebt_data.csv")
hps <- read_csv("data/datasets/hps_data.csv")

## Loading Guides 
pov_guide <- read_csv("data/datasets/poverty_thresholds_guide.csv")
fi_guide <- read_csv("data/guides/state-fi-multipliers.csv")

# See Datasets 
# View(pebt)
# View(hps)
# View(pov_guide)
# View(fi_guide)

# Income Markers -------------------------------------------------------

## Prepping for comparison
pov_guide <- pov_guide %>% 
  crossing(tibble(income = seq(1, 8))) %>% 
  mutate(
    low_income = ifelse(income <= low_income_line, 1, 0)
  ) %>% 
  select(state, household_size, income, low_income) 


## Combining with the household pulse data
full_data <- left_join(hps, pov_guide) %>% 
  mutate(with_kids = ifelse(with_kids == "yes", 1, 0))


# Base Data Set -----------------------------------------------------------

base <- full_data %>% 
  group_by(
    # Observation Levels 
    week, t, state, wave,  
    # Want to be able to filter by 
    with_kids, low_income) %>% 
  summarize(
    people = sum(people)
    ) %>% 
  group_by(t, state, wave, with_kids, low_income) %>% 
  summarize(people = mean(people))



# Education and Race/Ethnicity Rates ---------------------------------------------------------
edu_data <- full_data %>% 
  filter(!is.na(food_sufficiency)) %>% 
  group_by(week, t, state, with_kids, low_income, education) %>% 
  summarize(people = sum(people)) %>% 
  pivot_wider(names_from = education, values_from = people)%>% 
  mutate_at(vars(bach_or_higher, highschool_degree, no_highschool_degree, some_college), 
            ~ifelse(is.na(.), 0, .)) %>% 
  group_by(t, state, with_kids, low_income) %>% 
  summarize(bach_or_higher = mean(bach_or_higher), 
            highschool_degree = mean(highschool_degree), 
            no_highschool_degree = mean(no_highschool_degree), 
            some_college = mean(some_college))

race_data <- full_data %>% 
  filter(!is.na(food_sufficiency)) %>% 
  group_by(week, t, state, with_kids, low_income, race) %>% 
  summarize(people = sum(people)) %>% 
  mutate(race = str_extract(race, "(Asian|Black|Latino|Other|White)") %>%
           tolower()) %>% 
  pivot_wider(names_from = race, values_from = people) %>% 
  mutate_at(vars(asian, black, latino, other, white), ~ifelse(is.na(.), 0, .)) %>% 
  group_by(t, state, with_kids, low_income) %>% 
  summarize(asian = mean(asian), 
            black = mean(black), 
            latino = mean(latino), 
            white = mean(white), 
            other = mean(other))

# Hunger ----------------------------------------------------------

## Goal: We want rates of hunger. 
## 
## To do so, we'll get counts of hungry people at each observation 
## level and counts of question respondents so that we can 
## aggregate up more easily later. 
## 
## note: hunger is  sometimes or often not having enough to eat

hungry_data <- full_data %>% 
  mutate(
    hungry = case_when(
      food_sufficiency %in% c("often not enough", 
                              "sometimes not enough") ~ "yes", 
      food_sufficiency %in% c("enough not wanted", 
                              "enough wanted") ~ "no"
  )) %>% 
  group_by(
    week, t, state, wave, # Observation Levels 
    with_kids, low_income, # Want to be able to filter by 
    hungry # Target Variable
    ) %>% 
  summarize(
    people = sum(people)
  ) %>% 
  filter(!is.na(hungry)) %>% 
  pivot_wider(names_from = hungry, values_from = people) %>% 
  mutate(
    yes = ifelse(is.na(yes), 0, yes), 
    no = ifelse(is.na(no), 0, no),
    sufficiency_respondents = yes + no
  ) %>% 
  rename("n_hungry" = "yes") %>% 
  select(-no) %>% 
  group_by(t, state, wave, with_kids, low_income) %>% 
  summarize(n_hungry = mean(n_hungry), 
            sufficiency_respondents = mean(sufficiency_respondents)) 

# Food Non-Confidence ---------------------------------------------------------

## Goal: We want rates of food non-confidence 
## 
## To do so, we'll get counts of non-confident people at each observation 
## level and counts of question respondents so that we can 
## aggregate up more easily later. 
## 
## note: food non-confidence is not at all, somewhat, or moderately 
## confident in one's ability to afford food in the next four weeks
## because these responses best predict food insecurity and hunger. 

non_conf_data <-full_data %>% 
  mutate(nonconf = case_when(
    food_confidence %in% c("somewhat", 
                            "moderately",
                           "not at all") ~ "yes", 
    food_confidence %in% c("very") ~ "no"
  )) %>% 
  group_by(
    week, t, state, wave, # Observation Levels 
    with_kids, low_income, # Want to be able to filter by 
    nonconf # Target Variable
    ) %>% 
  summarize(
    people = sum(people)
  ) %>% 
  filter(!is.na(nonconf)) %>% 
  pivot_wider(names_from = nonconf, values_from = people) %>% 
  mutate(
    yes = ifelse(is.na(yes), 0, yes), 
    no = ifelse(is.na(no), 0, no),
    confidence_respondents = yes+no
  ) %>% 
  select(-no) %>% 
  rename("n_nonconf" = "yes") %>% 
  group_by(t, state, wave, with_kids, low_income) %>% 
  summarize(n_nonconf = mean(n_nonconf), 
            confidence_respondents = mean(confidence_respondents))



# Food Insecurity ---------------------------------------------------------

## Food insecurity is a conversion from the hunger data, using the 
## multipliers in the fi_guide data set.

# Prepping to compare to HPS data 
fi_guide <- fi_guide %>% 
  pivot_longer(cols = 2:9) %>% 
  mutate(with_kids = ifelse(str_detect(name, "_kid"), 1, 0),
         name = str_remove(name, "_kid")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  filter(region != "USA")

# Preparing the Full Data-set and Combining 
fi_data <- full_data %>% 
  group_by(
    week, t, state, wave, # Observation Levels
    with_kids, low_income, # Want to be able to filter by
    food_sufficiency # Target Variable 
    ) %>% 
  summarize(
    people = sum(people)
  ) %>% 
  filter(!is.na(food_sufficiency)) %>% 
  mutate(region = tolower(state)) %>% 
  pivot_wider(names_from = food_sufficiency, 
              values_from = people) %>% 
  left_join(fi_guide) 

# Calculating Food Insecurity 
# Note: the total number of respondents to the relevant question is 
# supplied by the hunger calculations
fi_data2 <- fi_data %>% 
  ungroup() %>% 
  select(-low_income) %>% 
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  add_column(low_income = fi_data$low_income) %>% 
  mutate(n_fi = (food1 * `enough wanted` +
           food2 * `enough not wanted` +
           food3 * `sometimes not enough` +
           food4 * `often not enough`)) %>% 
  group_by(week, t, state, wave, with_kids, low_income) %>% 
  summarize(n_fi = sum(n_fi)) %>% 
  group_by(t, state, wave, with_kids, low_income) %>% 
  summarize(n_fi = mean(n_fi)) 


# Adding the food insecurity data to the base dataset.
fi_dat <- left_join(base, fi_data2)

# SNAP Participants  ------------------------------------------------------

## Goal: Calculating how many people are receiving SNAP in a given month.
##
## note: we only have data on SNAP participation starting in t = 9, but 
## the data is retroactive to January. Elsewhere in my thesis I compare 
## these SNAP participation estimates to the available state data to 
## confirm their validity. 


snap_monthly <- full_data %>% 
  group_by(week, t, state, with_kids, low_income, 
           snap_jan, snap_feb, snap_mar, 
           snap_apr, snap_may, snap_jun,
           snap_jul, snap_aug, snap_sep, 
           snap_oct, snap_nov, snap_dec) %>% 
  summarize(people = sum(people)) %>% 
  pivot_longer(cols = 6:17, names_to = "month", values_to = "value") %>% 
  filter(value == "yes") %>% 
  group_by(t, state, with_kids, low_income, month) %>% 
  summarize(people = sum(people)) %>% 
  # Averaging monthly snap values across all iterations of the survey 
  group_by(state, with_kids, low_income, month) %>% 
  summarize(n_snap = mean(people)) %>% 
  # changing to variable names 
  mutate(
    month = case_when(
      month == "snap_jan" ~ "january",
      month == "snap_feb" ~ "february",
      month == "snap_mar" ~ "march",
      month == "snap_apr" ~ "april",
      month == "snap_may" ~ "may",
      month == "snap_jun" ~ "june",
      month == "snap_jul" ~ "july",
      month == "snap_aug" ~ "august",
      month == "snap_sep" ~ "september",
      month == "snap_oct" ~ "october",
      month == "snap_nov" ~ "november",
      month == "snap_dec" ~ "december"
    ),
    month_num = case_when(
      month == "january" ~ 1,
      month == "february" ~ 2,
      month == "march" ~ 3,
      month == "april" ~ 4,
      month == "may" ~ 5,
      month == "june" ~ 6,
      month == "july" ~ 7,
      month == "august" ~ 8,
      month == "september" ~ 9,
      month == "october" ~ 10,
      month == "november" ~ 11,
      month == "december"  ~ 12
    ))


# Combining Data  -----------------------------------------------------

# Period t, month number, and month name conversion
months_guide <- base %>% 
  ungroup() %>% 
  select(t) %>% 
  unique() %>% 
  mutate(month_num = case_when(
    t == 1 ~ 4, 
    t %in% 2:3 ~ 5, 
    t %in% 4:5 ~ 6, 
    t %in% 6:7 ~ 7, 
    t == 9 ~ 8, 
    t %in% 10:11 ~ 9, 
    t %in% 12:13 ~ 10, 
    t %in% 14:15 ~ 11, 
    t %in% 16:17 ~ 12
  )) %>% 
  left_join(snap_monthly %>% 
              ungroup() %>% 
              select(month, month_num) %>% 
              unique())

# Combining HPS Data
data <- left_join(hungry_data, non_conf_data) %>% 
  left_join(fi_dat) %>% 
  left_join(months_guide) %>% 
  left_join(snap_monthly) %>% 
  left_join(race_data) %>% 
  left_join(edu_data) %>% 
  mutate(snap = ifelse(is.na(n_snap), 0, n_snap)) %>% 
  select(-n_snap)


# P-EBT Final Editing 
pebt <- pebt %>% 
  select(-est_beneficiaries) 

# P-EBT release values to dummy variables 
pebt <- pebt %>% 
  mutate(
    pebt = ifelse(pebt > 1, 1, pebt), 
    app_snap = ifelse(application == "all", 1, 0), 
    app_nonsnap = ifelse(application == "none", 0, 1)
  ) %>%
  select(-application) %>% 
  unique() %>% 
  group_by(t, state, app_snap, app_nonsnap) %>% 
  summarize(pebt = max(pebt))


# Combining with P-EBT
final <- left_join(data, pebt) %>% 
  ungroup() %>% 
  select(
    # Observation Levels 
    t, month, month_num, # time 
    state,
    # For Sub-setting Data 
    with_kids, low_income, 
    # Target Variables 
    n_hungry, n_fi, n_nonconf, sufficiency_respondents, confidence_respondents,
    # Parameters of Interest 
    n_snap = snap, pebt, 
    # Race and Education 
    bach_or_higher, highschool_degree, no_highschool_degree, some_college, 
    asian, black, latino, other, white, 
    # Weight 
    people
  ) %>% 
  mutate(
    n_nonconf = ifelse(is.na(n_nonconf), 0, n_nonconf), 
    confidence_respondents = ifelse(is.na(confidence_respondents), 0, confidence_respondents)
  ) 


# Writing -------------------------------------------------------------

write.csv(snap_monthly, "data/snap_monthly.csv", row.names = F)

write.csv(final, "data/datasets/final.csv", row.names = F)


