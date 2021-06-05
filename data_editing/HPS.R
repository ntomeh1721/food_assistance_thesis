
### This file will reads the micro data from the Household Pulse 
### Survey. The goal is to get the data into a form that is 
### versatile to be analyzed later and combined with P-EBT data. 

# Set Up  -----------------------------------------------------------------

# Loading Libraries 
library(tidyverse)
library(tidymodels)
library(naniar)
library(janitor)
library(readxl)

# Removing Scientific Notation 
options(scipen = 999)

# Loading in the Data 

# State Naming Doc 
guide <- read_xlsx("data/guides/state_abbreviations.xlsx") %>% 
  clean_names()

food_insecurity <- read_csv("~/Documents/Thesis/Draft 1 (no UI, simple P-EBT)/Draft Analysis/data/datasets/food_insecurity.csv")

# Reading in Microdata ----------------------------------------------------
data <- {} 

for(i in 1:21){
  
  week <- read_csv(paste0("data/microdata/week", i, 
                          "/pulse2020_puf_",
                          ifelse(i >= 10, 
                                 i,
                                 paste0("0", i)),
                          ".csv")) %>%
    clean_names()
  
  # Selecting Variables by week 
  
  if(i %in% 1:6){
    
    week <- week %>% 
      select(rhispanic, rrace, thhld_numkid, 
             eeduc, curfoodsuf, foodconf,
             pweight, week, est_st,
             thhld_numper, income) %>% 
      mutate(
        wave = "first", 
        snapmnth1 = 0, snapmnth2 = 0, snapmnth3 = 0, 
        snapmnth4 = 0, snapmnth5 = 0, snapmnth6 = 0, 
        snapmnth7 = 0, snapmnth8 = 0, snapmnth9 = 0, 
        snapmnth10 = 0, snapmnth11 = 0, snapmnth12 = 0
      )
    
  }
  
  if(i %in% 7:12){
    
    week <- week %>% 
      select(rhispanic, rrace, thhld_numkid,
             eeduc, curfoodsuf, foodconf, est_st,
             pweight, week, 
             thhld_numper, income
      ) %>% 
      mutate(
        wave = "first", 
        snapmnth1 = 0, snapmnth2 = 0, snapmnth3 = 0, 
        snapmnth4 = 0, snapmnth5 = 0, snapmnth6 = 0, 
        snapmnth7 = 0, snapmnth8 = 0, snapmnth9 = 0, 
        snapmnth10 = 0, snapmnth11 = 0, snapmnth12 = 0
      )
    
  }
  
  if(i > 12) {
    
    week <- week %>% 
      select(
        # Same as above 
        rhispanic, rrace, thhld_numkid, 
        eeduc, curfoodsuf, foodconf, 
        est_st, pweight, week, 
        thhld_numper, income,  
        
        # New 
        snapmnth1, snapmnth2, snapmnth3,
        snapmnth4, snapmnth5, snapmnth6,
        snapmnth7, snapmnth8, snapmnth9,
        snapmnth10, snapmnth11, snapmnth12
      ) %>% 
      mutate(wave = "second")
    
  }
  
  
  data <- rbind(week, data)
  
}

# Editing State Names -----------------------------------------------------

## State Names
st_names <- guide %>%
  select(states) 

st_nums <- data %>% 
  arrange(est_st) %>% 
  select(est_st) %>% 
  unique() 

state_ids <- cbind(st_names, st_nums)

data1 <- left_join(data, state_ids, by = "est_st") %>% 
  select(-est_st) %>% 
  rename(state = states)

# Checking Missing Variables  ---------------------------------------------

## Rewriting nonanswers as NA 
# data <- data %>% 
#    map_dfr(.f = ~ ifelse(. < 0, NA, .))

## This is a data frame that returns a missing variable summary 
# data %>% 
#   miss_var_summary() 

## note: the snap month variables are 40% missing, which makes sense 
## giving that they were added in the second half of the survey. 
## for the first 13 weeks of the survey, the snapmnthx variables
## are equal to zero (meaning, not in the survey), whereas after 
## they are NA when the respondent did not input them. Editing is
## needed only to make them all equal to zero. 

## accounting for non-snap month variables 
# data %>% 
#   miss_var_summary() %>% 
#   filter(!str_detect(variable, "snapmnth"))

## income has 16.9 percent missing; will account for when writing 
## the recipe for the data; less than 20% can be accounted for
## foodconf, curfoodsuf are the only others with 
## missing data, though generally pretty small 



# Editing Column by Column ------------------------------------------------

data2 <- data1 %>% 
  mutate( 
    # Week - fixing second and third wave weeks 
    week = case_when(
      week < 13 ~ week, 
      week >= 13 ~ 2*(week - 4)
    ), 
    # Race
    race = case_when(
      rhispanic == 2 ~ "Hispanic or Latino, all Races",
      rhispanic == 1 & rrace == 1 ~ "White alone, not Hispanic", 
      rhispanic == 1 & rrace == 2 ~ "Black alone, not Hispanic",
      rhispanic == 1 & rrace == 3 ~ "Asian alone, not Hispanic", 
      TRUE ~ "Two or more races or Other"
    ), 
    # Education Level 
    education = case_when(
      eeduc %in% 1:2 ~ "no_highschool_degree", 
      eeduc == 3 ~ "highschool_degree", 
      eeduc == 4 ~ "some_college", 
      eeduc >= 5  ~ "bach_or_higher"
    ), 
    # Kids 
    with_kids = ifelse(
      thhld_numkid > 0,
      "yes",
      "no"
    ),
    household_kids = thhld_numkid, 
    household_size = thhld_numper, 
    # Food Sufficiency in the Past 7 Days 
    food_sufficiency = case_when(
      curfoodsuf == 1 ~ "enough wanted", 
      curfoodsuf == 2~ "enough not wanted", 
      curfoodsuf == 3 ~ "sometimes not enough", 
      curfoodsuf == 4 ~ "often not enough"
    ), 
    # Confidence in food sufficiency in the next four weeks 
    food_confidence = case_when(
      foodconf == 1 ~ "not at all", 
      foodconf == 2 ~ "somewhat", 
      foodconf == 3 ~ "moderately", 
      foodconf == 4 ~ "very"
    ), 
    # SNAP Rates, by Month 
    snap_jan = ifelse(snapmnth1 == 1, "yes", "no"), 
    snap_feb = ifelse(snapmnth2 == 1, "yes", "no"), 
    snap_mar = ifelse(snapmnth3 == 1, "yes", "no"),
    snap_apr = ifelse(snapmnth4 == 1, "yes", "no"),
    snap_may = ifelse(snapmnth5 == 1, "yes", "no"),
    snap_jun = ifelse(snapmnth6 == 1, "yes", "no"),
    snap_jul = ifelse(snapmnth7 == 1, "yes", "no"),
    snap_aug = ifelse(snapmnth8 == 1, "yes", "no"),
    snap_sep = ifelse(snapmnth9 == 1, "yes", "no"),
    snap_oct = ifelse(snapmnth10 == 1, "yes", "no"),
    snap_nov = ifelse(snapmnth11 == 1, "yes", "no"),
    snap_dec = ifelse(snapmnth12 == 1, "yes", "no"), 
  ) %>% 
  select(week, state, pweight, race, with_kids, wave, 
         education, food_sufficiency, food_confidence,
         income, household_kids, household_size, 
         snap_jan, 
         snap_feb, snap_mar, snap_apr, 
         snap_may, snap_jun, snap_jul, 
         snap_aug, snap_sep, snap_oct, 
         snap_nov, snap_dec)


# Condensing
data3 <- data2 %>% 
  group_by(
    week, state, race, with_kids, wave, 
    education, food_sufficiency, food_confidence,
    income, household_kids, household_size, 
    snap_jan, 
    snap_feb, snap_mar, snap_apr, 
    snap_may, snap_jun, snap_jul, 
    snap_aug, snap_sep, snap_oct, 
    snap_nov, snap_dec
  ) %>% 
  summarize(people = sum(pweight)) %>% 
  # Aggregate into two week periods
  mutate(t = case_when(
    week == 1 ~ 1, 
    week <= 12 ~ ceiling((week+1)/2), 
    week > 12 ~ 0.5*week
  )) 


data4 <- data3 %>%
  map_dfr(.f = ~ ifelse(. < 0, NA, .))



## Conclusion: all the errors add up, group by t later 
write.csv(data4, "data/datasets/hps_data.csv", row.names = F)


