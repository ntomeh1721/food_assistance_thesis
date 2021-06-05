
## The goal of this document is to fill in the missing income, 
## hunger and food confidence data in the household pulse data set. 


# Set Up  -----------------------------------------------------------------

## Loading Libraries 
library(tidyverse)
library(tidymodels)

## Loading Data 
hps_loaded <- read_csv("data/datasets/hps_data.csv")

## Loading Guide
pov_guide <- read_csv("data/datasets/poverty_thresholds_guide.csv")

# Pulling Relevant Variables  -------------------------------------------

hps <- hps_loaded %>% 
  group_by(t, state, race, with_kids, education, food_sufficiency, 
           food_confidence, household_size, household_kids, income) %>% 
  summarize(people = sum(people))

# Exploring Missing-ness ---------------------------------------------------

# Summary of All Data 

hps_loaded %>% 
  naniar::miss_var_summary()

hps_loaded %>% 
  group_by(wave) %>% 
  naniar::miss_var_summary() %>% 
  filter(n_miss > 0)

hps_loaded %>% 
  mutate_at(vars(food_sufficiency, food_confidence, income), ~ifelse(is.na(.), "missing", "not_missing")) %>% 
  group_by(t, wave, food_sufficiency, food_confidence, income) %>% 
  summarize(people = sum(people)) %>% 
  group_by(wave, food_sufficiency, food_confidence, income) %>% 
  summarize(people = mean(people)) %>% 
  pivot_longer(cols = 2:4, names_to = "question", values_to = "missing") %>% 
  group_by(wave, question, missing) %>% 
  summarize(people = sum(people)) %>% 
  pivot_wider(names_from = missing, values_from = people) %>% 
  mutate(response_rate = 100*not_missing/(missing+not_missing)) %>% 
  select(wave, question, response_rate)

hps %>% 
  ungroup() %>% 
  filter(!is.na(income)) %>% 
  naniar::miss_var_summary()

## Approximately 20% of income observations are missing and 10.8% of food_confidence


# Distribution of Missing Data --------------------------------------------

# Peak at t = 3, steady decrease 
hps %>%
  ungroup() %>% 
  filter(is.na(income)) %>% 
  ggplot(aes(x = t)) +
  geom_histogram(bins = 17)

# Same as above 
hps %>%
  ungroup() %>% 
  filter(is.na(food_confidence)) %>% 
  ggplot(aes(x = t)) +
  geom_histogram(bins = 17)

# Peak at t = 9, definitively a second wave problem
hps %>%
  ungroup() %>% 
  filter(is.na(food_sufficiency)) %>% 
  ggplot(aes(x = t)) +
  geom_histogram(bins = 17)




# Changing Income to Low Income -------------------------------------------

## Prepping for comparison
pov_guide <- pov_guide %>% 
  crossing(tibble(income = seq(1, 8))) %>% 
  mutate(
    poverty = ifelse(income <= poverty_line, 1, 0),
    pebt_eligible = ifelse(income <= frl_line, 1, 0), 
    low_income = ifelse(income <= low_income_line, 1, 0)
  ) %>% 
  select(state, household_size, income, 
         poverty, pebt_eligible, low_income) 


## Combining with the household pulse data
full_data <- left_join(hps, pov_guide) %>% 
  mutate(
    pebt_eligible = ifelse(pebt_eligible == 1 & with_kids == "yes", 1, 0),
    with_kids = ifelse(with_kids == "yes", 1, 0))

full_data <- full_data %>% 
  select(-income, -poverty, -pebt_eligible, ) %>% 
  mutate(
    food_confidence = factor(food_confidence, levels = c("not at all", "somewhat", "moderately", "very")) %>% as.numeric(),
    food_sufficiency = factor(food_sufficiency, levels = c("often not enough", "sometimes not enough",
                                                                "enough not wanted", "enough wanted")) %>% as.numeric(),
    education = factor(education, levels = c("no_highschool_degree", "highschool_degree",
                                                  "some_college", "bach_or_higher")) %>% as.numeric())  

# Building the Recipe and Model -------------------------------------------

training <- full_data %>% 
  filter(!is.na(low_income), 
         !is.na(food_confidence),
         !is.na(food_sufficiency)) %>% 
  ungroup() 


new_train <- training %>% 
  group_by(low_income, state, race, with_kids, education, food_sufficiency, food_confidence, household_size) %>% 
  summarize(people = sum(people)) %>% 
  ungroup() %>% 
  select(-people) %>% 
  mutate(low_income = as.character(low_income))




# other -------------------------------------------------------------------

li_folds <- vfold_cv(new_train, v = 3, repeats = 2, strata = low_income)

hps_recipe <- recipe(low_income ~ . , new_train) %>% 
#  step_rm(people) %>% 
  step_rm(state) %>% 
  step_dummy(all_nominal(), -low_income, one_hot = T)

hps_recipe %>%
  prep() %>%
  bake(new_data = NULL)


rf_model <- rand_forest(
  mtry = tune(), 
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")


# update parameters 
rf_params <- parameters(rf_model)  %>% 
  update(mtry = mtry(range = c(2, 10)))

# define tuning grid
rf_grid <- grid_regular(rf_params, levels = 3) 

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(hps_recipe) 

# Tuning/fitting ----
# rf_tune <- rf_workflow %>% 
#   tune_grid(
#     resample = li_folds, 
#     grid = rf_grid
#   ) 


# save(rf_tune, file = "data/rf_tune.rda")

# final_workflow <- finalize_workflow(
#   rf_workflow, 
#   select_best(rf_tune, metric = "accuracy")
# )


# rbind(rf_tune$.metrics[[1]],
#       rf_tune$.metrics[[2]]) %>% 
#   filter(.metric == "accuracy") %>% 
#   arrange(desc(.estimate))

# final_fit <- fit(final_workflow, training)
# 
# predictions <- predict(final_fit, training)
# 
# save(predictions, file = "data/rf_training_predictions.rda")




# SVM Model ---------------------------------------------------------------


svm_model <- svm_poly(
  cost = tune(),
  degree = tune(),
  scale_factor = tune() 
) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab")

# update parameters 
svm_params <- parameters(svm_model) 


# define tuning grid
svm_grid <- grid_regular(svm_params, levels = 5) 

# workflow ----
svm_workflow <- workflow() %>% 
  add_model(svm_model) %>% 
  add_recipe(hps_recipe) 

svm_tune <- svm_workflow %>% 
  tune_grid(
    resample = li_folds, 
    grid = svm_grid
  ) 

save(svm_tune, file = "data/svm_tune.rda")



