
## 1. A way to run regressions across all possible columns. 
## 2. Calculating how much P-EBT is released per iteration.




# Combinations of Outcomes, Variables, Observations, and Filtered  --------

time <- c("t", "month", NA) 
var1 <- c("perc_snap", "perc_snap_m1", "pebt_snap") 


a <- crossing(time, var1 = c("perc_snap_m1", "pebt_snap")) %>% 
  mutate(
    var2 = case_when(
      var1 == "pebt_snap" ~ "pebt_nonsnap"
    )) %>% 
  crossing(var3 = c(NA, "pebt_snap*app_snap")) %>% 
  mutate(
    var4 = ifelse(var3 == "pebt_snap*app_snap", "pebt_nonsnap*app_nonsnap", NA), 
    var5 = NA
  ) %>% 
  filter(var1 == "pebt_snap" | is.na(var3))

b <- crossing(
  time, 
  var1 = "perc_snap", 
  var2 = c("perc_snap_m1", "pebt_snap")
) %>% 
  mutate(
    var3 = case_when(
      var2 == "pebt_snap" ~ "pebt_nonsnap"
    )
  )

c <- b %>% 
  filter(!is.na(var3)) %>% 
  crossing(var4 = c(NA, "pebt_snap*app_snap")) %>% 
  mutate(var5 = ifelse(var4 == "pebt_snap*app_snap", "pebt_nonsnap*app_nonsnap", NA))

d <- crossing(time, var1 = "perc_snap", var2 = NA, var3 = NA, var4 = NA, var5 = NA)

params <- rbind(a, b %>% mutate(var4 = NA, var5 = NA)) %>% 
  rbind(c) %>% 
  rbind(d) 

setup <- crossing(low_income = c(T, F), with_children = c(T, F), race = c(T, F), education = c(T, F)) %>% 
  mutate(
    var6 = ifelse(race == T, "race", NA), 
    var7 = ifelse(education == T, "education", NA)
  ) %>% 
  crossing(params, outcome_var = c("perc_hungry", "perc_nonconf", "perc_fi")) %>% 
  crossing(state = c("state", NA))

setup <- setup %>% 
  select(low_income, with_children, race, education,
         outcome_var, state, time, var1, var2, var3, 
         var4, var5, var6, var7)



# Regression Function -----------------------------------------------------

### RETURN TO EDITING THIS 
regressing <- function(i){
  
  dat <- setup[i, ]
  
  # Selecting from combinations
  temp <- dat %>% 
    mutate(vars = paste(c(state, time, var1, var2, 
                          var3, var4, var5, var6, var7) %>% 
                          na.omit(), 
                        collapse = "+")) %>% 
    select(low_income, with_children, race, education, outcome_var, vars)
  
  # Creating the Data Set 
  temp_data <- paste0("lens( race =", 
                      temp %>% pluck("race"), 
                      ", education = ",
                      temp %>% pluck("education"), 
                      ", low_income = ",
                      temp %>% pluck("low_income"),
                      ", with_kids = ",
                      temp %>% pluck("with_children")
  )
  
  # Running the Regression
  model <- paste0(
    "lm(", temp %>% pluck("outcome_var")," ~ ", 
    temp %>% pluck("vars"), 
    ", data = temp_data, weight = people)"
  ) %>% 
    str2expression() %>% 
    eval() 
  
  # Creating the Tibble 
  data_name <- case_when(
    temp %>% pluck("low_income") == T & temp %>% pluck("with_children") == T ~ "Low Income Households With Kids",
    temp %>% pluck("low_income") == F & temp %>% pluck("with_children") == T ~ "Households With Kids",
    temp %>% pluck("low_income") == T & temp %>% pluck("with_children") == F ~ "Low Income Households",
    temp %>% pluck("low_income") == F & temp %>% pluck("with_children") == F ~ "All Households",
  )
  
  variables <- dat %>% 
    pivot_longer(cols = c(state, time, var1, var2, var3, var4, var5, var6, var7)) %>% 
    na.omit(values) %>% 
    pluck("value") %>% 
    list()
  
  outcome_name <- case_when(
    temp %>% pluck("outcome_var") == "perc_fi" ~ "Food Insecurity", 
    temp %>% pluck("outcome_var") == "perc_hungry" ~ "Hunger", 
    temp %>% pluck("outcome_var") == "perc_nonconf" ~ "Food Non-Confidence"
  )
  
  # Evaluating the Model
  list(
    data = data_name,
    outcome = outcome_name, 
    vars = variables, 
    model = model,
    dataset = temp_data
  )
}


# Making Final Tibble Function --------------------------------------------

control_variables <- c("state", "race", "education", "t", "month")
parameters <- c("perc_snap", "perc_snap_m1", "pebt_snap", "pebt_nonsnap", 
                "pebt_snap_amt", "pebt_nonsnap_amt")


making_table_babies <- function(i){
  
  reg <- regressing(i) 
  
  table <- reg$model %>% 
    summary() %>% 
    pluck("coefficients") 
  
  
  coef_table <- table %>% 
    as.tibble() %>% 
    cbind(variable = row.names(table)) %>% 
    filter(str_detect(variable, "state") == F,
           str_detect(variable, "month") == F) %>% 
    rename("coef" = "Estimate",
           "std_error" = "Std. Error", 
           "p_val" = "Pr(>|t|)") %>% 
    select(variable, coef, std_error, p_val) %>% 
    mutate(p_val = round(p_val, 4))
  
  
  controlled <- tibble(
    variable = control_variables
  ) %>% 
    mutate(
      coefficient = ifelse(variable %in% unlist(reg$vars), 
                           "x", "")
    )
  
  rsq <- tibble(
    variable = "r_squared", 
    coefficient = reg$model %>% summary() %>% pluck("r.squared") %>% round(3) %>% format(nsmall = 2)
  )
  
  
  results <- tibble(variable = parameters) %>% 
    left_join(coef_table) %>% 
    mutate(coef = round(coef, 2) %>% format(nsmall = 2),
           std_error = round(std_error, 2) %>% format(nsmall = 2), 
           coefficient = case_when(
             p_val < 0.001 ~ paste0(coef, "***", " (", std_error, ")"), 
             p_val %in% 0.001:0.01 ~ paste0(coef, "**", " (", std_error, ")"), 
             p_val %in% 0.01:0.05 ~ paste0(coef, "*", " (", std_error, ")"), 
             p_val > 0.05 ~ paste0(coef, " (", std_error, ")")
           )) %>% 
    select(variable, coefficient) %>% 
    rbind(controlled) %>% 
    rbind(rsq) %>% 
    mutate(coefficient = ifelse(is.na(coefficient), "", coefficient)) %>% 
    pivot_wider(names_from = variable, values_from = coefficient)
  
  fmla <- reg$model$call %>% 
    as.character() 
  
  
  tibble(
    group = reg$data, 
    outcome = reg$outcome, 
    formula = fmla[2],
    model = list(reg$model),
    full_data = nest(reg$dataset)
  ) %>% 
    cbind(results)
  
}




# Making the Tibble -------------------------------------------------------

final_tibble <- {}

for(i in 1:nrow(setup)){
  final_tibble <- rbind(final_tibble, making_table_babies(i))

  progress <- round(100*i/2016, 2) %>% format(nsmall = 2) %>%
    paste0("% Complete")

  print(progress)
}

save(final_tibble, file = "data/regressions_info.rda")





# How Much Per Release ----------------------------------------------------

issuances <- left_join(
  full_pebt_dates %>% 
    select(-t) %>% 
    pivot_longer(cols = 2:3, names_to = "group", values_to = "issue") %>% 
    na.omit(issue),
  full_pebt_dates %>% 
    select(-t) %>% 
    pivot_longer(cols = 2:3, names_to = "group", values_to = "issue") %>% 
    na.omit(issue) %>% 
    group_by(state, group) %>% 
    summarize(total_issues = max(issue))
) 


specified <- issuances %>% 
  filter(total_issues > 1) %>% 
  left_join(pebt %>% 
              select(state, amt_issued1, amt_issued2, amt_issued3) %>% 
              pivot_longer(cols = 2:4, names_to = "issue", values_to = "amt") %>% 
              na.omit(amt) %>% 
              mutate(issue = str_extract(issue, "[1-3]")) 
  ) %>% 
  na.omit(amt) %>% 
  arrange(state, group, issue) %>% 
  select(-total_issues) %>% 
  mutate(amt = round(amt, 2))

## accounting for Delaware 
## nonsnap released in two groups, snap in three 
## given that the first nonsnap release is just three days 
## behind the second snap release, assumed that amt released 
## first for nonsnap is the sum of the first two releases of SNAP. 

specified <- specified %>% 
  filter(state == "Delaware" & group == "nonsnap") %>% 
  group_by(state, group) %>% 
  summarize(issue = "1", amt = sum(amt)) %>% 
  rbind(specified %>% 
          filter(state == "Delaware",  group == "nonsnap", issue == "2") %>% 
          mutate(amt = 57)) %>% 
  rbind(specified %>% 
          filter(state != "Delaware" | group == "snap"))


# Those with a singular issuance 
once_released <- issuances %>% 
  filter(total_issues == 1) %>% 
  left_join(pebt %>% select(state, max_ben)) %>% 
  mutate(amt = max_ben / as.numeric(total_issues)) %>% 
  select(state, group, issue, amt) %>% 
  unique()


# Unspecified states with multiple issuances 
do_not_includes <- specified %>% 
  mutate(index = paste0(state, group, issue)) %>% 
  pluck("index")


unspecified <- issuances %>% 
  filter(total_issues > 1) %>% 
  mutate(index = paste0(state, group, issue)) %>% 
  filter(index %notin% do_not_includes) %>% 
  select(-index) %>% 
  arrange(state, group, issue) %>% 
  left_join(pebt %>% select(state, max_ben)) %>% 
  mutate(amt = max_ben/as.numeric(total_issues)) %>% 
  select(-total_issues, -max_ben)


## combining 
full_amts <- rbind(specified, unspecified) %>% 
  rbind(once_released) 


## Checking for Completion 
# full_amts %>% 
#   mutate(group = paste0(group, issue)) %>% 
#   select(-issue) %>% 
#   pivot_wider(names_from = group, values_from = amt) %>% 
#   unnest() %>% 
#   arrange(state) %>% 
#   select(state, snap1, snap2, snap3, nonsnap1, nonsnap2) %>% 
#   View()

## Note: three states missing and we don't know anything about 
## their P-EBT releases

## Checking for value consistency 
# full_amts %>%
#   group_by(state, group) %>%
#   summarize(tot_amt = sum(amt)) %>%
#   pivot_wider(names_from = group, values_from = tot_amt)%>%
#   left_join(pebt %>% select(state, max_ben)) %>%
#   filter(nonsnap != max_ben)
# 


# Combining Amounts and Dates ---------------------------------------------

# full_pebt_data <- full_pebt_dates %>% 
#   left_join(full_amts %>% 
#               filter(group == "snap") %>% 
#               select(state, snap = issue, snap_amt = amt)) %>% 
#   left_join(full_amts %>% 
#               filter(group == "nonsnap") %>% 
#               select(state, nonsnap = issue, nonsnap_amt = amt)) %>% 
#   mutate_all(~ ifelse(is.na(.), 0, .)) %>%
#   # Getting an estimate on the number of kids benefitting 
#   left_join(pebt %>% select(state, est_kids))
# 


# Final Version: Just dates, application, and number of kids --------------
full_pebt_data <- full_pebt_dates %>% 
  mutate_all(~ ifelse(is.na(.), 0, .)) %>% 
  left_join(pebt %>% select(state, app, est_kids)) %>% 
  mutate(app_snap = ifelse(app == "all", 1, 0), 
         app_nonsnap = ifelse(app %in% c("all_but_snap", "all"), 1, 0)) %>% 
  select(-app)




# Getting Graphs for Presentation -----------------------------------------

# Loading Libraries 
library(tidyverse)
library(tidymodels)
library(Metrics)
library(gtsummary)

library(knitr)
library(kableExtra)


# Loading 

## Functions 
load("data_editing/functions.rda")

## Data 
loaded <- read_csv("data/datasets/final.csv")

# Setting not in 
`%notin%` <- negate(`%in%`)


# Removing Scientific Notation 
options(scipen = 999)




with_kids <- lens(with_kids = T)

# Hunger, Households With Kids
with_kids %>% 
  mutate(n_hungry = perc_hungry*people/100) %>% 
  group_by(t, month_num, month, pebt_1m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_1m = as.character(pebt_1m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_1m, t < 9),
             color = pebt_1m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                   labels = with_kids %>% arrange(month_num) %>% 
                     pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 21, 5), 
                     labels = paste0(seq(0, 21, 5), "%"), 
                     limits = c(0, 21)) + 
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )

# Hunger, Low Income Households With Kids
lens(with_kids = T, low_income = T) %>% 
  mutate(n_hungry = perc_hungry*people/100) %>% 
  group_by(t, month_num, month, pebt_1m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_1m = as.character(pebt_1m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_1m, t < 9),
             color = pebt_1m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                     labels = with_kids %>% arrange(month_num) %>% 
                       pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 35, 5),
                     labels = paste0(seq(0, 35, 5), "%"),
                     limits = c(0, 35)) +
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point(alpha = 0) + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )


# Food Insecurity, Low Income Households With Kids
lens(with_kids = T, low_income = T) %>% 
  mutate(n_hungry = perc_fi*people/100) %>% 
  group_by(t, month_num, month, pebt_1m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_1m = as.character(pebt_1m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_1m, t < 9),
             color = pebt_1m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                     labels = with_kids %>% arrange(month_num) %>% 
                       pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 50, 5),
                     labels = paste0(seq(0, 50, 5), "%"),
                     limits = c(0, 50)) +
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point(alpha = 0) + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )



# Graphs: 2 month horizon -------------------------------------------------

# Hunger, Households With Kids
lens(with_kids = T) %>% 
  mutate(n_hungry = perc_hungry*people/100) %>% 
  group_by(t, month_num, month, pebt_2m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_2m = as.character(pebt_2m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_2m, t < 9),
             color = pebt_2m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                     labels = with_kids %>% arrange(month_num) %>% 
                       pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 20, 5),
                     labels = paste0(seq(0, 20, 5), "%"),
                     limits = c(0, 20)) +
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point(alpha = 0) + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )

# Food Insecurity, Households With Kids
lens(with_kids = T) %>% 
  mutate(n_hungry = perc_fi*people/100) %>% 
  group_by(t, month_num, month, pebt_2m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_2m = as.character(pebt_2m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_2m, t < 9),
             color = pebt_2m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                     labels = with_kids %>% arrange(month_num) %>% 
                       pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 35, 5),
                     labels = paste0(seq(0, 35, 5), "%"),
                     limits = c(0, 35)) +
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point(alpha = 0) + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )

# Hunger, Low Income Households With Kids
lens(with_kids = T, low_income = T) %>% 
  mutate(n_hungry = perc_hungry*people/100) %>% 
  group_by(t, month_num, month, pebt_2m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_2m = as.character(pebt_2m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_2m, t < 9),
             color = pebt_2m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                     labels = with_kids %>% arrange(month_num) %>% 
                       pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 35, 5),
                     labels = paste0(seq(0, 35, 5), "%"),
                     limits = c(0, 35)) +
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point(alpha = 0) + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )

# Food Insecurity, Low Income Households With Kids
lens(with_kids = T, low_income = T) %>% 
  mutate(n_hungry = perc_fi*people/100) %>% 
  group_by(t, month_num, month, pebt_2m) %>% 
  summarize(n_hungry = sum(n_hungry),
            people = sum(people)) %>% 
  mutate(perc_hungry = 100*n_hungry/people,
         pebt_2m = as.character(pebt_2m)) %>% 
  ggplot(aes(x = t, y = perc_hungry, 
             group = interaction(pebt_2m, t < 9),
             color = pebt_2m, 
             weight = people)) + 
  scale_x_continuous(breaks = c(1, seq(2, 17, 2)), 
                     labels = with_kids %>% arrange(month_num) %>% 
                       pluck("month") %>% unique())+
  scale_y_continuous(breaks = seq(0, 60, 10),
                     labels = paste0(seq(0, 60, 10), "%"),
                     limits = c(0, 60)) +
  scale_color_manual(values = c("#4ba173", "#9abdf7")) + 
  geom_point(alpha = 0) + 
  geom_smooth(method = "lm", se = F, size = 2) + 
  theme_classic() + 
  theme(
    text = element_text(size = 14) 
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    color = "P-EBT Treated"
  )






# Graphs and Charts Doc ---------------------------------------------------




### Changing Response Rates


data <- all %>%
  select(t, month, month_num, w2, people,
         perc_bach_or_higher, perc_some_college, perc_highschool, perc_no_highschool,
         perc_asian, perc_black, perc_latino, perc_white, perc_other) %>%
  mutate_at(
    vars(perc_bach_or_higher, perc_some_college, perc_highschool, perc_no_highschool,
         perc_asian, perc_black, perc_latino, perc_white, perc_other),
    ~.*people/100) %>%
  group_by(t, month, month_num, w2) %>%
  summarize_all(sum) %>%
  mutate_at(
    vars(perc_bach_or_higher, perc_some_college, perc_highschool, perc_no_highschool,
         perc_asian, perc_black, perc_latino, perc_white, perc_other),
    ~.*100/people) %>%
  select(-people) %>%
  pivot_longer(cols = 5:13, names_to = "group", values_to = "perc") %>%
  mutate(group = str_remove(group, "perc_") %>%
           str_replace_all("_", " "),
         stat_group = ifelse(
           group %in% c("asian", "black", "white", "latino", "other"),
           "race/ethnicity",
           "education"
         ),
         month = capitalize(month),
         group = capitalize(group))

data %>%
  filter(stat_group == "race/ethnicity") %>%
  ggplot(aes(x = t, y = perc, color = group, group = interaction(group, t > 8))) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, 17, 2),
                     labels = data %>% arrange(t) %>% pluck("month") %>% unique()) +
  scale_y_continuous(breaks = seq(0, 65, 15),
                     labels = seq(0, 65, 15) %>% paste0("%")) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  )

data %>% 
  filter(stat_group == "education") %>% 
  ggplot(aes(x = t, y = perc, color = group, group = interaction(group, t > 8))) +    
  geom_point() +    
  geom_line() +    
  theme_classic() +    
  scale_x_continuous(breaks = seq(1, 17, 2),    
                     labels = data %>% arrange(t) %>% pluck("month") %>% unique()) +    
  scale_y_continuous(breaks = seq(0, 50, 10),   
                     labels = seq(0, 50, 10) %>% paste0("%")) +   
  labs(   
    x = NULL,    
    y = NULL,    
    color = NULL   
  ) 





loaded %>%
  mutate(low_income = ifelse(is.na(low_income), "missing", "present")) %>%
  group_by(t, month, month_num, low_income) %>%
  summarize(people = sum(people)) %>%
  pivot_wider(names_from = low_income, values_from = people) %>%
  mutate(perc_missing = 100*missing/(missing + present)) %>%
  ggplot(aes(x = t, y = perc_missing)) +
  geom_point() +
  geom_line() +
  theme_classic()



### Who's Considered Low Income?


### Monthly SNAP Participation

snap_monthly <- read_csv("data/snap_monthly.csv")


graph_data <- snap_monthly %>%
  group_by(month) %>%
  summarize(n_snap = sum(n_snap)) %>%
  left_join(all %>% select(month, month_num) %>% unique()) %>%
  mutate(month_num = case_when(
    month == "january" ~ 1,
    month == "february" ~ 2,
    month == "march" ~ 3,
    TRUE ~ month_num
  ),
  month = substring(month, 1, 1) %>%
    toupper() %>%
    paste0(
      substring(month, 2, 3),
      "."
    ))

graph_data %>%
  ggplot(aes(x = month_num, y = n_snap/1000000)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:12,
                     labels = graph_data %>% arrange(month_num) %>% pluck("month") %>% unique()) +
  scale_y_continuous(breaks = seq(0, 25, 5),
                     limits = c(0, 25)) +
  theme_classic() +
  labs(x = "Month", y = "SNAP Participants (millions)")










# Appendix: Lagged SNAP Variable


### Lagged SNAP Variable


safinl <- lm(perc_fi ~ perc_snap + perc_snap_m1,
             weights = people,
             data = all)

safil <- lm(perc_fi ~ perc_snap + perc_snap_m1 + t1 + t2 + state +
              perc_asian + perc_black + perc_latino + perc_other +
              perc_some_college + perc_highschool + perc_no_highschool,
            weights = people,
            data = all)

sahnl <- lm(perc_hungry ~ perc_snap + perc_snap_m1,
            weights = people,
            data = all)

sahl <- lm(perc_hungry ~ perc_snap + perc_snap_m1 + t1 + t2 + state +
             perc_asian + perc_black + perc_latino + perc_other +
             perc_some_college + perc_highschool + perc_no_highschool,
           weights = people,
           data = all)

sancnl <- lm(perc_nonconf ~ perc_snap + perc_snap_m1,
             weights = people,
             data = all)

sancl <- lm(perc_nonconf ~ perc_snap + perc_snap_m1 + t1 + t2 + state +
              perc_asian + perc_black + perc_latino + perc_other +
              perc_some_college + perc_highschool + perc_no_highschool,
            weights = people,
            data = all)

making_nice(
  list(safinl, safil, sahnl, sahl, sancnl, sancl),
  c("a", "b", "d", "e", "f", "g"),
  override_names = T,
  c(" " = 1, "Food Insecurity" = 2 , "Hunger" = 2 , "Food Non-Confidence" = 2)
)


# Appendix: Exploring Time Trends

all_graphs <- loaded %>%
  group_by(t, month, month_num) %>%
  summarize(
    n_hungry = sum(n_hungry),
    n_fi = sum(n_fi),
    n_nonconf = sum(n_nonconf),
    sufficiency_respondents = sum(sufficiency_respondents),
    confidence_respondents = sum(confidence_respondents),
    people = sum(people)
  ) %>%
  mutate(
    perc_hungry = 100*n_hungry/sufficiency_respondents,
    perc_fi = 100*n_fi/sufficiency_respondents,
    perc_nonconf = 100*n_nonconf/confidence_respondents,
    dataset = "all"
  ) %>%
  select(t, month, month_num, perc_hungry, perc_fi, perc_nonconf, dataset)

kids_graphs <- loaded %>%
  filter(with_kids == 1) %>%
  group_by(t, month, month_num) %>%
  summarize(
    n_hungry = sum(n_hungry),
    n_fi = sum(n_fi),
    n_nonconf = sum(n_nonconf),
    sufficiency_respondents = sum(sufficiency_respondents),
    confidence_respondents = sum(confidence_respondents),
    people = sum(people)
  ) %>%
  mutate(
    perc_hungry = 100*n_hungry/sufficiency_respondents,
    perc_fi = 100*n_fi/sufficiency_respondents,
    perc_nonconf = 100*n_nonconf/confidence_respondents,
    dataset = "with_kids"
  ) %>%
  select(t, month, month_num, perc_hungry, perc_fi, perc_nonconf, dataset)

low_income_graphs <- loaded %>%
  filter(low_income == 1) %>%
  group_by(t, month, month_num) %>%
  summarize(
    n_hungry = sum(n_hungry),
    n_fi = sum(n_fi),
    n_nonconf = sum(n_nonconf),
    sufficiency_respondents = sum(sufficiency_respondents),
    confidence_respondents = sum(confidence_respondents),
    people = sum(people)
  ) %>%
  mutate(
    perc_hungry = 100*n_hungry/sufficiency_respondents,
    perc_fi = 100*n_fi/sufficiency_respondents,
    perc_nonconf = 100*n_nonconf/confidence_respondents,
    dataset = "low_income"
  ) %>%
  select(t, month, month_num, perc_hungry, perc_fi, perc_nonconf, dataset)


data <- rbind(all_graphs, kids_graphs) %>%
  rbind(low_income_graphs) %>%
  mutate(dataset = case_when(
    dataset == "all" ~ "All Households",
    dataset == "with_kids" ~ "Households With Kids",
    dataset == "low_income" ~ "Low Income Households"
  ))


boxplot_data <- loaded %>%
  group_by(t, month, month_num, state) %>%
  summarize(
    n_hungry = sum(n_hungry),
    n_fi = sum(n_fi),
    n_nonconf = sum(n_nonconf),
    sufficiency_respondents = sum(sufficiency_respondents),
    confidence_respondents = sum(confidence_respondents),
    people = sum(people)
  ) %>%
  mutate(
    perc_hungry = 100*n_hungry/sufficiency_respondents,
    perc_fi = 100*n_fi/sufficiency_respondents,
    perc_nonconf = 100*n_nonconf/confidence_respondents,
    dataset = "all"
  ) %>%
  select(t, month, month_num, perc_hungry, perc_fi, perc_nonconf, dataset)


#### Food Insecurity



data %>%
  ggplot(aes(x = t, y = perc_fi, color = dataset, group = interaction(t < 9, dataset))) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  theme_classic() +
  labs(
    x = "BiWeekly Period t",
    y = "Food Insecurity (%)",
    color = NULL
  ) +
  scale_color_manual(values = c("#4E2A84", "#FFC520", "#7FCECD"))


boxplot_data %>%
  ggplot(aes(x = month_num %>% as.factor(), y = perc_fi)) +
  geom_boxplot() +
  scale_x_discrete(labels = boxplot_data %>% arrange(month_num) %>% pluck("month") %>% unique()) +
  labs(
    y = "Food Insecure (%)",
    x = "Month"
  ) +
  theme_classic()

#### Hunger
ata %>%
  ggplot(aes(x = t, y = perc_hungry, color = dataset, group = interaction(t < 9, dataset))) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  theme_classic() +
  labs(
    color = "Households",
    x = "BiWeekly Period t",
    y = "Hunger (%)"
  )


boxplot_data %>%
  ggplot(aes(x = month_num %>% as.factor(), y = perc_hungry)) +
  geom_boxplot() +
  scale_x_discrete(labels = boxplot_data %>% arrange(month_num) %>% pluck("month") %>% unique()) +
  labs(
    y = "Hunger (%)",
    x = "Month"
  ) +
  theme_classic()



#### Food Non-Confidence


data %>%
  ggplot(aes(x = t, y = perc_nonconf, color = dataset, group = interaction(t < 9, dataset))) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  theme_classic() +
  labs(
    color = "Households",
    x = "BiWeekly Period t",
    y = "Food Non-Confidence (%)"
  )


boxplot_data %>%
  ggplot(aes(x = month_num %>% as.factor(), y = perc_nonconf)) +
  geom_boxplot() +
  scale_x_discrete(labels = boxplot_data %>% arrange(month_num) %>% pluck("month") %>% unique()) +
  labs(
    y = "Food Non-Confidence (%)",
    x = "Month"
  ) +
  theme_classic()




### SNAP Food Insecurity, All Households


snap_all_naive <- lm(perc_fi ~ perc_snap,
                     data = all, weights = people)

snap_t <- lm(perc_fi ~ perc_snap + t + state +
               perc_asian + perc_black + perc_latino + perc_other +
               perc_some_college + perc_highschool + perc_no_highschool,
             data = all, weights = people)

snap_2t <- lm(perc_fi ~ perc_snap + t1 + t2 + w2 + state +
                perc_asian + perc_black + perc_latino + perc_other +
                perc_some_college + perc_highschool + perc_no_highschool,
              data = all, weights = people)

snap_month <- lm(perc_fi ~ perc_snap + month + state +
                   perc_asian + perc_black + perc_latino + perc_other +
                   perc_some_college + perc_highschool + perc_no_highschool,
                 data = all, weights = people)

snap_t_dummy <- lm(perc_fi ~ perc_snap + t_dummy + state +
                     perc_asian + perc_black + perc_latino + perc_other +
                     perc_some_college + perc_highschool + perc_no_highschool,
                   data = all, weights = people)

making_nice(
  list(snap_all_naive, snap_t, snap_2t, snap_month, snap_t_dummy),
  c("Naive Regression", "Single Time Trend", "Two Time Trends", "Month Fixed Effects", "Bi-weekly Dummy")
)



### P-EBT Food Insecurity

Note: Time Horizon = 2 Months, Households With Children


pfins <- lm(perc_fi ~ pebt,
            data = with_kids, weights = people)

pfits <- lm(perc_fi ~ pebt*t + state +
              perc_asian + perc_black + perc_latino + perc_other +
              perc_some_college + perc_highschool + perc_no_highschool,
            data = with_kids %>% change_pebt("two months"), weights = people)


pfi2ts <- lm(perc_fi ~ pebt*t1 + pebt*t2 + state + w2 +
               perc_asian + perc_black + perc_latino + perc_other +
               perc_some_college + perc_highschool + perc_no_highschool,
             data = with_kids%>% change_pebt("two months"), weights = people)

pfims <- lm(perc_fi ~ pebt*month + state +
              perc_asian + perc_black + perc_latino + perc_other +
              perc_some_college + perc_highschool + perc_no_highschool,
            data = with_kids%>% change_pebt("two months"), weights = people)

making_nice(
  list(pfins, pfits, pfi2ts, pfims),
  c("Naive Regression", "Single Time Trend", "Two Time Trends", "Month Fixed Effects")
)






# Old Functions -----------------------------------------------------------


sorting_hat <- function(li = F, wk = F){
  
  house <- loaded 
  
  if(li == T){
    house <- house %>% 
      filter(low_income == 1)
  }
  
  if(wk == T){
    house <- house %>% 
      filter(with_kids == 1)
  }
  
  house %>% 
    group_by(t, month, month_num, 
             state, race, education, 
             pebt, app_snap, app_nonsnap) %>% 
    summarize(
      n_snap = sum(n_snap),
      n_hungry = sum(n_hungry),
      n_nonconf = sum(n_nonconf), 
      n_fi = sum(n_fi),
      sufficiency_respondents = sum(sufficiency_respondents), 
      confidence_respondents = sum(confidence_respondents), 
      people = sum(people),
      .groups = "drop"
    )
}


# Observation Level Aggregation -------------------------------------------

# Determining Observation Levels
# Default is State and NOT race or education

lens <- function(race = F, education = F, low_income = F, with_kids = F){
  
  data <- sorting_hat(li = low_income, wk = with_kids)
  
  if(race == T & education == T){
    
    prescription <- data  %>% 
      mutate(perc_snap = 100*n_snap/people, 
             perc_hungry = 100*n_hungry/sufficiency_respondents,
             perc_nonconf = 100*n_nonconf/confidence_respondents, 
             perc_fi = 100*n_fi/sufficiency_respondents) %>% 
      select(-n_snap, -n_hungry, -n_nonconf, -n_fi,
             -confidence_respondents, -sufficiency_respondents)
    
    
    glasses <- left_join(prescription,
                         prescription %>% 
                           select(t, state, race, education, perc_snap) %>% 
                           mutate(t = t + 2) %>% 
                           rename("perc_snap_m1" = "perc_snap") %>% 
                           filter(t <= 17))
    
  }
  
  if(race == T & education == F){
    
    edu_counts <- data %>% 
      group_by(t, month, month_num, race, 
               state, education) %>% 
      summarize(people = sum(sufficiency_respondents)) %>% 
      pivot_wider(names_from = education, values_from = people) %>% 
      mutate_all(~ifelse(is.na(.), 0, .)) %>% 
      mutate(
        edu_respondents = bach_or_higher + some_college + highschool_degree + no_highschool_degree,
        perc_bach_or_higher = 100*bach_or_higher/edu_respondents,
        perc_some_college = 100*some_college/edu_respondents,
        perc_highschool = 100*highschool_degree/edu_respondents,
        perc_no_highschool = 100*no_highschool_degree/edu_respondents
      ) %>% 
      select(t, month, month_num, state, race, 
             perc_bach_or_higher, perc_some_college, 
             perc_highschool, perc_no_highschool)
    
    prescription <- data %>% 
      group_by(t, month, month_num, 
               state, race, pebt, 
               app_snap, app_nonsnap) %>% 
      summarize(
        n_snap = sum(n_snap),
        n_hungry = sum(n_hungry),
        n_nonconf = sum(n_nonconf), 
        n_fi = sum(n_fi),
        sufficiency_respondents = sum(sufficiency_respondents), 
        confidence_respondents = sum(confidence_respondents), 
        people = sum(people),
        .groups = "drop"
      )  %>% 
      mutate(
        # Predictor 
        perc_snap = 100*n_snap/people, 
        # Outcome Variables
        perc_hungry = 100*n_hungry/sufficiency_respondents,
        perc_nonconf = 100*n_nonconf/confidence_respondents, 
        perc_fi = 100*n_fi/sufficiency_respondents
      ) %>% 
      left_join(edu_counts) %>%
      select(-n_snap, -n_hungry, -n_nonconf, -n_fi,
             -confidence_respondents, -sufficiency_respondents)
    
    
    glasses <- left_join(prescription,
                         prescription %>% 
                           select(t, state, race, perc_snap) %>% 
                           mutate(t = t + 2) %>% 
                           rename("perc_snap_m1" = "perc_snap") %>% 
                           filter(t <= 17)) 
    
  }
  
  if(race == F & education == T){
    
    race_counts <- data %>% 
      group_by(t, month, month_num, 
               state, education, race) %>% 
      summarize(people = sum(sufficiency_respondents)) %>% 
      mutate(race = str_extract(race, "(Asian|Black|Latino|Other|White)") %>%
               tolower() %>% 
               paste0("perc_", .)) %>% 
      pivot_wider(names_from = race, values_from = people) %>% 
      mutate_all(~ifelse(is.na(.), 0, .)) %>% 
      mutate(race_respondents = perc_asian + perc_black + perc_latino + perc_other + perc_white,
             perc_asian = 100*perc_asian/race_respondents, 
             perc_black = 100*perc_black/race_respondents,
             perc_latino = 100*perc_latino/race_respondents,
             perc_other = 100*perc_other/race_respondents,
             perc_white = 100*perc_white/race_respondents
      ) %>% 
      select(-race_respondents)
    
    prescription <- data %>% 
      group_by(t, month, month_num, 
               state, education, pebt, 
               app_snap, app_nonsnap) %>% 
      summarize(
        n_snap = sum(n_snap),
        n_hungry = sum(n_hungry),
        n_nonconf = sum(n_nonconf), 
        n_fi = sum(n_fi),
        sufficiency_respondents = sum(sufficiency_respondents), 
        confidence_respondents = sum(confidence_respondents), 
        people = sum(people),
        .groups = "drop"
      )  %>% 
      left_join(race_counts) %>% 
      mutate(
        perc_snap = 100*n_snap/people, 
        perc_hungry = 100*n_hungry/sufficiency_respondents,
        perc_nonconf = 100*n_nonconf/confidence_respondents, 
        perc_fi = 100*n_fi/sufficiency_respondents
      ) %>% 
      select(-n_snap, -n_hungry, -n_nonconf, -n_fi,
             -confidence_respondents, -sufficiency_respondents)
    
    
    glasses <- left_join(prescription,
                         prescription %>% 
                           select(t, state, education, perc_snap) %>% 
                           mutate(t = t + 2) %>% 
                           rename("perc_snap_m1" = "perc_snap") %>% 
                           filter(t <= 17))
    
  }
  
  if(race == F & education == F){
    
    edu_counts <- data %>% 
      group_by(t, month, month_num, 
               state, education) %>% 
      summarize(people = sum(sufficiency_respondents)) %>% 
      pivot_wider(names_from = education, values_from = people) %>% 
      mutate_all(~ifelse(is.na(.), 0, .)) %>% 
      mutate(
        edu_respondents = bach_or_higher + some_college + highschool_degree + no_highschool_degree,
        perc_bach_or_higher = 100*bach_or_higher/edu_respondents,
        perc_some_college = 100*some_college/edu_respondents,
        perc_highschool = 100*highschool_degree/edu_respondents,
        perc_no_highschool = 100*no_highschool_degree/edu_respondents
      ) %>% 
      select(t, month, month_num, state, 
             perc_bach_or_higher, perc_some_college, 
             perc_highschool, perc_no_highschool)
    
    race_counts <- data %>% 
      group_by(t, month, month_num, 
               state, race) %>% 
      summarize(people = sum(sufficiency_respondents)) %>% 
      mutate(race = str_extract(race, "(Asian|Black|Latino|Other|White)") %>%
               tolower() %>% 
               paste0("perc_", .)) %>% 
      pivot_wider(names_from = race, values_from = people) %>% 
      mutate_all(~ifelse(is.na(.), 0, .)) %>% 
      mutate(race_respondents = perc_asian + perc_black + perc_latino + perc_other + perc_white,
             perc_asian = 100*perc_asian/race_respondents, 
             perc_black = 100*perc_black/race_respondents,
             perc_latino = 100*perc_latino/race_respondents,
             perc_other = 100*perc_other/race_respondents,
             perc_white = 100*perc_white/race_respondents
      ) %>% 
      select(-race_respondents)
    
    prescription <- data %>% 
      group_by(t, month, month_num, 
               state, 
               pebt, 
               app_snap, app_nonsnap) %>% 
      summarize(
        n_snap = sum(n_snap),
        n_hungry = sum(n_hungry),
        n_nonconf = sum(n_nonconf), 
        n_fi = sum(n_fi),
        sufficiency_respondents = sum(sufficiency_respondents), 
        confidence_respondents = sum(confidence_respondents), 
        people = sum(people),
        .groups = "drop"
      )  %>% 
      mutate(perc_snap = 100*n_snap/people, 
             perc_hungry = 100*n_hungry/sufficiency_respondents,
             perc_nonconf = 100*n_nonconf/confidence_respondents, 
             perc_fi = 100*n_fi/sufficiency_respondents) %>% 
      select(-n_snap, -n_hungry, -n_nonconf, -n_fi,
             -confidence_respondents, -sufficiency_respondents) %>% 
      left_join(edu_counts) %>% 
      left_join(race_counts)
    
    glasses <- left_join(prescription,
                         prescription %>% 
                           select(t, state, perc_snap) %>% 
                           mutate(t = t + 2) %>% 
                           rename("perc_snap_m1" = "perc_snap") %>% 
                           filter(t <= 17))
  }
  
  # Getting a dummy for whether its the week after p-ebt 
  pebt_postweek_df <- glasses %>% 
    select(t, state, pebt) %>% 
    filter(pebt == 1) %>% 
    mutate(
      t = t + 1
    ) %>% 
    rename(pebt_post = pebt) %>% 
    unique()
  
  # Getting a dummy for both the week of and week after pebt 
  pebt_1m_df <- glasses %>% 
    select(t, state, pebt) %>% 
    filter(pebt == 1) %>% 
    rename(pebt_1m = pebt) %>% 
    rbind(pebt_postweek_df %>% rename(pebt_1m = pebt_post)) %>% 
    unique()
  
  # Getting a dummy for a two-month horizon of p-ebt 
  pebt_2m_df <- pebt_1m_df %>% 
    mutate(t = t + 2) %>% 
    rbind(pebt_1m_df) %>% 
    rename(pebt_2m = pebt_1m) %>% 
    unique()
  
  # Attaching different P-EBT Variables 
  glasses %>% 
    left_join(pebt_postweek_df) %>% 
    left_join(pebt_1m_df) %>% 
    left_join(pebt_2m_df) %>% 
    mutate(
      pebt_post = ifelse(is.na(pebt_post), 0, pebt_post), 
      pebt_1m = ifelse(is.na(pebt_1m), 0, pebt_1m), 
      pebt_2m = ifelse(is.na(pebt_2m), 0, pebt_2m),
      # Alternative Time Trends
      t1 = ifelse(t < 9, t, 0), 
      t2 = ifelse(t >= 9, t-8, 0), 
      w2 = ifelse(t >= 9, 1, 0),
      t_dummy = as.factor(t)
    ) %>%
    arrange(state, t)
  
}










