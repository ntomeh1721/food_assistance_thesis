

# Set Up ------------------------------------------------------------------

# Loading Libraries 
library(tidyverse)
library(tidymodels)
library(Metrics)
library(stringr)

library(knitr)
library(kableExtra)


# Loading Data 
loaded <- read_csv("data/datasets/final.csv")

# Setting not in 
`%notin%` <- negate(`%in%`)


# Removing Scientific Notation 
options(scipen = 999)


# Filtering Data Function -------------------------------------------------

# Filtering the data for some combination of low_income households 
# and households with kids 
# Default is all households

lens <- function(low_income = F, with_kids = F){
  
  
  house <- loaded 
  
  if(low_income == T){
    house <- house %>% 
      filter(low_income == 1)
  }
  
  if(with_kids == T){
    house <- house %>% 
      filter(with_kids == 1)
  }
  
  new_house <- house %>% 
    select(-low_income, -with_kids) %>% 
    group_by(t, month, month_num, state, pebt) %>% 
    summarize_all(sum) %>% 
    mutate_at(vars(n_hungry, n_fi, bach_or_higher, highschool_degree, 
                   no_highschool_degree, some_college, 
                   asian, black, latino, other, white),
              ~100*./sufficiency_respondents) %>% 
    mutate_at(vars(n_nonconf), ~100*./confidence_respondents) %>%
    mutate_at(vars(n_snap), ~100*./people) %>% 
    select(-sufficiency_respondents, - confidence_respondents)  %>% 
    ungroup()
  
  names(new_house) <- names(new_house) %>% str_replace("n_", "perc_") 
  
  names(new_house) <- names(new_house) %>% str_remove("_degree")
  
  names(new_house) <- c(names(new_house)[1:9],
                        names(new_house)[10:18] %>% 
                          paste0("perc_", .),
                        names(new_house)[19])
  
  new_house <- left_join(new_house,
                         new_house %>% 
                         select(t, state, perc_snap) %>% 
                         mutate(t = t + 2) %>% 
                         rename("perc_snap_m1" = "perc_snap") %>% 
                         filter(t <= 17))
  
  # Getting a dummy for whether its the week after p-ebt 
  pebt_postweek_df <- new_house %>% 
    select(t, state, pebt) %>% 
    filter(pebt == 1) %>% 
    mutate(
      t = t + 1
    ) %>% 
    rbind(tibble(
      t = 9, 
      state = c("Colorado", "Georgia", "Utah", "Nebraska"), 
      pebt = 1
    )) %>% 
    rename(pebt_post = pebt) %>% 
    unique()
  
  # Getting a dummy for both the week of and week after pebt 
  pebt_1m_df <- new_house %>% 
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
    unique() %>% 
    rbind(tibble(
      t = 10, 
      state = c("Colorado", "Georgia", "Utah", "Nebraska"), 
      pebt_2m = 1
    ))
  
  # Attaching different P-EBT Variables 
  new_house %>% 
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

# Getting Different P-EBT Variable ----------------------------------------
change_pebt <- function(df, time_period) {
  
  if(time_period == "week of"){
    new_pebt_df <- df 
  }
  
  if(time_period == "week after"){
    new_pebt_df <- df %>% 
      select(-pebt) %>% 
      rename(pebt = pebt_post) 
  }
  
  if(time_period == "one month"){
    new_pebt_df <- df %>% 
      select(-pebt) %>% 
      rename(pebt = pebt_1m) 
  }
  
  if(time_period == "two months"){
    new_pebt_df <- df %>% 
      select(-pebt) %>% 
      rename(pebt = pebt_2m) 
  }
  
  new_pebt_df
}


# Regressions Table  ------------------------------------------------------

## Variable Guide For Set Up 
## Edit when P-EBT is added
var_guide <- tibble(
  name = c(
    "P-EBT", 
    "P-EBT, Lagged", 
    "P-EBT, 1 Month", 
    "P-EBT, 2 Months", 
    "P-EBT X Time, t",
    "P-EBT X Wave 1, t",
    "P-EBT X Wave 2, t",
    "P-EBT X Month (Apr)", 
    "P-EBT X Month (May)", 
    "P-EBT X Month (Jun)", 
    "P-EBT X Month (Jul)", 
    "P-EBT X Month (Aug)", 
    "P-EBT X Month (Sep)", 
    "P-EBT X Month (Oct)", 
    "P-EBT X Month (Nov)", 
    "P-EBT X Month (Dec)", 
    "SNAP Rate (%)",
    "SNAP Rate (%), Lagged",
    "Biweekly Period, t",
    "Wave 1, t",
    "Wave 2, t",
    "Wave 2 Constant", 
    "Constant", 
    "State",
    "Month", 
    "Biweekly Period, t  ", 
           "Race/Ethnicity", 
           "Education"), 
  variable = c(
    "pebt", 
    "pebt_post", 
    "pebt_1m", 
    "pebt_2m", 
    "pebt:t", 
    "pebt:t1", 
    "pebt:t2",
    "pebt:monthapril",
    "pebt:monthmay",
    "pebt:monthjune",
    "pebt:monthjuly",
    "pebt:monthaugust",
    "pebt:monthseptember",
    "pebt:monthoctober",
    "pebt:monthnovember",
    "pebt:monthdecember",
    "perc_snap", 
    "perc_snap_m1",
    "t",
    "t1", 
    "t2", 
    "w2", 
    "(Intercept)",
    "state", 
    "month", 
    "t_dummy", 
    "race",
    "education"
  ) 
)

## Function
table_read <- function(mod, model_name = "Input Model"){
    
  reg_df <- mod %>% 
    tidy() %>% 
    mutate(
      variable = case_when(
        str_detect(term, "snap|pebt|(Intercept)|^t$|t([1-2])$|^w2$") ~ term,
        str_detect(term, "state") ~ "state",
        str_detect(term, "t_dummy") ~ "t_dummy", 
        str_detect(term, "^month") ~ "month",
        str_detect(term, "college|school|higher|education") ~ "education", 
        str_detect(term, "asian|black|white|latino|other|race") ~ "race"
      ))
  
  if("t" %in% reg_df$variable) {
    reg_df <- reg_df %>% 
      filter(variable != "t1", 
             variable != "t2")
  }
  
  reg_table <- reg_df %>% 
    filter(str_detect(variable, "pebt|snap|^t$|t([1-2])$|(Intercept)|^w2$"))  %>% 
    mutate(
      coefficient = case_when(
        p.value < 0.001 ~ paste0(
          estimate %>% round(3) %>% format(nsmall = 3), 
          "\\***", 
          " <br> (",
          std.error %>% round(3) %>% format(nsmall = 3), 
          ") "
        ), 
        p.value >= 0.001 & p.value < 0.01 ~ paste0(
          estimate %>% round(3) %>% format(nsmall = 3), 
          "\\**", 
          " <br> (",
          std.error %>% round(3) %>% format(nsmall = 3), 
          ") "
        ), 
        p.value >= 0.01 & p.value < 0.05 ~ paste0(
          estimate %>% round(3) %>% format(nsmall = 3), 
          "\\*", 
          " <br> (",
          std.error %>% round(3) %>% format(nsmall = 3), 
          ") "
        ), 
        p.value >= 0.05 ~ paste0(
          estimate %>% round(3) %>% format(nsmall = 3), 
          " <br> (",
          std.error %>% round(3) %>% format(nsmall = 3), 
          ") "
        ), 
        )) %>% 
    select(variable, coefficient)
  
  if(length(reg_df %>% 
     filter(variable %in% c("state", "month", "t_dummy", "race", "education")) %>% 
     pluck("variable") %>% unique()) > 0){
    
    reg_table <- reg_table %>% 
      rbind(
      tibble(
        variable =  reg_df %>% 
          filter(variable %in% c("state", "month", "t_dummy", "race", "education")) %>% 
          pluck("variable") %>% unique(), 
        coefficient = "x"
      )
    )
  }
    
  
  metrics <- tibble(
    name = c("Observations", "R-Squared"),
    coefficient = c(
      nobs(mod), 
      mod %>% summary() %>% pluck("r.squared") %>% round(3) %>% format(nsmall = 3))
  )
  
  reg_table <- left_join(var_guide, reg_table) %>% 
    select(name, coefficient) %>% 
    rbind(metrics)
  
  names(reg_table) <- c("Variable", model_name)
  
  reg_table %>% 
    na.omit()
  
  
}


# Making Pretty Regression Tables  ----------------------------------------

making_nice <- function(
  models, # List of Models 
  model_names, # Vector of Model Names,
  override_names = F, 
  header_row_titles = c(), # Vector Denoting Header Table Info
  header_row_titles2 = c(), 
  naive = F
){
  
  tables_allread <- table_read(models[[1]], model_names[[1]]) 
  
  if(length(models) > 1){
    
    for(i in 2:length(models)){
      
      tables_allread <- merge(
        tables_allread,
        table_read(models[[i]], model_names[[i]]),
        by = "Variable", 
        all.x = T, 
        all.y = T
      )
    }
    
  }

  
  # Changing Missing Variables to Blank Spaces 
  tables_allread <- tables_allread  %>% 
    mutate(Variable = factor(Variable, levels = c(var_guide$name, "Observations", "R-Squared"))) %>% 
    arrange(Variable) %>% 
    mutate_if(is.character, ~ifelse(is.na(.), " ", .)) %>% 
    unique()
  
  # Row Packing
  how_deep1 <- tables_allread %>% 
    filter(Variable %in% c("P-EBT", 
                           "P-EBT, Lagged", 
                           "P-EBT, 1 Month", 
                           "P-EBT, 2 Month", 
                           "P-EBT X Time, t",
                           "P-EBT X Wave 1, t",
                           "P-EBT X Wave 2, t",
                           "P-EBT X Month (Apr)", 
                           "P-EBT X Month (May)", 
                           "P-EBT X Month (Jun)", 
                           "P-EBT X Month (Jul)", 
                           "P-EBT X Month (Aug)", 
                           "P-EBT X Month (Sep)", 
                           "P-EBT X Month (Oct)", 
                           "P-EBT X Month (Nov)", 
                           "P-EBT X Month (Dec)", 
                           "SNAP Rate (%)",
                           "SNAP Rate (%), Lagged",
                           "Biweekly Period, t",
                           "Wave 1, t",
                           "Wave 2, t",
                           "Wave 2 Constant", 
                           "Constant")) %>% 
    nrow() 
  
  how_deep2 <- tables_allread %>% 
    filter(Variable %in% c("State",
                           "Month", 
                           "Biweekly Period, t  ", 
                           "Race/Ethnicity", 
                           "Education")) %>% 
    nrow() 
  
  
  # Column Names 
  if(override_names == T){
    
    names(tables_allread) <- rep(" ", ncol(tables_allread))
    
  }
  
  # Final Table 
  final_kable <- kable(tables_allread, escape = F) %>% 
    kable_classic(html_font = "Times")  %>% 
    footnote(general_title = "Significance Codes",
             general = "p < 0.001 (\\***); p < 0.01 (\\**); p < 0.05 (\\*)") %>% 
    add_header_above(header_row_titles) %>% 
    add_header_above(header_row_titles2) 
  
  if(naive == T){
    final_kable %>% 
      pack_rows(index = c(" " = how_deep1, 
                          "Metrics" = 2))
      
  } else {
    final_kable %>% 
      pack_rows(index = c(" " = how_deep1, 
                          "Fixed Effects" = how_deep2, 
                          "Metrics" = 2))
  }
  
}



# Saving Functions --------------------------------------------------------

save(lens, change_pebt, 
     var_guide, table_read, making_nice,  
     file = "data_editing/functions.rda")
