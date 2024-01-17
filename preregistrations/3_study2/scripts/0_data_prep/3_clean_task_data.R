
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)
library(sjlabelled)

# Load data ---------------------------------------------------------------

load(here("data", "3_study2", "0_task_data_raw.Rdata"))

source("preregistrations/3_study2/scripts/custom_functions/functions_exclusions.R")

# Planned exclusions ------------------------------------------------------

## Flanker ----

flanker_data_clean <- flanker_data %>%
    group_by(id) %>%
    mutate(
      # Is accuracy above the cut-off for performance at chance level?
      ex_narb_acc_below_cutoff = ifelse( (sum(correct, na.rm=T)/n())*100 < gbinom(64, 0.5), TRUE, FALSE),
      # Do all participants have the expected number of trials? 
      ex_narb_rowcount         = n(),
      # Number of missing RTs per participant
      ex_narb_NA_trials        = ifelse(n() > 1, sum(is.na(rt)), NA), 
      
      ex_narb_log_outlier      = ifelse(abs(scale(log(rt))) > 3.2, TRUE, FALSE),
      # Number of outliers (>3.2SD, based on log-transformed RTs) per subject
      ex_narb_log_outliers_n   = ifelse(n() > 1, sum(scale(log(rt)) > 3.2), NA),
      # Number of trials with fast (<250 ms) or slow (>3500 ms) outliers
      ex_narb_invalid_trial      = ifelse(rt < 3500 & rt > 250, FALSE, TRUE),
      ex_narb_invalid_trials_n   = ifelse(n() > 1, sum(rt > 3500 | rt < 250, na.rm = TRUE), NA),
    ) %>%
    ungroup() %>%
    mutate(
      # Exclude participants with more than 10 excluded trials, who did not complete the task, or who performed at chance level
      ex_narb_flanker_pass   = ifelse(across(matches("ex_narb_(NA_trials|log_outliers|invalid_trials)")) %>% rowSums(., na.rm = TRUE) > 10 | 
                                        ex_narb_rowcount %in% c(1,2) |
                                        ex_narb_acc_below_cutoff == TRUE, 
                                        FALSE, TRUE)
    ) %>%
  left_join(browser_interactions_summary %>% select(id, event_during_flanker))

# Apply exclusions --------------------------------------------------------

flanker_data_clean %<>%
  # Remove invalid trials
  filter(!ex_narb_log_outlier) %>%
  filter(!ex_narb_invalid_trial) %>%
  # Exclude participants who did not pass the quality checks
  filter(ex_narb_flanker_pass == TRUE) %>%
  # Exclude participants with blur events during the task
  filter(!event_during_flanker) %>%
  select(-starts_with("ex_narb"))

# Compute average RTs and accuracy ----------------------------------------

flanker_data_clean_average <- flanker_data_clean %>%
  # Convert rts to seconds for DDM
  mutate(rt = rt / 1000) %>%
  group_by(id, congruency, correct) %>%
  mutate(rt_flanker = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, congruency) %>%
  summarise(
    rt_flanker     = mean(rt_flanker, na.rm = T), 
    acc_flanker    = (sum(correct) / n()),
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "congruency", values_from = c("rt_flanker", "acc_flanker")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(flanker_data_long = map(id, function(x) {flanker_data_clean %>% filter(id == x)})) %>%
  var_labels(
    counterbalance = "Counterbalancing code of the Flanker conditions",
    rt_flanker_congruent    = "Reaction time in seconds on the congruent Flanker trials.",
    rt_flanker_incongruent_standard  = "Reaction time in seconds on the incongruent Flanker trials",
    acc_flanker_congruent   = "Accuracy (proportion) on the congruent Flanker trials.",
    acc_flanker_incongruen_standard = "Accuracy (proportion) on the incongruent Flanker trials.",
    
    flanker_data_long = "Nested variable containing the dataframes with trial-level data for each participant. These dataframes are required for the DDM analyses."
  ) 


## Global-Local ----

globloc_data_clean <- globloc_data %>%
  group_by(id) %>%
  mutate(
    perc_correct = sum(correct, na.rm=T)/n()*100,
    # Is accuracy above the cut-off for performance at chance level?
    ex_narb_acc_below_cutoff = ifelse( (sum(correct, na.rm=T)/n())*100 < 200, FALSE, TRUE),# gbinom(64, 0.5), TRUE, FALSE),
    # Do all participants have the expected number of trials? 
    ex_narb_rowcount         = n(),
    # Number of missing RTs per participant
    ex_narb_NA_trials        = ifelse(n() > 1, sum(is.na(rt)), NA), 
    ex_narb_log_outlier      = ifelse(abs(scale(log(rt))) > 3.2, TRUE, FALSE) |> as.numeric(),
    # Number of outliers (>3.2SD, based on log-transformed RTs) per subject
    ex_narb_log_outliers_n   = ifelse(n() > 1, sum(scale(log(rt)) > 3.2), NA),
    # Number of trials with fast (<250 ms) or slow (>3500 ms) outliers
    ex_narb_invalid_trial      = ifelse(rt < 3500 & rt > 250, FALSE, TRUE),
    ex_narb_invalid_trials_n   = ifelse(n() > 1, sum(rt > 3500 | rt < 250, na.rm = TRUE), NA),
  ) %>%
  ungroup() %>%
  mutate(
    # Exclude participants with more than 10 excluded trials, who did not complete the task, or who performed at chance level
    ex_narb_globloc_pass   = ifelse(across(matches("ex_narb_(NA_trials|log_outliers|invalid_trials)")) %>% rowSums(., na.rm = TRUE) > 10 | 
                                      ex_narb_rowcount %in% c(1,2) |
                                      ex_narb_acc_below_cutoff == TRUE, 
                                    FALSE, TRUE)
  ) %>%
  left_join(browser_interactions_summary %>% select(id, event_during_globloc))




# Apply exclusions --------------------------------------------------------

globloc_data_clean %<>%
  # Remove invalid trials
  filter(!ex_narb_log_outlier) %>%
  filter(!ex_narb_invalid_trial) %>%
  # Exclude participants who did not pass the quality checks
  filter(ex_narb_globloc_pass == TRUE) %>%
  # Exclude participants with blur events during the task
  filter(!event_during_globloc) %>%
  select(-starts_with("ex_narb"))

# Compute average RTs and accuracy ----------------------------------------

globloc_data_clean_average <- globloc_data_clean %>%
  # Convert rts to seconds for DDM
  mutate(rt = rt / 1000) %>%
  group_by(id, rule, correct) %>%
  mutate(rt_globloc = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, rule) %>%
  summarise(
    rt_globloc     = mean(rt_globloc, na.rm = T), 
    acc_globloc    = (sum(correct) / n()),
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "rule", values_from = c("rt_globloc", "acc_globloc")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(globloc_data_long = map(id, function(x) {globloc_data_clean %>% filter(id == x)})) %>%
  var_labels(
    
    rt_globloc_global    = "Reaction time in seconds on the global globloc trials.",
    rt_globloc_local     = "Reaction time in seconds on the local globloc trials",
    acc_globloc_global   = "Accuracy (proportion) on the global globloc trials.",
    acc_globloc_local    = "Accuracy (proportion) on the local globloc trials.",
    
    globloc_data_long = "Nested variable containing the dataframes with trial-level data for each participant. These dataframes are required for the DDM analyses."
  ) 



# Write objects -----------------------------------------------------------

save(flanker_data_clean_average, 
     globloc_data_clean_average,
     browser_interactions, 
     browser_interactions_summary,
     resize_screen,
     file = here("data", "3_study2", "1_task_data_clean.Rdata"))
