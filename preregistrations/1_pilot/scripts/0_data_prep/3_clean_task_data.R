
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "0_task_data_raw.Rdata"))

source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_exclusions.R"))

# Planned exclusions ------------------------------------------------------

task_exclusions <- function(data, label) {
  data %>%
    group_by(id) %>%
    mutate(
      # Is accuracy above the cut-off for performance at chance level?
      ex_narb_acc_below_cutoff = ifelse( (sum(correct, na.rm=T)/n())*100 < gbinom(50, 0.50), TRUE, FALSE),
      # Do all participants have the expected number of trials? 
      ex_narb_rowcount         = n(),
      # Number of missing RTs per participant
      ex_narb_NA_trials        = ifelse(n() > 1, sum(is.na(rt)), NA), 
      
      ex_narb_log_outlier      = ifelse(scale(log(rt)) > 3.2, TRUE, FALSE),
      # Number of outliers (>3.2SD, based on log-transformed RTs) per subject
      ex_narb_log_outliers_n   = ifelse(n() > 1, sum(scale(log(rt)) > 3.2), NA),
      # Number of trials with fast (<250 ms) or slow (>3500 ms) outliers
      ex_narb_invalid_trial      = ifelse(rt < 3500 & rt > 250, FALSE, TRUE),
      ex_narb_invalid_trials_n   = ifelse(n() > 1, sum(rt > 3500 | rt < 250, na.rm = TRUE), NA),
    ) %>%
    ungroup() %>%
    mutate(
      # Exclude participants with more than 10 excluded trials, who did not complete the task, or who performed at chance level
      "ex_narb_{label}_pass"   := ifelse(across(matches("ex_narb_(NA_trials|log_outliers|invalid_trials)")) %>% rowSums(., na.rm = TRUE) > 10 | 
                                        ex_narb_rowcount %in% c(1,2) |
                                        ex_narb_acc_below_cutoff == TRUE, 
                                        FALSE, TRUE),
    )
}

## Add exclusion variables ----

change_data_clean <- 
  task_exclusions(change_data, "change")

cueing_data_clean <- 
  task_exclusions(cueing_data, "cueing")

flanker_data_clean <- 
  task_exclusions(flanker_data, "flanker")


# Apply exclusions --------------------------------------------------------

# Exclude participants who did not complete one or more tasks across all tasks
ids_to_exclude <- c(
  change_data_clean %>% filter(ex_narb_rowcount == 2) %>% select(id) %>% distinct() %>% pull,
  cueing_data_clean %>% filter(ex_narb_rowcount == 1) %>% select(id) %>% distinct() %>% pull,
  flanker_data_clean %>% filter(ex_narb_rowcount == 1) %>% select(id) %>% distinct() %>% pull
)

change_data_clean %<>%
  # Remove invalid trials
  filter(!ex_narb_log_outlier) %>%
  filter(!ex_narb_invalid_trial) %>%
  # Exclude participants who did not pass the quality checks
  filter(ex_narb_change_pass == TRUE) %>%
  # Exclude participants who did not complete all tasks
  filter(!id %in% ids_to_exclude) %>%
  # Exclude participants based on their own feedback:
  # 1. Participants who did tasks twice due to technical difficulties
  filter(!id %in% c("291", "410")) %>%
  # 2. Participants for whom all task windows were cut off
  filter(!id %in% "302") %>%
  select(-starts_with("ex_narb"))

cueing_data_clean %<>%
  # Remove invalid trials
  filter(!ex_narb_log_outlier) %>%
  filter(!ex_narb_invalid_trial) %>%
  # Exclude participants who did not pass the quality checks
  filter(ex_narb_cueing_pass == TRUE) %>%
  # Exclude participants who did not complete all tasks
  filter(!id %in% ids_to_exclude) %>%
  # Exclude participants based on their own feedback:
  # 1. Participants for whom task screen was too small for their window
  filter(!id %in% c("30", "551", "296", "459")) %>%
  # 2. Participants who did tasks twice due to technical difficulties
  filter(!id %in% c("291", "410")) %>%
  # 3. Participants for whom all task windows were cut off
  filter(!id %in% "302") %>%
  select(-starts_with("ex_narb"))

flanker_data_clean %<>%
  # Remove invalid trials
  filter(!ex_narb_log_outlier) %>%
  filter(!ex_narb_invalid_trial) %>%
  # Exclude participants who did not pass the quality checks
  filter(ex_narb_flanker_pass == TRUE) %>%
  # Exclude participants who did not complete all tasks
  filter(!id %in% ids_to_exclude) %>%
  # Exclude participants based on their own feedback:
  # 1. Participants who did tasks twice due to technical difficulties
  filter(!id %in% c("291", "410")) %>%
  # 2. Participants for whom all task windows were cut off
  filter(!id %in% "302") %>%  
  select(-starts_with("ex_narb"))

browser_interactions %<>%
  filter(!id %in% ids_to_exclude)

resize_screen %<>%
  filter(!id %in% ids_to_exclude)



# Compute average RTs and accuracy ----------------------------------------

change_data_clean_average <- change_data_clean %>%
  # Convert rts to seconds for DDM
  mutate(rt = rt / 1000) %>%
  group_by(id, correct) %>%
  mutate(rt_change = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id) %>%
  summarise(
    rt_change     = mean(rt_change, na.rm = T), 
    acc_change    = (sum(correct) / n()),
    rt_var_change = sd(rt, na.rm = TRUE)^2) %>%
  ungroup() %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(change_data_long = map(id, function(x) {change_data_clean %>% filter(id == x)}))


cueing_data_clean_average <- cueing_data_clean %>%
  # Convert rts to seconds for DDM
  mutate(rt = rt / 1000) %>%
  group_by(id, condition, correct) %>%
  mutate(rt_cueing = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, condition) %>%
  summarise(
    rt_cueing     = mean(rt_cueing, na.rm = T), 
    acc_cueing    = (sum(correct) / n()),
    rt_var_cueing = sd(rt, na.rm = T)^2
    ) %>%
  ungroup() %>%
  pivot_wider(names_from = "condition", values_from = c("rt_cueing", "acc_cueing", "rt_var_cueing")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(cueing_data_long = map(id, function(x) {cueing_data_clean %>% filter(id == x)}))


flanker_data_clean_average <- flanker_data_clean %>%
  # Convert rts to seconds for DDM
  mutate(rt = rt / 1000) %>%
  group_by(id, congruency, correct) %>%
  mutate(rt_flanker = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, congruency) %>%
  summarise(
    rt_flanker     = mean(rt_flanker, na.rm = T), 
    acc_flanker    = (sum(correct) / n()),
    rt_var_flanker = sd(rt, na.rm = T)^2
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "congruency", values_from = c("rt_flanker", "acc_flanker", "rt_var_flanker")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(flanker_data_long = map(id, function(x) {flanker_data_clean %>% filter(id == x)}))


# Write objects -----------------------------------------------------------

save(change_data_clean_average, 
     cueing_data_clean_average, 
     flanker_data_clean_average, 
     browser_interactions, 
     browser_interactions_summary,
     resize_screen,
     file = here("data", "1_pilot", "1_task_data_clean.Rdata"))
