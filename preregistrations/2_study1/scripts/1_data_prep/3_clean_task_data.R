
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)
library(sjlabelled)

# Load data ---------------------------------------------------------------

load(here("data", "2_study1", "0_task_data_raw.Rdata"))

source("preregistrations/2_study1/scripts/custom_functions/functions_exclusions.R")

# Planned exclusions ------------------------------------------------------

flanker_data_clean <- flanker_data %>%
    group_by(id, condition) %>%
    mutate(
      # Is accuracy above the cut-off for performance at chance level?
      #ex_narb_acc_below_cutoff_50 = ifelse( (sum(correct, na.rm=T)/n())*100 < gbinom(50, 0.5), TRUE, FALSE),
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
  # Exclude participants who did not complete all tasks
  filter(!id %in% ids_to_exclude) %>%
  # Exclude participants with blur events during the task
  filter(!event_during_flanker) %>%
  # Exclude participants based on their own feedback:
  mutate(across(c(rt, correct), ~ifelse(id == 14 & condition == "degraded", NA, .))) %>%
  select(-starts_with("ex_narb"))


# Exclude participants who did not complete one or more conditions of the Flanker task
ids_to_exclude <- c(
  flanker_data_clean %>% group_by(id) %>% summarise(n_condition = length(unique(condition))) %>% filter(n_condition < 3) %>% select(id) %>% distinct() %>% pull
)

flanker_data_clean %<>%
  filter(!id %in% ids_to_exclude)

# Compute average RTs and accuracy ----------------------------------------

flanker_data_clean_average <- flanker_data_clean %>%
  # Convert rts to seconds for DDM
  mutate(rt = rt / 1000) %>%
  group_by(id, congruency, condition, correct, counterbalance) %>%
  mutate(rt_flanker = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, congruency, condition, counterbalance) %>%
  summarise(
    rt_flanker     = mean(rt_flanker, na.rm = T), 
    acc_flanker    = (sum(correct) / n()),
  ) %>%
  ungroup() %>%
  mutate(condition = case_when(
    condition == "standard" ~ "std",
    condition == "enhanced" ~ "enh",
    condition == "degraded" ~ "deg"
  )) %>%
  pivot_wider(names_from = "congruency", values_from = c("rt_flanker", "acc_flanker")) %>%
  pivot_wider(names_from = "condition", values_from = c("rt_flanker_congruent", "rt_flanker_incongruent", "acc_flanker_congruent", "acc_flanker_incongruent")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(flanker_data_long = map(id, function(x) {flanker_data_clean %>% filter(id == x)})) %>%
  var_labels(
    counterbalance = "Counterbalancing code of the Flanker conditions",
    
    rt_flanker_congruent_standard    = "Reaction time in seconds on the congruent Flanker trials of the Standard condition.",
    rt_flanker_incongruent_standard  = "Reaction time in seconds on the incongruent Flanker trials of the Standard condition.",
    acc_flanker_congruent_standard   = "Accuracy (proportion) on the congruent Flanker trials of the Standard condition.",
    acc_flanker_incongruent_standard = "Accuracy (proportion) on the incongruent Flanker trials of the Standard condition.",
    
    rt_flanker_congruent_enhanced    = "Reaction time in seconds on the congruent Flanker trials of the Enhanced condition.",
    rt_flanker_incongruent_enhanced  = "Reaction time in seconds on the incongruent Flanker trials of the Enhanced condition.",
    acc_flanker_congruent_enhanced   = "Accuracy (proportion) on the congruent Flanker trials of the Enhanced condition.",
    acc_flanker_incongruent_enhanced = "Accuracy (proportion) on the incongruent Flanker trials of the Enhanced condition.",
    
    rt_flanker_congruent_degraded    = "Reaction time in seconds on the congruent Flanker trials of the Degraded condition.",
    rt_flanker_incongruent_degraded  = "Reaction time in seconds on the incongruent Flanker trials of the Degraded condition.",
    acc_flanker_congruent_degraded   = "Accuracy (proportion) on the congruent Flanker trials of the Degraded condition.",
    acc_flanker_incongruent_degraded = "Accuracy (proportion) on the incongruent Flanker trials of the Degraded condition.",
    
    flanker_data_long = "Nested variable containing the dataframes with trial-level data for each participant. These dataframes are required for the DDM analyses."
  ) %>%
  val_labels(
    counterbalance = c("Standard - Enhanced - Degraded" = 1,
                       "Standard - Degraded - Enhanced" = 2,
                       "Enhanced - Standard - Degraded" = 3,
                       "Enhanced - Degraded - Standard" = 4,
                       "Degraded - Standard - Enhanced" = 5,
                       "Degraded - Enhanced - Standard" = 6)
  )


flanker_data_clean_average %>%
  mutate(across(matches("(acc|rt)_flanker_(congruent|incongruent)"), ~scale(.) %>% as.numeric, .names = "{.col}_z")) %>%
  select(id, matches("^(acc|rt)")) %>%
  filter(if_any(matches("(std|enh|deg)_z"), ~.>3.2))



browser_interactions %<>%
  filter(!id %in% ids_to_exclude)

resize_screen %<>%
  filter(!id %in% ids_to_exclude)



# Write objects -----------------------------------------------------------

save(flanker_data_clean_average, 
     browser_interactions, 
     browser_interactions_summary,
     resize_screen,
     file = here("data", "2_study1", "1_task_data_clean.Rdata"))
