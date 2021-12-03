
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
      # Number of outliers (>3.2SD, based on log-transformed RTs) per subject
      ex_narb_log_outliers     = ifelse(n() > 1, sum(scale(log(rt)) > 3.2), NA),
      # Number of trials with fast (<250 ms) or slow (>3500 ms) outliers
      ex_narb_invalid_trials   = ifelse(n() > 1, sum(rt > 3500 | rt < 250, na.rm = TRUE), NA),
    ) %>%
    ungroup() %>%
    mutate(
      # Exclude participants with more than 10 excluded trials or who did not complete the task
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
  filter(ex_narb_change_pass == TRUE) %>%
  filter(!id %in% ids_to_exclude) %>%
  select(-starts_with("ex_narb"))

cueing_data_clean %<>%
  filter(ex_narb_cueing_pass == TRUE) %>%
  filter(!id %in% ids_to_exclude) %>%
  select(-starts_with("ex_narb"))

flanker_data_clean %<>%
  filter(ex_narb_flanker_pass == TRUE) %>%
  filter(!id %in% ids_to_exclude) %>%
  select(-starts_with("ex_narb"))

browser_interactions %<>%
  filter(!id %in% ids_to_exclude)

resize_screen %<>%
  filter(!id %in% ids_to_exclude)


# Write objects -----------------------------------------------------------

save(change_data_clean, 
     cueing_data_clean, 
     flanker_data_clean, 
     browser_interactions, 
     browser_interactions_summary,
     resize_screen,
     file = here("data", "1_pilot", "1_task_data_clean.Rdata"))
