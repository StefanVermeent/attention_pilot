
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "2_study1", "1_self_report_clean.Rdata"))
load(here("data", "2_study1", "1_task_data_clean.Rdata"))
#load(here("data", "1_pilot", "1_SSP_objects.Rdata"))


# Combine data ------------------------------------------------------------

cleaned_data <-
  # Self-report data
  self_report_clean %>%
  # Task data: RTs and accuracy
  left_join(flanker_data_clean_average, by = "id") %>%
  # Task data: DDM
 # left_join(flanker_ssp_results, by = "id") %>%
  left_join(resize_screen, by = "id") %>%
  left_join(browser_interactions_summary, by = "id") %>%
  # Remove participants who have no task data
  filter(!is.na(rt_flanker_congruent)) 


#cleaned_data %>% 
#  select(id, matches("rt_flanker")) %>% 
#  mutate(across(starts_with("rt_"), ~as.numeric(scale(log(.))))) %>% 
#  View


#cleaned_data %>%
#  filter(id %in% c(318, 515, 494, 158, 280, 6, 189, 528)) %>%
#  select(id, meta_feedback, contains("event"), ends_with("mean")) %>%
#  View


# .csv file (nested columns containing trial-level data are dropped)
write_csv(cleaned_data %>% select(-matches("(change|flanker|cueing)_data_long")), here("data", "1_pilot", "2_cleaned_data.csv"))

# .RData file including nested columns
save(cleaned_data, browser_interactions, file = here("data", "2_study1", "2_cleaned_data.Rdata"))
