
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "3_study2", "1_self_report_clean.Rdata"))
load(here("data", "3_study2", "1_task_data_clean.Rdata"))
load(here("data", "3_study2", "1_SSP_objects.Rdata"))
load('preregistrations/3_study2/analysis_objects/hddm_model_objects.RData')


# Combine data ------------------------------------------------------------

cleaned_data <-
  # Self-report data
  self_report_clean %>%
  # Task data: RTs and accuracy
  left_join(flanker_data_clean_average, by = "id") %>%
  left_join(globloc_data_clean_average, by = "id") %>%
  
  # Task data: DDM
  left_join(ssp_results_refit, by = "id") %>%
  left_join(hddm_globloc_results_mod2) %>%
  left_join(resize_screen, by = "id") %>%
  left_join(browser_interactions_summary, by = "id") %>%
  # Remove participants who have no task data
  filter(!is.na(rt_flanker_congruent)) %>%
  mutate(
    interference_flanker = sda_flanker/ rd_flanker,
    # Remove age typ0s
    dems_age = ifelse(dems_age < 18 | dems_age > 30, NA, dems_age)
  )

# .csv file (nested columns containing trial-level data are dropped)
write_csv(cleaned_data %>% select(-matches("(globloc|flanke)_data_long")), here("data", "3_study2", "2_cleaned_data.csv"))

# .RData file including nested columns
save(cleaned_data, browser_interactions, file = here("data", "3_study2", "2_cleaned_data.Rdata"))






  
