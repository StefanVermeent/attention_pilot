
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load("data/1_pilot/1_self_report_clean.Rdata")
load("data/1_pilot/1_task_data_clean.Rdata")
load("preregistrations/1_pilot/analysis_objects/DDM_objects.Rdata")
load("preregistrations/1_pilot/analysis_objects/hddm_model_objects.Rdata")



# Combine data ------------------------------------------------------------

cleaned_data <-
  # Self-report data
  self_report_clean |>
  # Task data: RTs and accuracy
  left_join(change_data_clean_average) |>
  left_join(cueing_data_clean_average, by = "id") |>
  left_join(flanker_data_clean_average, by = "id") |>
  # Task data: DDM
  left_join(change_DDM_results_mod4, by = "id") |>
  left_join(change_DDM_results_mod5, by = "id") |>
  left_join(hddm_change_data, by = "id") |> 
  left_join(cueing_DDM_results_mod8, by = "id") |>
  left_join(cueing_DDM_results_mod9, by = "id") |>
  left_join(hddm_cueing_data, by = "id") |> 
  left_join(flanker_DDM_results_mod2 |> distinct(), by = "id") |>
  left_join(flanker_ssp_results, by = "id") |>
  left_join(hddm_flanker_data, by = "id") |> 
  left_join(resize_screen, by = "id") |>
  left_join(browser_interactions_summary, by = "id") |>
  # Remove participants who have no task data
  filter(!is.na(rt_change) | !is.na(rt_cueing_cued) | !is.na(rt_flanker_congruent)) |>
  mutate(across(matches('meta.*duration.*_z'), as.numeric)) |> 
  mutate(unp_subj = as.numeric(unp_subj)) |>
  # Fix typos in age
  mutate(dems_age = ifelse(dems_age < 18 | dems_age > 30, NA, dems_age))




# .csv file (nested columns containing trial-level data are dropped)
write_csv(cleaned_data |> select(-matches("(change|flanker|cueing)_data_long")), here("data", "1_pilot", "2_cleaned_data.csv"))

# .RData file including nested columns
save(cleaned_data, browser_interactions, file = here("data", "1_pilot", "2_cleaned_data.Rdata"))
