
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "2_study1", "1_self_report_clean.Rdata"))
load(here("data", "2_study1", "1_task_data_clean.Rdata"))
load(here("data", "2_study1", "1_SSP_objects.Rdata"))


# Combine data ------------------------------------------------------------

cleaned_data <-
  # Self-report data
  self_report_clean %>%
  # Task data: RTs and accuracy
  left_join(flanker_data_clean_average, by = "id") %>%
  # Task data: DDM
  left_join(ssp_results_standard, by = "id") %>%
  left_join(ssp_results_enhanced, by = "id") %>%
  left_join(ssp_results_degraded, by = "id") %>%
  left_join(resize_screen, by = "id") %>%
  left_join(browser_interactions_summary, by = "id") %>%
  # Remove participants who have no task data
  filter(!is.na(rt_flanker_congruent_std), !is.na(rt_flanker_congruent_deg), !is.na(rt_flanker_congruent_enh)) %>%
  # Manual exclusions
  filter(!id %in% c(99, 176, 304, 340, 483, 73, 335, 114, 466, 16)) %>%
  mutate(
    interference_flanker_std = sda_flanker_std / rd_flanker_std,
    interference_flanker_enh = sda_flanker_enh / rd_flanker_enh,
    interference_flanker_deg = sda_flanker_deg / rd_flanker_deg
  )

# .csv file (nested columns containing trial-level data are dropped)
write_csv(cleaned_data %>% select(-matches("flanker_data_long")), here("data", "2_study1", "2_cleaned_data.csv"))

# .RData file including nested columns
save(cleaned_data, browser_interactions, file = here("data", "2_study1", "2_cleaned_data.Rdata"))



# RTs:

# 304 is clearly doing something different than most others
# same goes for 99
# Same goes for 176
# Same goes for 483
# Same goes for 340


# Acc:
# Standard: 73, 483 and 176 are clear outliers on standard congruent accuracy

# 335: below chance level on enhanced incongruent; (near) perfect on others
# 114: Very different pattern than other participants
# 466: at chance level on degraded



  
