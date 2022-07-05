
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
  left_join(ssp_results_refit, by = "id") %>%
  left_join(resize_screen, by = "id") %>%
  left_join(browser_interactions_summary, by = "id") %>%
  # Remove participants who have no task data
  filter(!is.na(rt_flanker_congruent_std), !is.na(rt_flanker_congruent_deg), !is.na(rt_flanker_congruent_enh)) %>%
  # Manual exclusions
  filter(!id %in% c(73, 99, 114, 163, 176, 304, 308, 335, 340, 483)) %>%
 # filter(!id %in% c(19, 121, 223, 8, 269, 464, 466, 491, 223)) %>%
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
# 73: 75th quantile of RTs for each condition are extreme outliers
# 483: 75th quantile of RTs for each condition are extreme outliers
# 340: Nearly all quantiles across conditions are extreme outliers
# 304: Consistently does something else than the rest of the sample
# 176: Extreme interference outlier on standard condition
# 99: Extreme outliers on 50th and 75th quantile

# Accuracy:
# 114: Accuracy is substantially lower on degraded than all other participants
# 163: Prediction is way off on degraded trials
# 308: Among lowest accuracy on all conditions, substantial outlier on standard congruent
# 335: Below chance level on enhanced ingruent; (near) perfect on others






  
