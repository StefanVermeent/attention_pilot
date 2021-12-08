
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "1_self_report_clean.Rdata"))
load(here("data", "1_pilot", "1_task_data_clean.Rdata"))
load(here("data", "1_pilot", "1_DDM_objects.Rdata"))


# Combine data ------------------------------------------------------------

cleaned_data <-
  self_report_clean %>%
  inner_join(change_data_clean_average) %>%
  inner_join(cueing_data_clean_average, by = "id") %>%
  inner_join(flanker_data_clean_average, by = "id") %>%
  left_join(change_DDM_results, by = "id") %>%
  left_join(resize_screen, by = "id") %>%
  left_join(browser_interactions_summary, by = "id") %>%
  # Remove participants who have no task data
# filter(!is.na(rt_change) | !is.na(rt_cueing_cued) | !is.na(rt_flanker_congruent)) %>%
  # Reorder variables
  select(
    id, 
    starts_with("meta_"),
    attention_check_sum, attention_interrupt_sum, 
    starts_with("dems_"),
    matches("quic_(monitoring|par_predict|par_env|phys_env|safety)_mean"), chaos_mean, unp_mean,
    violence_mean, fighting_mean, violence_composite,
    ses_obj_mean, ses_subj_mean, ses_subj_mean_recoded, poverty_composite,
    impuls_mean, fos_pa_mean, fos_tp_mean, fos_fc_mean, fos_fo_mean,
    depression_mean,
    rt_change, acc_change, change_a, change_v, change_t0, change_fit, change_bad_fit, change_data_long,
    rt_cueing_cued, acc_cueing_cued, rt_cueing_neutral, acc_cueing_neutral, cueing_data_long,
    rt_flanker_congruent, acc_flanker_congruent, rt_flanker_incongruent, acc_flanker_incongruent, flanker_data_long,
    everything(),
    -starts_with("sd_")
  )

save(cleaned_data, browser_interactions, file = here("data", "1_pilot", "2_combined_clean_data.Rdata"))
