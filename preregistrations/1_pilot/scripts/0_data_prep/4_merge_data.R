
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "1_self_report_clean.Rdata"))
load(here("data", "1_pilot", "1_task_data_clean.Rdata"))



# Compute average RTs and accuracy ----------------------------------------

change_data_clean_average <- change_data_clean %>%
  group_by(id, correct) %>%
  mutate(rt_change = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id) %>%
  summarise(rt_change = mean(rt_change, na.rm = T), acc_change = (sum(correct) / n()) * 100) %>%
  ungroup() %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(change_data_long = map(id, function(x) {change_data_clean %>% filter(id == x)}))


cueing_data_clean_average <- cueing_data_clean %>%
  group_by(id, condition, correct) %>%
  mutate(rt_cueing = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, condition) %>%
  summarise(rt_cueing = mean(rt_cueing, na.rm = T), acc_cueing = (sum(correct) / n()) * 100) %>%
  ungroup() %>%
  pivot_wider(names_from = "condition", values_from = c("rt_cueing", "acc_cueing")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(cueing_data_long = map(id, function(x) {cueing_data_clean %>% filter(id == x)}))


flanker_data_clean_average <- flanker_data_clean %>%
  group_by(id, congruency, correct) %>%
  mutate(rt_flanker = ifelse(correct, mean(rt, na.rm = T), NA)) %>%
  group_by(id, congruency) %>%
  summarise(rt_flanker = mean(rt_flanker, na.rm = T), acc_flanker = (sum(correct) / n()) * 100) %>%
  ungroup() %>%
  pivot_wider(names_from = "congruency", values_from = c("rt_flanker", "acc_flanker")) %>%
  # Add nested column containing task data in long-form (for DDM)
  mutate(flanker_data_long = map(id, function(x) {flanker_data_clean %>% filter(id == x)}))


# Combine data ------------------------------------------------------------

cleaned_data <-
  self_report_clean %>%
  full_join(change_data_clean_average) %>%
  full_join(cueing_data_clean_average, by = "id") %>%
  full_join(flanker_data_clean_average, by = "id") %>%
  full_join(resize_screen, by = "id") %>%
  full_join(browser_interactions_summary, by = "id") %>%
  # Remove participants who have no task data
  filter(!is.na(rt_change) | !is.na(rt_cueing_cued) | !is.na(rt_flanker_congruent)) %>%
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
    rt_change, acc_change, change_data_long,
    rt_cueing_cued, acc_cueing_cued, rt_cueing_neutral, acc_cueing_neutral, cueing_data_long,
    rt_flanker_congruent, acc_flanker_congruent, rt_flanker_incongruent, acc_flanker_incongruent, flanker_data_long,
    everything(),
    -starts_with("sd_")
  )

save(cleaned_data, browser_interactions, file = here("data", "1_pilot", "2_combined_clean_data.Rdata"))
