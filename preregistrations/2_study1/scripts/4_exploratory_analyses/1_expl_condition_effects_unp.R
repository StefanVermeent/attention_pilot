# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(magrittr)
library(here)
library(lme4)
library(lmerTest)
library(glue)
library(tictoc)
library(furrr)
library(ggeffects)
library(interactions)
library(performance)

source(here("preregistrations", "2_study1", "scripts", "custom_functions", "multiverse_functions.R"))

source(here("preregistrations", "2_study1", "scripts", "custom_functions", "functions_analyses.R"))
source(here("preregistrations", "2_study1", "scripts", "2_primary_analyses", "0_multiverse_specs.R"))

load(here("data", "2_study1", "2_cleaned_data.Rdata"))

# Load data ---------------------------------------------------------------
exploratory_enh_data <- cleaned_data %>%
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, counterbalance, 
         unp_pcunp_quic_comp, chaos_mean, unp_subj_comp, unp_obj_comp, quic_monitoring_mean, quic_par_predict_mean, quic_par_env_mean, quic_phys_env_mean, quic_safety_mean,
         matches("^((a|t0|p|sda|rd|interference)_flanker_(enh|std))")) %>%
  mutate(
    scale_factor             = ifelse(round(scale_factor, 4) == 0.9007, 0, 1),
  ) %>%
  # Long format for analyses
  pivot_longer(matches("^((a|t0|p|sda|rd|interference)_flanker_(enh|std))"),
               names_to = c(".value", "condition"),
               names_pattern = "(^.*_flanker)(.*)") %>%
  # Sum-code conditions
  mutate(condition = ifelse(condition == "_std", -1, 1)) %>%
  group_by(condition) %>%
  # Calculate standardized dv measures
  mutate(across(matches("(a|t0|p|sda|rd|interference)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
  ) %>%
  ungroup()


exploratory_deg_data <- cleaned_data %>%
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, counterbalance, 
         unp_pcunp_quic_comp, chaos_mean, unp_subj_comp, unp_obj_comp, quic_monitoring_mean, quic_par_predict_mean, quic_par_env_mean, quic_phys_env_mean, quic_safety_mean,
         matches("^((a|t0|p|sda|rd|interference)_flanker_(deg|std))")) %>%
  mutate(
    scale_factor             = ifelse(round(scale_factor, 4) == 0.9007, 0, 1),
  ) %>%
  # Long format for analyses
  pivot_longer(matches("^((a|t0|p|sda|rd|interference)_flanker_(deg|std))"),
               names_to = c(".value", "condition"),
               names_pattern = "(^.*_flanker)(.*)") %>%
  # Sum-code conditions
  mutate(condition = ifelse(condition == "_std", -1, 1)) %>%
  group_by(condition) %>%
  # Calculate standardized dv measures
  mutate(across(matches("(a|t0|p|sda|rd|interference)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
  ) %>% 
  ungroup()



# **ANALYSIS 1** STANDARD - ENHANCED COMPARISON ------------------------------------------

exploratory_unp_enh_ssp_grid <- multiverse_grid(secondary_condition_unp_expl_decisions) 
exploratory_unp_enh_ssp_data_list <- multiverse_apply_grid(data = exploratory_enh_data, grid = exploratory_unp_enh_ssp_grid)
exploratory_unp_enh_ssp_results <- multiverse_run_lmer(data_list = exploratory_unp_enh_ssp_data_list, predictors = c("condition"), levels = list(c(-1, 1), c(0, 1)), random = "(1|id)", parallel = TRUE, cores = 4)

# Investigate singularity in subset of models
secondary_enh_ssp_results %>%
  map(function(x) {
    
    specs = x$specifications %>%
      mutate(singular = x$singular)
  }) %>%
  bind_rows %>%
  filter(spec_var == "dv_type") %>%
  group_by(spec_expr) %>%
  summarise(sum(singular))

# ├ Extract effects ---------------------------------------------------------
# ├ Raw effect size data ----------------------------------------------------
secondary_ssp_effects_enh <- multiverse_extract_effects(model_list = secondary_enh_ssp_results, grid = secondary_enh_ssp_grid) %>%
  mutate(
    mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
                               mod_term == "unp_comp_c" ~ "Unpredictability",
                               mod_term == "condition"            ~ "Condition",
                               str_detect(mod_term, ":")          ~ "Interaction",
    ),
    mod_term_unique = case_when(mod_term == "unp_comp_c" ~ "unp",
                                mod_term == "unp_comp_c:condition" ~ "unp~symbol('\\264')~Task-Condition",
                                T ~ mod_term_label),
    mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num)
  )

# ├ Medians -----------------------------------------------------------------
secondary_effects_medians_enh <- multiverse_medians(secondary_ssp_effects_enh)

# ├ Interaction Data --------------------------------------------------------
secondary_effects_points_enh <- multiverse_interaction_points(multiverse = secondary_enh_ssp_results, effects = secondary_ssp_effects_enh)

# ├ Results Table
secondary_unp_medians_table_enh <- multiverse_median_effects_table(effects = secondary_ssp_effects_enh)
secondary_unp_simslopes_table_enh <- multiverse_simple_slopes_table(model_list = secondary_enh_ssp_results, label_names = list(c("std", "enh"), c("low", "high")))



# **ANALYSIS 2** STANDARD - DEGRADED COMPARISON ------------------------------------------


# ├ Apply exclusions and analyze multiverse ---------------------------------

secondary_deg_ssp_grid <- multiverse_grid(primary_condition_ssp_decisions) 
secondary_deg_ssp_data_list <- multiverse_apply_grid(data = secondary_deg_data, grid = secondary_deg_ssp_grid)
secondary_deg_ssp_results <- multiverse_run_lmer(data_list = secondary_deg_ssp_data_list, predictors = c("unp_comp_c", "condition"), levels = list(c(-1, 1), c(0, 1)), random = "(1|id)", parallel = TRUE, cores = 4)

# Investigate singularity in subset of models
secondary_deg_ssp_results %>%
  map(function(x) {
    
    specs = x$specifications %>%
      mutate(singular = x$singular)
  }) %>%
  bind_rows %>%
  group_by(spec_expr) %>%
  summarise(sum(singular))

# ├ Extract effects ---------------------------------------------------------
# ├ Raw effect size data ----------------------------------------------------
secondary_ssp_effects_deg <- multiverse_extract_effects(model_list = secondary_deg_ssp_results, grid = secondary_deg_ssp_grid) %>%
  mutate(
    mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
                               mod_term == "unp_comp_c" ~ "Unpredictability",
                               mod_term == "condition"            ~ "Condition",
                               str_detect(mod_term, ":")          ~ "Interaction",
    ),
    mod_term_unique = case_when(mod_term == "unp_comp_c" ~ "unp",
                                mod_term == "unp_comp_c:condition" ~ "unp~symbol('\\264')~Task-Condition",
                                T ~ mod_term_label),
    mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num)
  )

# ├ Medians -----------------------------------------------------------------
secondary_effects_medians_deg <- multiverse_medians(secondary_ssp_effects_deg)

# ├ Interaction Data --------------------------------------------------------
secondary_effects_points_deg <- multiverse_interaction_points(multiverse = secondary_deg_ssp_results, effects = secondary_ssp_effects_deg)

# ├ Results Table
secondary_unp_medians_table_deg <- multiverse_median_effects_table(effects = secondary_ssp_effects_deg)
secondary_unp_simslopes_table_deg <- multiverse_simple_slopes_table(model_list = secondary_deg_ssp_results, label_names = list(c("std", "deg"), c("low", "high")))


save(secondary_ssp_effects_enh, secondary_effects_medians_enh, secondary_effects_points_enh, secondary_unp_medians_table_enh,
     secondary_ssp_effects_deg, secondary_effects_medians_deg, secondary_effects_points_deg, secondary_unp_medians_table_deg,
     file = here("data", "2_study1", "secondary_unp_results.Rdata"))
