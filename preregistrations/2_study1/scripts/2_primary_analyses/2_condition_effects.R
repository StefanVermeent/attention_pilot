
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
library(gt)

source(here("preregistrations", "2_study1", "scripts", "custom_functions", "multiverse_functions.R"))

source(here("preregistrations", "2_study1", "scripts", "custom_functions", "functions_analyses.R"))
source(here("preregistrations", "2_study1", "scripts", "2_primary_analyses", "0_multiverse_specs.R"))

load(here("data", "2_study1", "2_cleaned_data.Rdata"))

# Load data ---------------------------------------------------------------

primary_enh_data <- cleaned_data %>%
  select(id, scale_factor, dems_edu, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, counterbalance, 
         vio_comp, matches("^((a|t0|p|sda|rd|interference|rt)_(flanker|flanker_(congruent|incongruent))_(enh|std))")) %>%
  mutate(
    scale_factor             = ifelse(round(scale_factor, 4) == 0.9007, 0, 1),
    rtdiff_flanker_std       = rt_flanker_congruent_std - rt_flanker_incongruent_std,
    rtdiff_flanker_enh       = rt_flanker_congruent_enh - rt_flanker_incongruent_enh
   # vio_comp_c               = scale(vio_comp, scale = F) %>% as.numeric
    ) %>%
  # Long format for analyses
  pivot_longer(matches("^((a|t0|p|sda|rd|interference|rtdiff)_flanker_(enh|std))"),
               names_to = c(".value", "condition"),
               names_pattern = "(^.*_flanker)(.*)") %>%
  # Sum-code conditions
  mutate(condition = ifelse(condition == "_std", 0, 1)) %>%
  group_by(condition) %>%
  # Calculate standardized dv measures
  mutate(across(matches("(a|t0|p|sda|rd|interference|rtdiff)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
  ) %>%
  ungroup()


primary_deg_data <- cleaned_data %>%
  select(id, scale_factor, dems_edu, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, counterbalance, 
         vio_comp, matches("^((a|t0|p|sda|rd|interference|rt)_(flanker|flanker_(congruent|incongruent))_(deg|std))")) %>%
  mutate(
    scale_factor             = ifelse(round(scale_factor, 4) == 0.9007, 0, 1),
    rtdiff_flanker_std       = rt_flanker_congruent_std - rt_flanker_incongruent_std,
    rtdiff_flanker_deg       = rt_flanker_congruent_deg - rt_flanker_incongruent_deg
    # vio_comp_c               = scale(vio_comp, scale = F) %>% as.numeric
  ) %>%
  # Long format for analyses
  pivot_longer(matches("^((a|t0|p|sda|rd|interference|rtdiff)_flanker_(deg|std))"),
               names_to = c(".value", "condition"),
               names_pattern = "(^.*_flanker)(.*)") %>%
  # Sum-code conditions
  mutate(condition = ifelse(condition == "_std", 0, 1)) %>%
  group_by(condition) %>%
  # Calculate standardized dv measures
  mutate(across(matches("(a|t0|p|sda|rd|interference|rtdiff)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
  ) %>%
  ungroup()



# **ANALYSIS 1** STANDARD - ENHANCED COMPARISON ------------------------------------------

primary_enh_ssp_grid <- multiverse_grid(primary_condition_ssp_decisions) 
primary_enh_ssp_data_list <- multiverse_apply_grid(data = primary_enh_data, grid = primary_enh_ssp_grid)
primary_enh_ssp_results <- multiverse_run_lmer(data_list = primary_enh_ssp_data_list, predictors = c("vio_comp_c", "condition"), levels = list(c(-1, 1), c(0, 1)), random = "(1|id)", parallel = TRUE, cores = 4)

# Investigate singularity in subset of models
primary_enh_ssp_results %>%
  map(function(x) {
    
    specs = x$specifications %>%
      mutate(singular = x$singular)
  }) %>%
  bind_rows %>%
  group_by(spec_expr) %>%
  summarise(sum(singular))

# ├ Extract effects ---------------------------------------------------------
# ├ Raw effect size data ----------------------------------------------------
primary_ssp_effects_enh <- multiverse_extract_effects(model_list = primary_enh_ssp_results, grid = primary_enh_ssp_grid) %>%
  mutate(
    mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
                               mod_term == "vio_comp_c" ~ "Violence exposure",
                               mod_term == "condition"            ~ "Condition",
                               str_detect(mod_term, ":")          ~ "Interaction",
    ),
    mod_term_unique = case_when(mod_term == "vio_comp_c" ~ "Vio",
                                mod_term == "vio_comp_c:condition" ~ "Vio~symbol('\\264')~Task-Condition",
                                T ~ mod_term_label),
    mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num)
  )

# ├ Medians -----------------------------------------------------------------
primary_effects_medians_enh <- multiverse_medians(primary_ssp_effects_enh)

# ├ Interaction Data --------------------------------------------------------
primary_effects_points_enh <- multiverse_interaction_points(multiverse = primary_enh_ssp_results, effects = primary_ssp_effects_enh)

# ├ Results Table
primary_vio_medians_table_enh <- multiverse_median_effects_table(effects = primary_ssp_effects_enh)
primary_vio_simslopes_table_enh <- multiverse_simple_slopes_table(model_list = primary_enh_ssp_results, label_names = list(c("std", "enh"), c("low", "high")))


# **ANALYSIS 2** STANDARD - DEGRADED COMPARISON ------------------------------------------


# ├ Apply exclusions and analyze multiverse ---------------------------------

primary_deg_ssp_grid <- multiverse_grid(primary_condition_ssp_decisions) 
primary_deg_ssp_data_list <- multiverse_apply_grid(data = primary_deg_data, grid = primary_deg_ssp_grid)
primary_deg_ssp_results <- multiverse_run_lmer(data_list = primary_deg_ssp_data_list, predictors = c("vio_comp_c", "condition"), levels = list(c(-1, 1), c(0, 1)), random = "(1|id)", parallel = TRUE, cores = 4)

# Investigate singularity in subset of models
primary_deg_ssp_results %>%
  map(function(x) {
    
    specs = x$specifications %>%
      mutate(singular = x$singular)
  }) %>%
  bind_rows %>%
  group_by(spec_expr) %>%
  summarise(sum(singular))

# ├ Extract effects ---------------------------------------------------------
# ├ Raw effect size data ----------------------------------------------------
primary_ssp_effects_deg <- multiverse_extract_effects(model_list = primary_deg_ssp_results, grid = primary_deg_ssp_grid) %>%
  mutate(
    mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
                               mod_term == "vio_comp_c" ~ "Violence exposure",
                               mod_term == "condition"            ~ "Condition",
                               str_detect(mod_term, ":")          ~ "Interaction",
    ),
    mod_term_unique = case_when(mod_term == "vio_comp_c" ~ "Vio",
                                mod_term == "vio_comp_c:condition" ~ "Vio~symbol('\\264')~Task-Condition",
                                T ~ mod_term_label),
    mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num)
  )

# ├ Medians -----------------------------------------------------------------
primary_effects_medians_deg <- multiverse_medians(primary_ssp_effects_deg)

# ├ Interaction Data --------------------------------------------------------
primary_effects_points_deg <- multiverse_interaction_points(multiverse = primary_deg_ssp_results, effects = primary_ssp_effects_deg)

# ├ Results Table
primary_vio_medians_table_deg <- multiverse_median_effects_table(effects = primary_ssp_effects_deg)
primary_vio_simslopes_table_deg <- multiverse_simple_slopes_table(model_list = primary_deg_ssp_results, label_names = list(c("std", "deg"), c("low", "high")))




save(
  primary_ssp_effects_deg, primary_effects_medians_deg, primary_effects_points_deg, #primary_vio_medians_table_deg, primary_vio_simslopes_table_deg,
     primary_ssp_effects_enh, primary_effects_medians_enh, primary_effects_points_enh, #primary_vio_medians_table_enh, primary_vio_simslopes_table_enh,
     file = here("data", "2_study1", "primary_vio_results.RData"))
