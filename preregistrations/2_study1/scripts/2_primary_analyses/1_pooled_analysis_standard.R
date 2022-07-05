# This script contains code used to address Aim 1 of the preregistration:
# Investigate the robustness of the primary DDM findings in the pilot study 
# by pooling the pilot data and the data of the standard condition in the current study. 



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(magrittr)
library(here)
library(furrr)
library(tictoc)
library(gt)

source(here("preregistrations", "2_study1", "scripts", "custom_functions", "multiverse_functions.R"))
source(here("preregistrations", "2_study1", "scripts", "custom_functions", "functions_analyses.R"))
source(here("preregistrations", "2_study1", "scripts", "2_primary_analyses", "0_multiverse_specs.R"))

# Load data ---------------------------------------------------------------

# Pilot study
load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
pilot_data <- cleaned_data %>%
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, 
         vio_comp = violence_composite, rt_flanker_congruent, rt_flanker_incongruent, acc_flanker_congruent, acc_flanker_incongruent,
         a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker) %>%
  mutate(
    id = str_c("pilot_", id),
    study = -1,
    counterbalance = 'pilot',
    scale_factor = ifelse(round(scale_factor, 4) == 0.3081, 0, 1)
    )


# Study 1
load(here("data", "2_study1", "2_cleaned_data.Rdata"))
study1_data <- cleaned_data %>%
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, 
         vio_comp, rt_flanker_congruent_std, rt_flanker_incongruent_std, acc_flanker_congruent_std, acc_flanker_incongruent_std,
         a_flanker_std, t0_flanker_std, p_flanker_std, rd_flanker_std, sda_flanker_std) %>%
  rename(rt_flanker_congruent = rt_flanker_congruent_std,
         rt_flanker_incongruent = rt_flanker_incongruent_std,
         acc_flanker_congruent = acc_flanker_congruent_std,
         acc_flanker_incongruent = acc_flanker_incongruent_std,
         a_flanker = a_flanker_std,
         t0_flanker = t0_flanker_std,
         p_flanker = p_flanker_std,
         rd_flanker = rd_flanker_std,
         sda_flanker = sda_flanker_std) %>%
  mutate(
    id = str_c("study1_", id),
    study = 1,
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 0, 1)
    )

# Combine data of pilot study and study 1
pooled_data <- bind_rows(pilot_data, study1_data) %>%
  filter(rd_flanker > 0) %>%
  mutate(interference_flanker = sda_flanker / rd_flanker) %>%
  group_by(study) %>%
  mutate(across(matches("(a|t0|p|sda|rd|interference)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
         ) 




# **ANALYSIS 1** POOLED DATA ------------------------------------------

primary_pooled_ssp_grid <- multiverse_grid(primary_pooled_ssp_decisions) 
primary_pooled_ssp_data_list <- multiverse_apply_grid(data = pooled_data, grid = primary_pooled_ssp_grid)
primary_pooled_ssp_results <- multiverse_run_lm(data_list = primary_pooled_ssp_data_list, predictors = c("vio_comp_c"), covariates = "study", parallel = TRUE, cores = 4)


# ├ Extract effects ---------------------------------------------------------
primary_ssp_effects_pooled <- multiverse_extract_effects(model_list = primary_pooled_ssp_results, grid = primary_pooled_ssp_grid) %>%
  mutate(
    mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
                               mod_term == "vio_comp_c" ~ "Violence exposure",
                               mod_term == "study"            ~ "Study"
    ),
    mod_term_unique = case_when(mod_term == "vio_comp_c" ~ "Vio",
                                T ~ mod_term_label),
    mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num)
  )

## Medians 
primary_effects_medians_pooled <- multiverse_medians(primary_ssp_effects_pooled)



# ├ Results Table ---------------------------------------------------------


medians <- 
  primary_ssp_effects_pooled %>% 
  mutate(
    iv = "Violence exposure",
    dv = spec_dv_type
  ) %>%
  filter(mod_term != "study") %>%
  select(spec_number,iv,dv,mod_term_group, mod_std_coefficient, mod_ci_high, mod_ci_low, n, mod_p.value) %>% 
  filter(mod_term_group %in% c("Main Effect")) %>% 
  group_by(iv, dv, mod_term_group) %>% 
  mutate(p_percent = (sum(mod_p.value < .05)/n())) %>% 
  summarize(across(where(is.numeric), median)) %>% 
  rename(null_term = mod_term_group) %>% 
  transmute(
    # iv          = case_when(iv == "Unpredictability" ~ "unp", iv == "Violence" ~ "vio", iv == "SES" ~ "ses"),
    # dv          = ifelse(dv == "Attention-Shifting","shifting","updating"),
    null_term   = ifelse(null_term == "Main Effect", "main", "int"),
    median_beta = mod_std_coefficient,
    median_lo   = mod_ci_low,
    median_hi   = mod_ci_high,
    median_n    = n,
    p_percent   = p_percent
  )

# Table - Base ------------------------------------------------------------
table_base <- 
  medians %>%
  ungroup() %>%
  select(-iv) %>%
  #left_join(boot_medians, primary_medians, by = c("iv","dv", "null_term")) %>% 
  arrange(dv) %>%#, boot_num) %>% 
  group_by(dv, null_term) %>% 
  # mutate(
  #   is_sig = if_else(median_beta < 0, median_null < median_beta, median_null > median_beta)
  # ) %>% 
  summarize(
    #   p_overall   = sum(is_sig)/n(),
    p_percent   = unique(p_percent),
    median_beta = unique(median_beta),
    median_lo   = unique(median_lo),
    median_hi   = unique(median_hi),
    median_n    = unique(median_n)
  ) %>% 
  ungroup() %>%
  arrange(dv, desc(null_term)) %>% 
  mutate(
    median_beta = formatC(round(median_beta,2), digits = 2, width = 3, flag = '0', format = 'f'),
    median_n    = formatC(round(median_n), digits = 0, format = 'f'),
    p_percent   = formatC(round(p_percent*100, 2), digits = 2, width = 3, flag = '0', format = 'f') %>% paste0(.,"%"),
    # p_overall   = formatC(round(p_overall,3), digits = 3, width = 4, flag = '0', format = 'f'),
    ci_lo          = formatC(round(median_lo,2), digits = 2, width = 3, flag = '0', format = 'f'),
    ci_hi          = formatC(round(median_hi,2), digits = 2, width = 3, flag = '0', format = 'f'),
    ci             = glue::glue("[{ci_lo}, {ci_hi}]")
  ) %>% 
  select(dv,null_term, p_percent, median_beta, ci, median_n) %>% 
  pivot_wider(names_from = null_term, values_from = c(p_percent, median_beta, ci, median_n)) %>% 
  # mutate(
  #   dv          = ifelse(dv == "shifting", "Attention Shifting","WM Updating"),
  #   iv          = case_when(iv == "ses" ~ "Poverty",
  #                           iv == "unp" ~ "Unpredictability",
  #                           iv == "vio" ~ "Violence")
  # ) %>% 
  select(
    dv, 
    matches("(beta|ci)_main"), p_percent_main, #p_overall_main, 
    #matches("(beta|ci)_int"), p_percent_int# p_overall_int
  )

# Table 2 - Mixed Models --------------------------------------------------
table_pooled_vio <- 
  table_base %>% 
  gt() %>% 
  cols_label(
    dv = "",
    median_beta_main = "\\Beta", 
    ci_main          = md("95\\% CI"),
    p_percent_main   = md("*p* (\\%)")
  ) %>% 
  tab_spanner(c("Main Effect"), columns = 2:4) %>% 
  tab_options(
    row_group.font.weight = "bold"
  )



save(primary_ssp_effects_pooled, table_pooled_vio,
  file = here("data", "2_study1", "primary_vio_pooled_results.RData"))
