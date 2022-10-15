
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(here)
library(ggeffects)
library(lmerTest)


# Load data files ---------------------------------------------------------
source(here("preregistrations", "2_study1", "scripts", "custom_functions", "functions_analyses.R"))
load(here("data", "2_study1", "2_cleaned_data.Rdata"))
load("data/2_study1/hddm_objects.RData")


cleaned_data <- read_csv("data/2_study1/2_cleaned_data.csv") %>%
  left_join(
    reduce(
      list(
        model_std %>% mutate(id = as.numeric(id)), 
        model_deg %>% mutate(id = as.numeric(id)), 
        model_enh %>% mutate(id = as.numeric(id))), 
      left_join)
  ) %>%
  mutate(
    v_hddm_diff_std = v_con_hddm_std - v_incon_hddm_std,
    v_hddm_diff_enh = v_con_hddm_enh - v_incon_hddm_enh,
    v_hddm_diff_deg = v_con_hddm_deg - v_incon_hddm_deg,
    
    t_hddm_diff_std = t_con_hddm_std - t_incon_hddm_std,
    t_hddm_diff_enh = t_con_hddm_enh - t_incon_hddm_enh,
    t_hddm_diff_deg = t_con_hddm_deg - t_incon_hddm_deg
  )




# 1. Shrinking Spotlight Model (SSP) --------------------------------------

## 1.1 Standard - Enhanced comparison ----

mult_ssp_data_enh <- cleaned_data %>%
  select(id, vio_comp, unp_comp, matches("^(a|t0|p|sda|rd|interference)_flanker_(std|enh)"),
         scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  pivot_longer(
    cols = matches("flanker"),
    names_to = c(".value", "condition"),
    names_pattern = "(^.*_flanker)(.*)"
    ) %>%
  mutate(condition = ifelse(condition == "_std", 0, 1)) 


mult_ssp_grid_enh <- mult_ssp_data_enh |>
  multitool::add_variables("iv", vio_comp, unp_comp) |>
  multitool::add_variables("dv", a_flanker, t0_flanker, p_flanker, interference_flanker) |>
  multitool::add_filters(
    round(scale_factor, 4) != 0.9007,
    fullscreenenter == 1,
    fullscreenexit == 0,
    attention_interrupt_sum < 1,
    att_noise %in% c(0,1,2)
  ) |>
  multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
  multitool::add_model(lmer({dv} ~ {iv} * condition + (1|id))) |>
  multitool::add_postprocess("simslopes_adv", sim_slopes(pred = 'condition', modx = "{iv}", modx.values = c(-1,1))) |>
  add_postprocess("points", ggpredict(terms = c("condition [0,1]", "{iv} [-1,1]"))) |>
  multitool::expand_decisions()

mult_ssp_fit_enh <- run_multiverse(mult_ssp_grid_enh)


mult_ssp_eff_enh <- mult_ssp_fit_enh %>%
  unnest(c(lmer_fitted, specifications)) %>%
  select(decision, variables, filters, lmer_tidy) %>%
  unnest(c(filters, variables, lmer_tidy)) %>%
  select(-c(effect, group, statistic)) %>%
  filter(term %in% c("vio_comp", "unp_comp", "vio_comp:condition", "unp_comp:condition")) %>%
  mutate(
    term = case_when(
      term == "vio_comp" ~ "vio_main",
      term == "unp_comp" ~ "unp_main",
      term == "vio_comp:condition" ~ "vio_int",
      term == "unp_comp:condition" ~ "unp_int"
    )
  )

mult_ssp_simsl_enh <- mult_ssp_fit_enh %>%
  unnest(sim_slopes_fitted) %>%
  select(decision, sim_slopes_tidy) %>%
  unnest(sim_slopes_tidy) %>%
  select(-c(statistic, mod2, mod2.value, term)) 


## 1.2 Standard - Degraded comparison ----

mult_ssp_data_deg <- cleaned_data %>%
  select(id, vio_comp, unp_comp, matches("^(a|t0|p|sda|rd|interference)_flanker_(std|deg)"),
         scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  pivot_longer(
    cols = matches("flanker"),
    names_to = c(".value", "condition"),
    names_pattern = "(^.*_flanker)(.*)"
  ) %>%
  mutate(condition = ifelse(condition == "_std", 0, 1)) 


mult_ssp_grid_deg <- mult_ssp_data_deg |>
  multitool::add_variables("iv", vio_comp, unp_comp) |>
  multitool::add_variables("dv", a_flanker, t0_flanker, p_flanker, interference_flanker) |>
  multitool::add_filters(
    round(scale_factor, 4) != 0.9007,
    fullscreenenter == 1,
    fullscreenexit == 0,
    attention_interrupt_sum < 1,
    att_noise %in% c(0,1,2)
  ) |>
  multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
  multitool::add_model(lmer({dv} ~ {iv} * condition + (1|id))) |>
  multitool::add_postprocess("simslopes_adv", sim_slopes(pred = 'condition', modx = "{iv}", modx.values = c(-1,1))) |>
  add_postprocess("points", ggpredict(terms = c("condition [0,1]", "{iv} [-1,1]"))) |>
  multitool::expand_decisions()

mult_ssp_fit_deg <- run_multiverse(mult_ssp_grid_deg)


mult_ssp_eff_deg <- mult_ssp_fit_deg %>%
  unnest(c(lmer_fitted, specifications)) %>%
  select(decision, variables, filters, lmer_tidy) %>%
  unnest(c(filters, variables, lmer_tidy)) %>%
  select(-c(effect, group, statistic)) %>%
  filter(term %in% c("vio_comp", "unp_comp", "vio_comp:condition", "unp_comp:condition")) %>%
  mutate(
    term = case_when(
      term == "vio_comp" ~ "vio_main",
      term == "unp_comp" ~ "unp_main",
      term == "vio_comp:condition" ~ "vio_int",
      term == "unp_comp:condition" ~ "unp_int"
    )
  )

mult_ssp_simsl_deg <- mult_ssp_fit_deg %>%
  unnest(sim_slopes_fitted) %>%
  select(decision, sim_slopes_tidy) %>%
  unnest(sim_slopes_tidy) %>%
  select(-c(statistic, mod2, mod2.value, term)) 



# 2. Hierarchical Bayesian DDM (HDDM) -------------------------------------

## 2.1 Standard - Enhanced comparison ----

mult_hddm_data_enh <- cleaned_data %>%
  select(id, vio_comp, unp_comp, a_hddm_std, a_hddm_enh, matches("^(v|t)_hddm_diff_(std|enh)"),
         scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  pivot_longer(
    cols = matches("hddm"),
    names_to = c("parameter", "condition"),
    values_to = "value",
    names_pattern = "(.*hddm.*)(std|enh)"
  ) %>%
  mutate(
    parameter = str_replace_all(parameter, "_$", ""),
    condition = ifelse(condition == "std", 0, 1)
  ) %>%
  pivot_wider(names_from = "parameter", values_from = "value")


mult_hddm_grid_enh <- mult_hddm_data_enh |>
  multitool::add_variables("iv", vio_comp, unp_comp) |>
  multitool::add_variables("dv", a_hddm, v_hddm_diff, t_hddm_diff) |>
  multitool::add_filters(
    round(scale_factor, 4) != 0.9007,
    fullscreenenter == 1,
    fullscreenexit == 0,
    attention_interrupt_sum < 1,
    att_noise %in% c(0,1,2)
  ) |>
  multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
  multitool::add_model(lmer({dv} ~ {iv} * condition + (1|id))) |>
  multitool::add_postprocess("simslopes_adv", sim_slopes(pred = 'condition', modx = "{iv}", modx.values = c(-1,1))) |>
  add_postprocess("points", ggpredict(terms = c("condition [0,1]", "{iv} [-1,1]"))) |>
  multitool::expand_decisions()


mult_hddm_fit_enh <- run_multiverse(mult_hddm_grid_enh)


mult_hddm_eff_enh <- mult_hddm_fit_enh %>%
  unnest(c(lmer_fitted, specifications)) %>%
  select(decision, variables, filters, lmer_tidy) %>%
  unnest(c(filters, variables, lmer_tidy)) %>%
  select(-c(effect, group, statistic)) %>%
  filter(term %in% c("vio_comp", "unp_comp", "vio_comp:condition", "unp_comp:condition")) %>%
  mutate(
    term = case_when(
      term == "vio_comp" ~ "vio_main",
      term == "unp_comp" ~ "unp_main",
      term == "vio_comp:condition" ~ "vio_int",
      term == "unp_comp:condition" ~ "unp_int"
    )
  )

mult_hddm_simsl_enh <- mult_hddm_fit_enh %>%
  unnest(sim_slopes_fitted) %>%
  select(decision, sim_slopes_tidy) %>%
  unnest(sim_slopes_tidy) %>%
  select(-c(statistic, mod2, mod2.value, term)) 




## 2.2 Standard - Degraded comparison ----

mult_hddm_data_deg <- cleaned_data %>%
  select(id, vio_comp, unp_comp, a_hddm_std, a_hddm_deg, matches("^(v|t)_hddm_diff_(std|deg)"),
         scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  pivot_longer(
    cols = matches("hddm"),
    names_to = c("parameter", "condition"),
    values_to = "value",
    names_pattern = "(.*hddm.*)(std|deg)"
  ) %>%
  mutate(
    parameter = str_replace_all(parameter, "_$", ""),
    condition = ifelse(condition == "std", 0, 1)
  ) %>%
  pivot_wider(names_from = "parameter", values_from = "value")


mult_hddm_grid_deg <- mult_hddm_data_deg |>
  multitool::add_variables("iv", vio_comp, unp_comp) |>
  multitool::add_variables("dv", a_hddm, v_hddm_diff, t_hddm_diff) |>
  multitool::add_filters(
    round(scale_factor, 4) != 0.9007,
    fullscreenenter == 1,
    fullscreenexit == 0,
    attention_interrupt_sum < 1,
    att_noise %in% c(0,1,2)
  ) |>
  multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
  multitool::add_model(lmer({dv} ~ {iv} * condition + (1|id))) |>
  multitool::add_postprocess("simslopes_adv", sim_slopes(pred = 'condition', modx = "{iv}", modx.values = c(-1,1))) |>
  add_postprocess("points", ggpredict(terms = c("condition [0,1]", "{iv} [-1,1]"))) |>
  multitool::expand_decisions()

mult_hddm_fit_deg <- run_multiverse(mult_hddm_grid_deg)


mult_hddm_eff_deg <- mult_hddm_fit_deg %>%
  unnest(c(lmer_fitted, specifications)) %>%
  select(decision, variables, filters, lmer_tidy) %>%
  unnest(c(filters, variables, lmer_tidy)) %>%
  select(-c(effect, group, statistic)) %>%
  filter(term %in% c("vio_comp", "unp_comp", "vio_comp:condition", "unp_comp:condition")) %>%
  mutate(
    term = case_when(
      term == "vio_comp" ~ "vio_main",
      term == "unp_comp" ~ "unp_main",
      term == "vio_comp:condition" ~ "vio_int",
      term == "unp_comp:condition" ~ "unp_int"
    )
  )

mult_hddm_simsl_deg <- mult_hddm_fit_deg %>%
  unnest(sim_slopes_fitted) %>%
  select(decision, sim_slopes_tidy) %>%
  unnest(sim_slopes_tidy) %>%
  select(-c(statistic, mod2, mod2.value, term)) 