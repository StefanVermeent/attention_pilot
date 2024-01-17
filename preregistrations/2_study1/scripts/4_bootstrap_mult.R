
source("preregistrations/2_study1/scripts/custom_functions/functions_analyses.R")
cleaned_data <- read_csv("data/2_study1/2_cleaned_data.csv") 

load("preregistrations/2_study1/analysis_objects/primary_mult_results.RData")

# 2. Prepare data ------------------------------------------------------------

# Pilot study
pilot_data <- read_csv("data/1_pilot/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         vio_comp, rt_flanker_congruent, rt_flanker_incongruent, acc_flanker_congruent, acc_flanker_incongruent,
         a_flanker = flanker_ssp_a, t0_flanker = flanker_ssp_t0, p_flanker = flanker_ssp_p, interference_flanker = flanker_ssp_interference) %>%
  mutate(
    id = str_c("pilot_", id),
    study = -1,
    counterbalance = 'pilot',
    scale_factor = ifelse(round(scale_factor, 4) == 0.3081, 0,1),
    rt_diff      = rt_flanker_congruent - rt_flanker_incongruent
  ) |> 
  filter(!is.infinite(interference_flanker)) |> 
  mutate(
    a_flanker                = log(a_flanker),
    interference_flanker     = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker))


# Study 1

study1_data <-  read_csv("data/2_study1/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         vio_comp, rt_flanker_congruent_std, rt_flanker_incongruent_std, acc_flanker_congruent_std, acc_flanker_incongruent_std,
         a_flanker_std, t0_flanker_std, p_flanker_std, interference_flanker_std) %>%
  rename(rt_flanker_congruent = rt_flanker_congruent_std,
         rt_flanker_incongruent = rt_flanker_incongruent_std,
         acc_flanker_congruent = acc_flanker_congruent_std,
         acc_flanker_incongruent = acc_flanker_incongruent_std,
         a_flanker = a_flanker_std,
         t0_flanker = t0_flanker_std,
         p_flanker = p_flanker_std,
         interference_flanker = interference_flanker_std) %>%
  mutate(
    id           = str_c("study1_", id),
    study        = 1,
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 0,1),
    rt_diff      = log(rt_flanker_congruent) - log(rt_flanker_incongruent),
    a_flanker    = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) 

# Combine data of pilot study and study 1
prim_ssp_data_pooled <- bind_rows(pilot_data, study1_data) 



prim_ssp_data_enh <- cleaned_data %>%
  select(id, vio_comp, unp_comp, starts_with("rt"), matches("^(a|t0|p|sda|rd|interference)_flanker_(std|enh)"),
         scale_factor, fullscreenenter, meta_captcha, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  mutate(
    rt_flanker_std = rt_flanker_incongruent_std - rt_flanker_congruent_std,
    rt_flanker_enh = rt_flanker_incongruent_enh - rt_flanker_congruent_enh,
  ) |> 
  select(-matches("rt_flanker_(congruent|incongruent)")) |> 
  pivot_longer(
    cols = matches("flanker"),
    names_to = c(".value", "condition"),
    names_pattern = "(^.*_flanker)(.*)"
  ) %>%
  rename(rt_diff = rt_flanker) |> 
  mutate(
    condition = ifelse(condition == "_std", 0, 1),
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 1, -1),
    a_flanker    = log(a_flanker)
  ) |> 
  group_by(condition) |> 
  mutate(
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) |> 
  ungroup()

prim_ssp_data_deg <- cleaned_data %>%
  select(id, vio_comp, unp_comp, starts_with("rt"), matches("^(a|t0|p|sda|rd|interference)_flanker_(std|deg)"),
         scale_factor, fullscreenenter, meta_captcha, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  mutate(
    rt_flanker_std = rt_flanker_incongruent_std - rt_flanker_congruent_std,
    rt_flanker_deg = rt_flanker_incongruent_deg - rt_flanker_congruent_deg,
  ) |> 
  select(-matches("rt_flanker_(congruent|incongruent)")) |> 
  pivot_longer(
    cols = matches("flanker"),
    names_to = c(".value", "condition"),
    names_pattern = "(^.*_flanker)(.*)"
  ) %>%
  rename(rt_diff = rt_flanker) |> 
  mutate(
    condition = ifelse(condition == "_std", 0, 1),
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 1, -1),
    a_flanker    = log(a_flanker)
  ) |> 
  group_by(condition) |> 
  mutate(
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) |> 
  ungroup()



# 1. Bootstrapped multiverse p-values -------------------------------------

# Aim 1
study1_vio_boot <- bootstrap_multiverse(data = study1_data, mult = study1_prim_flanker_mult_study1, effect = "vio_comp", n_bootstrap = 500, cores = 15)
study1_unp_boot <- bootstrap_multiverse(data = study1_data, mult = study1_expl_flanker_mult_study1, effect = "unp_comp", n_bootstrap = 500, cores = 15)

study1_pooled_vio_boot <- bootstrap_multiverse(data = prim_ssp_data_pooled, mult = study1_prim_flanker_mult_pooled, effect = "vio_comp", n_bootstrap = 500, cores = 6)

# Aim 2
study1_enh_vio_boot <- bootstrap_multiverse(data = prim_ssp_data_enh, mult = study1_prim_flanker_mult_enh, grouping_id = "id", effect = "vio_comp:condition", n_bootstrap = 500, cores = 6)
study1_deg_vio_boot <- bootstrap_multiverse(data = prim_ssp_data_deg, mult = study1_prim_flanker_mult_deg, grouping_id = "id", effect = "vio_comp:task", n_bootstrap = 500, cores = 6)


save(study1_vio_boot, study1_unp_boot, study1_pooled_vio_boot, study1_enh_vio_boot, study1_deg_vio_boot, file = "preregistrations/2_study1/analysis_objects/study1_bootstrap_pvalues.RData")
