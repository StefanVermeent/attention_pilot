load(file = "primary_multiverse_objects.RData")
source("bootstrap_multiverse.R")

# Prepare data ------------------------------------------------------------

# Pilot

# Pilot study
pilot_data <- read_csv("~/data/1_pilot/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         unp_comp, vio_comp, rt_flanker_congruent, rt_flanker_incongruent, acc_flanker_congruent, acc_flanker_incongruent,
         a_flanker = flanker_ssp_a, t0_flanker = flanker_ssp_t0, p_flanker = flanker_ssp_p, interference_flanker = flanker_ssp_interference) %>%
  mutate(
    id = str_c("pilot_", id),
    study = 'pilot',
    counterbalance = 'pilot',
    scale_factor = ifelse(round(scale_factor, 4) == 0.3081, -1,1)
  ) |> 
  mutate(
    a_flanker           = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  )




# Study 1

study1_data <-  read_csv("~/data/2_study1/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         vio_comp, unp_comp, rt_flanker_congruent_std, rt_flanker_incongruent_std, acc_flanker_congruent_std, acc_flanker_incongruent_std,
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
    study        = 'study1',
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, -1,1)
  ) |> 
  mutate(
    a_flanker    = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) 

# Study 2
study2_data <-  read_csv("2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         vio_comp, unp_comp, rt_flanker_congruent, rt_flanker_incongruent, acc_flanker_congruent, acc_flanker_incongruent,
         a_flanker, t0_flanker, p_flanker, interference_flanker,
         rt_globloc_global, rt_globloc_local, acc_globloc_global, acc_globloc_local,
         a_globloc_fixed, v_globloc_local, v_globloc_global, t0_globloc_local, t0_globloc_global
  ) %>%
  mutate(
    id               = str_c("study2_", id),
    study            = 'study2',
    scale_factor     = ifelse(round(scale_factor, 4) == 0.9007, -1,1),
    v_globloc_diff   = v_globloc_global - v_globloc_local 
  ) |> 
  mutate(
    a_flanker    = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) 

study2_prim_aim2_data <- study2_data |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         vio_comp, unp_comp, v_globloc_local, v_globloc_global) |> 
  pivot_longer(c(starts_with("v_")), names_to = "condition", values_to = "v_globloc") |> 
  mutate(condition = ifelse(str_detect(condition, "local$"), -1, 1))


study2_prim_aim3_data <- study2_data |> 
  mutate(
    id = str_remove(id, "study2_"),
    v_globloc_diff = scale(v_globloc_diff) |> as.numeric(), 
    p_flanker      = scale(p_flanker) |> as.numeric()
  ) |> 
  pivot_longer(c(v_globloc_diff, p_flanker), names_to = 'task', values_to = "value") |> 
  mutate(task = ifelse(str_detect(task, "^p_"), -1, 1)) 



# Combine data of pilot study and study 1
study2_prim_aim1_data <- bind_rows(pilot_data, study1_data, study2_data) |> 
  filter(is.finite(interference_flanker)) |> 
  mutate(rt_diff = rt_flanker_incongruent - rt_flanker_congruent) |> 
  mutate(study = faux::contr_code_sum(study))


study2_prim_ssp_data <- cleaned_data %>%
  select(id, vio_comp, unp_comp, starts_with("rt"), matches("^(a|t0|p|sda|rd|interference)_flanker$"),
         scale_factor, fullscreenenter, meta_captcha, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  mutate(
    rt_diff = rt_flanker_incongruent - rt_flanker_congruent,
  ) |> 
  select(-matches("rt_flanker_(congruent|incongruent)")) |> 
  mutate(
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, -1, 1),
  )


# 1. Bootstrapped multiverse p-values -------------------------------------

## Primary Aim 3: Within-subject Global Local and Flanker Task ----

study2_aim1_vio_boot <- bootstrap_multiverse(data = study2_prim_aim1_data, mult = study2_prim_aim1_mult, effect = "vio_comp", n_bootstrap = 500, cores = 15)
study2_aim1_unp_boot <- bootstrap_multiverse(data = study2_prim_aim1_data, mult = study2_prim_aim1_mult, effect = "unp_comp", n_bootstrap = 500, cores = 15)

study2_aim2_vio_boot_main1 <- bootstrap_multiverse(data = study2_prim_aim2_data, mult = study2_prim_aim2_mult, grouping_id = "id", effect = "vio_comp", n_bootstrap = 500, cores = 15)
study2_aim2_vio_boot_main2 <- bootstrap_multiverse(data = study2_prim_aim2_data, mult = study2_prim_aim2_mult, grouping_id = "id", effect = "condition", n_bootstrap = 500, cores = 15)
study2_aim2_vio_boot_int   <- bootstrap_multiverse(data = study2_prim_aim2_data, mult = study2_prim_aim2_mult, grouping_id = "id", effect = "vio_comp:condition", n_bootstrap = 500, cores = 15)

study2_aim2_unp_boot_main1 <- bootstrap_multiverse(data = study2_prim_aim2_data, mult = study2_prim_aim2_mult, grouping_id = "id", effect = "unp_comp", n_bootstrap = 500, cores = 15)
study2_aim2_unp_boot_main2 <- bootstrap_multiverse(data = study2_prim_aim2_data, mult = study2_prim_aim2_mult, grouping_id = "id", effect = "condition", n_bootstrap = 500, cores = 15)
study2_aim2_unp_boot_int   <- bootstrap_multiverse(data = study2_prim_aim2_data, mult = study2_prim_aim2_mult, grouping_id = "id", effect = "unp_comp:condition", n_bootstrap = 500, cores = 15)


study2_aim3_vio_boot_main1 <- bootstrap_multiverse(data = study2_prim_aim3_data, mult = study2_prim_aim3_mult, grouping_id = "id", effect = "vio_comp", n_bootstrap = 500, cores = 15)
study2_aim3_vio_boot_main2 <- bootstrap_multiverse(data = study2_prim_aim3_data, mult = study2_prim_aim3_mult, grouping_id = "id", effect = "task", n_bootstrap = 500, cores = 15)
study2_aim3_vio_boot_int <- bootstrap_multiverse(data = study2_prim_aim3_data, mult = study2_prim_aim3_mult, grouping_id = "id", effect = "vio_comp:task", n_bootstrap = 500, cores = 15)

study2_aim3_unp_boot_main1 <- bootstrap_multiverse(data = study2_prim_aim3_data, mult = study2_prim_aim3_mult, grouping_id = "id", effect = "unp_comp", n_bootstrap = 500, cores = 15)
study2_aim3_unp_boot_main2 <- bootstrap_multiverse(data = study2_prim_aim3_data, mult = study2_prim_aim3_mult, grouping_id = "id", effect = "task", n_bootstrap = 500, cores = 15)
study2_aim3_unp_boot_int <- bootstrap_multiverse(data = study2_prim_aim3_data, mult = study2_prim_aim3_mult, grouping_id = "id", effect = "unp_comp:task", n_bootstrap = 500, cores = 15)


save(
  study2_aim2_vio_boot_main1, 
  study2_aim2_vio_boot_main2, 
  study2_aim2_vio_boot_int, 
  study2_aim2_unp_boot_main1,
  study2_aim2_unp_boot_main1,
  study2_aim2_unp_boot_int, 
  study2_aim3_vio_boot_main1, 
  study2_aim3_vio_boot_main2, 
  study2_aim3_vio_boot_int, 
  study2_aim3_unp_boot_main1,
  study2_aim3_unp_boot_main1,
  study2_aim3_unp_boot_int, 
  file = "preregistrations/3_study2/analysis_objects/study2_bootstrap_pvalues.RData")
