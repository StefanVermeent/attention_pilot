
# 1. Dependencies ---------------------------------------------------------------
library(tidyverse)
library(ggsci)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(here)
library(ggeffects)
library(interactions)
library(lmerTest)
library(parameters)

source("preregistrations/2_study1/scripts/custom_functions/functions_analyses.R")
cleaned_data <- read_csv("data/2_study1/2_cleaned_data.csv") 
load("preregistrations/2_study1/analysis_objects/hddm_model_objects.RData")




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



# 3. Run Multiverse analyses ----------------------------------------------

## 3.1 Pooled Data analyses ----


study1_prim_flanker_mult_pooled <- multitool::run_multiverse(
  .grid = 
    prim_ssp_data_pooled |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", a_flanker, p_flanker, t0_flanker, interference_flanker, rt_diff) |> 
    multitool::add_filters(
      scale_factor == 1,
      (scale_factor == 1 & study == 1) | study == -1,
      (scale_factor == 1 & study == -1) | study == 1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 2,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lm", lm({dv} ~ {iv} + study)) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::add_postprocess(postprocess_name = "skew", "(\\\\(x) residuals(x) |> scale() |> parameters::skewness())()") |> 
    multitool::expand_decisions()
)

## 3.2 Flanker Standard ----

study1_prim_flanker_mult_study1 <- multitool::run_multiverse(
  .grid = 
    study1_data |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", p_flanker, a_flanker, t0_flanker, interference_flanker, rt_diff) |> 
    multitool::add_filters(
      scale_factor == 1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 2,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::add_postprocess(postprocess_name = "skew", "(\\\\(x) residuals(x) |> scale() |> parameters::skewness())()") |> 
    multitool::expand_decisions()
)


## 3.3 Standard - Enhanced comparisons ----
study1_prim_flanker_mult_enh <- multitool::run_multiverse(
  .grid = 
    prim_ssp_data_enh |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", a_flanker, p_flanker, t0_flanker, interference_flanker, rt_diff) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 2,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess(postprocess_name = "ss_task", code = sim_slopes(pred = {iv}, modx = condition, modx.values = c(0,1))) |> 
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [0,1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

## 3.2 Standard - Degraded comparisons ----
study1_prim_flanker_mult_deg <- multitool::run_multiverse(
  .grid = 
    prim_ssp_data_deg |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", a_flanker, p_flanker, t0_flanker, interference_flanker, rt_diff) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 2,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess(postprocess_name = "ss_task", code = sim_slopes(pred = {iv}, modx = condition, modx.values = c(0,1))) |> 
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [0,1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

save(study1_prim_flanker_mult_pooled, study1_prim_flanker_mult_study1, study1_prim_flanker_mult_enh, study1_prim_flanker_mult_deg, file = "preregistrations/2_study1/analysis_objects/primary_mult_results.RData")


# 4. Process Results ------------------------------------------------------

## 4.1 Pooled data comparision ----

## 4.1 Standard - Enhanced comparison ----

study1_prim_ssp_points_enh <- reveal(study1_prim_flanker_mult_enh, .what = points_fitted, .which = ggpredict_full, .unpack_specs = T) |> 
  rename(level = x)


study1_prim_ssp_simslopes_enh <- reveal(study1_prim_flanker_mult_enh, .what = ss_task_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) 


# ## 4.2 Standard - Degraded comparison ----

study1_prim_ssp_points_deg <- reveal(study1_prim_flanker_mult_deg, .what = points_fitted, .which = ggpredict_full, .unpack_specs = T) |> 
  rename(level = x)



study1_prim_ssp_simslopes_deg <- reveal(study1_prim_flanker_mult_deg, .what = ss_task_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) 



# 5. Create Multiverse summaries ---------------------------------------------

## 5.1 Pooled data ----

study1_prim_ssp_pooled_effects_sum <- reveal(study1_prim_flanker_mult_pooled, .what = model_fitted, .which = lm_tidy, .unpack_specs = TRUE) |> 
  filter(
    !term %in% c("study")
  ) |> 
  left_join(
    reveal(study1_prim_flanker_mult_pooled, .what = std_coef_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  )

study1_prim_ssp_effects_sum_study1 <- reveal(study1_prim_flanker_mult_study1, .what = model_fitted, .which = lm_tidy, .unpack_specs = TRUE) |> 
  filter(
    !term %in% c("study")
  ) |> 
  left_join(
    reveal(study1_prim_flanker_mult_study1, .what = std_coef_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  )

# Store the median regression effects
study1_prim_ssp_pooled_medians_sum <- unique(study1_prim_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    study1_prim_ssp_pooled_effects_sum |> 
      filter(dv == x, !term %in% c("study", "(Intercept)")) |> 
      group_by(dv, term) |> 
      summarise(
        med_effect     = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(study1_prim_ssp_pooled_effects_sum $dv))

study1_prim_ssp_medians_sum_study1 <- unique(study1_prim_ssp_effects_sum_study1$dv) |> 
  map(function(x) {
    
    study1_prim_ssp_effects_sum_study1 |> 
      filter(dv == x, !term %in% c("study", "(Intercept)")) |> 
      group_by(dv, term) |> 
      summarise(
        med_effect     = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(study1_prim_ssp_effects_sum_study1$dv))

# Store the influence of filter decisions
study1_prim_ssp_pooled_variance_sum <- unique(study1_prim_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    data <- study1_prim_ssp_pooled_effects_sum |> 
      filter(dv == x) |> 
      filter(!term %in% c("study", "(Intercept)"))
    
    model <- lmer(Std_Coefficient ~ 1 + (1|scale_factor) + (1|fullscreenenter) + (1|fullscreenexit) + (1|attention_interrupt_sum) + (1|att_noise), data = data)
    spec_icc <- icc_specs(model) |> 
      as_tibble() |> 
      mutate(
        percent = round(percent, 2),
        grp = case_when(
          grp == 'att_noise' ~ "Noise",
          grp == 'attention_interrupt_sum' ~ "Interrupted",
          grp == "fullscreenenter" ~ "Fullscreen enter",
          grp == "fullscreenexit" ~ "Fullscreen exit",
          grp == "meta_captcha" ~ "Captcha score",
          grp == "scale_factor" ~ "Scaled",
          TRUE ~ "Residual"
        ),
        grp = factor(grp, levels = c("Residual","Fullscreen enter","Fullscreen exit","Interrupted","Noise","Scaled"))) 
    
  }) |> 
  setNames(unique(study1_prim_ssp_pooled_effects_sum$dv))

study1_prim_ssp_variance_sum_study1 <- unique(study1_prim_ssp_effects_sum_study1$dv) |> 
  map(function(x) {
    
    data <- study1_prim_ssp_effects_sum_study1 |> 
      filter(dv == x) |> 
      filter(!term %in% c("study", "(Intercept)"))
    
    model <- lmer(Std_Coefficient ~ 1 + (1|scale_factor) + (1|fullscreenenter) + (1|fullscreenexit) + (1|attention_interrupt_sum) + (1|att_noise), data = data)
    spec_icc <- icc_specs(model) |> 
      as_tibble() |> 
      mutate(
        percent = round(percent, 2),
        grp = case_when(
          grp == 'att_noise' ~ "Noise",
          grp == 'attention_interrupt_sum' ~ "Interrupted",
          grp == "fullscreenenter" ~ "Fullscreen enter",
          grp == "fullscreenexit" ~ "Fullscreen exit",
          grp == "meta_captcha" ~ "Captcha score",
          grp == "scale_factor" ~ "Scaled",
          TRUE ~ "Residual"
        ),
        grp = factor(grp, levels = c("Residual","Fullscreen enter","Fullscreen exit","Interrupted","Noise","Scaled"))) 
    
  }) |> 
  setNames(unique(study1_prim_ssp_effects_sum_study1$dv))

## 5.2 Standard - Enhanced comparison ----


study1_prim_ssp_enh_effects_sum <- reveal(study1_prim_flanker_mult_enh, .what = model_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
  filter(
    effect == 'fixed',
    term != "(Intercept)"
  ) |> 
  left_join(
    reveal(study1_prim_flanker_mult_enh, .what = std_coef_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  )

# Store the median effects
study1_prim_ssp_enh_medians_sum <- unique(study1_prim_ssp_enh_effects_sum$dv) |> 
  map(function(x) {
    
    study1_prim_ssp_enh_effects_sum |> 
      filter(dv == x, str_detect(term, ":")) |> 
      mutate(term = ifelse(str_detect(term, ":"), "interaction", "main effect")) |> 
      group_by(dv, term) |> 
      summarise(
        med_effect = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(study1_prim_ssp_enh_effects_sum$dv))


# Store the plotting points per decision
study1_prim_ssp_enh_points_sum <- unique(study1_prim_ssp_points_enh$dv) |> 
  map(function(x){
    
    study1_prim_ssp_points_enh |> 
      filter(dv == x) |> 
      left_join(study1_prim_ssp_enh_effects_sum |> filter(str_detect(term, ":")) |>  select(decision, dv, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(study1_prim_ssp_points_enh$dv))


# Store the influence of filter decisions
study1_prim_ssp_enh_decisions_sum <- unique(study1_prim_ssp_enh_effects_sum$dv) |> 
  map(function(x) {
    
    study1_prim_ssp_enh_effects_sum |> 
      filter(dv == x) |> 
      filter(str_detect(term, ":")) |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(dv, filters, setting) |> 
      summarise(
        sum_pvalue = sum(p.value < .05) / n() * 100,
        med_effect = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        se = sd(Std_Coefficient, na.rm = T)/sqrt(n())
        ) |> 
      ungroup() |> 
      mutate(
        sum_pvalue_chr = paste(as.character(round(sum_pvalue, 1)), "%"),
        filters = case_when(
          filters == 'att_noise' ~ "Noise",
          filters == 'attention_interrupt_sum' ~ "Interrupted",
          filters == "fullscreenenter" ~ "Fullscreen enter",
          filters == "fullscreenexit" ~ "fullscreen exit",
          filters == "meta_captcha" ~ "Captcha score",
          filters == "scale_factor" ~ "Scaled"
        ),
        filters_plot = ifelse(str_detect(setting, "%in% unique"), 
                              paste0(filters, " - ", "incl"),
                              paste0(filters, " - ", "excl")))
  }) |> 
  setNames(unique(study1_prim_ssp_enh_effects_sum$dv))


study1_prim_ssp_enh_variance_sum <- unique(study1_prim_ssp_enh_effects_sum$dv) |> 
  map(function(x) {
    
    data <- study1_prim_ssp_enh_effects_sum |> 
      filter(dv == x) |> 
      filter(str_detect(term, ":"))
    
    model <- lmer(Std_Coefficient ~ 1 + (1|scale_factor) + (1|fullscreenenter) + (1|fullscreenexit) + (1|attention_interrupt_sum) + (1|att_noise), data = data)
    spec_icc <- icc_specs(model) |> 
      as_tibble() |> 
      mutate(
        percent = round(percent, 2),
        grp = case_when(
          grp == 'att_noise' ~ "Noise",
          grp == 'attention_interrupt_sum' ~ "Interrupted",
          grp == "fullscreenenter" ~ "Fullscreen enter",
          grp == "fullscreenexit" ~ "Fullscreen exit",
          grp == "meta_captcha" ~ "Captcha score",
          grp == "scale_factor" ~ "Scaled",
          TRUE ~ "Residual"
        ),
        grp = factor(grp, levels = c("Residual","Fullscreen enter","Fullscreen exit","Interrupted","Noise","Scaled"))) 
    
  }) |> 
  setNames(unique(study1_prim_ssp_enh_effects_sum$dv))



## 5.3 Standard - Degraded comparison ----

study1_prim_ssp_deg_effects_sum <- reveal(study1_prim_flanker_mult_deg, .what = model_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
  filter(
    effect == 'fixed',
    term != "(Intercept)"
  ) |> 
  left_join(
    reveal(study1_prim_flanker_mult_deg, .what = std_coef_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  )

# Store the median effects
study1_prim_ssp_deg_medians_sum <- unique(study1_prim_ssp_deg_effects_sum$dv) |> 
  map(function(x) {
    
    study1_prim_ssp_deg_effects_sum |> 
      filter(dv == x, str_detect(term, ":")) |> 
      mutate(term = ifelse(str_detect(term, ":"), "interaction", "main effect")) |> 
      group_by(dv, term) |> 
      summarise(
        med_effect = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(study1_prim_ssp_deg_effects_sum$dv))


# Store the plotting points per decision
study1_prim_ssp_deg_points_sum <- unique(study1_prim_ssp_points_deg$dv) |> 
  map(function(x){
    
    study1_prim_ssp_points_deg |> 
      filter(dv == x) |> 
      left_join(study1_prim_ssp_deg_effects_sum |> filter(str_detect(term, ":")) |>  select(decision, dv, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(study1_prim_ssp_points_deg$dv))


# Store the influence of filter decisions
study1_prim_ssp_deg_decisions_sum <- unique(study1_prim_ssp_deg_effects_sum$dv) |> 
  map(function(x) {
    
    study1_prim_ssp_deg_effects_sum |> 
      filter(dv == x) |> 
      filter(str_detect(term, ":")) |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(dv, filters, setting) |> 
      summarise( sum_pvalue = sum(p.value < .05) / n() * 100) |> 
      ungroup() |> 
      mutate(
        sum_pvalue_chr = paste(as.character(round(sum_pvalue, 1)), "%"),
        filters = case_when(
          filters == 'att_noise' ~ "Noise",
          filters == 'attention_interrupt_sum' ~ "Interrupted",
          filters == "fullscreenenter" ~ "Fullscreen enter",
          filters == "fullscreenexit" ~ "fullscreen exit",
          filters == "meta_captcha" ~ "Captcha score",
          filters == "scale_factor" ~ "Scaled"
        ),
        filters_plot = ifelse(str_detect(setting, "%in% unique"), 
                              paste0(filters, " - ", "incl"),
                              paste0(filters, " - ", "excl")))
  }) |> 
  setNames(unique(study1_prim_ssp_deg_effects_sum$dv))

study1_prim_ssp_deg_variance_sum <- unique(study1_prim_ssp_deg_effects_sum$dv) |> 
  map(function(x) {
    
    data <- study1_prim_ssp_deg_effects_sum |> 
      filter(dv == x) |> 
      filter(str_detect(term, ":"))
    
    model <- lmer(Std_Coefficient ~ 1 + (1|scale_factor) + (1|fullscreenenter) + (1|fullscreenexit) + (1|attention_interrupt_sum) + (1|att_noise), data = data)
    spec_icc <- icc_specs(model) |> 
      as_tibble() |> 
      mutate(
        percent = round(percent, 2),
        grp = case_when(
          grp == 'att_noise' ~ "Noise",
          grp == 'attention_interrupt_sum' ~ "Interrupted",
          grp == "fullscreenenter" ~ "Fullscreen enter",
          grp == "fullscreenexit" ~ "Fullscreen exit",
          grp == "meta_captcha" ~ "Captcha score",
          grp == "scale_factor" ~ "Scaled",
          TRUE ~ "Residual"
        ),
        grp = factor(grp, levels = c("Residual","Fullscreen enter","Fullscreen exit","Interrupted","Noise","Scaled"))) 
    
  }) |> 
  setNames(unique(study1_prim_ssp_deg_effects_sum$dv))


save(study1_prim_ssp_points_enh, study1_prim_ssp_simslopes_enh, study1_prim_ssp_points_deg, study1_prim_ssp_simslopes_deg, 
     study1_prim_ssp_pooled_effects_sum, study1_prim_ssp_effects_sum_study1, study1_prim_ssp_pooled_medians_sum, study1_prim_ssp_medians_sum_study1, study1_prim_ssp_pooled_variance_sum, study1_prim_ssp_variance_sum_study1,
     study1_prim_ssp_enh_effects_sum, study1_prim_ssp_enh_medians_sum, study1_prim_ssp_enh_points_sum, study1_prim_ssp_enh_decisions_sum, study1_prim_ssp_enh_variance_sum,
     study1_prim_ssp_deg_effects_sum, study1_prim_ssp_deg_medians_sum, study1_prim_ssp_deg_points_sum, study1_prim_ssp_deg_decisions_sum, study1_prim_ssp_deg_variance_sum,
     file = "preregistrations/2_study1/analysis_objects/primary_multiverse_summaries.RData")
