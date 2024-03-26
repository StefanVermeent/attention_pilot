
# 1. Dependencies ---------------------------------------------------------------
library(tidyverse)
library(ggsci)
library(multitool) 
library(here)
library(ggeffects)
library(interactions)
library(lmerTest)
library(parameters)
library(specr)

source("preregistrations/3_study2/scripts/custom_functions/functions_analyses.R")

load("data/3_study2/2_cleaned_data.rData") 

load("preregistrations/3_study2/analysis_objects/hddm_model_objects.RData")

# 2. Prepare data ------------------------------------------------------------


# Pilot study
pilot_data <- read_csv("data/1_pilot/2_cleaned_data.csv") |> 
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

study1_data <-  read_csv("data/2_study1/2_cleaned_data.csv") |> 
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
study2_data <-  read_csv("data/3_study2/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         vio_comp, unp_comp, rt_flanker_congruent, rt_flanker_incongruent, acc_flanker_congruent, acc_flanker_incongruent,
         a_flanker, t0_flanker, p_flanker, interference_flanker,
         rt_globloc_global, rt_globloc_local, acc_globloc_global, acc_globloc_local,
         a_globloc_fixed = hddm_a_fixed, v_globloc_local = hddm_v_local, v_globloc_global = hddm_v_global, t0_globloc_local = hddm_t0_local, t0_globloc_global = hddm_t0_global
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


# 3. Run Multiverse analyses ----------------------------------------------

## 3.1 Primary Aim 1: Pooled Data analyses ----

study2_prim_aim1_mult <- multitool::run_multiverse(
  .grid = 
    study2_prim_aim1_data |> 
    multitool::add_variables("iv", vio_comp, unp_comp) |> 
    multitool::add_variables("dv", p_flanker, interference_flanker) |> 
    multitool::add_filters(
      fullscreenenter == 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2),
      scale_factor == 1,
      fullscreenexit == 0, 
      attention_interrupt_sum < 1
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lm", lm({dv} ~ {iv} + study)) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardise_parameters()') |> 
    multitool::add_postprocess(postprocess_name = "skew", "(\\\\(x) residuals(x) |> scale() |> parameters::skewness())()") |> 
    multitool::add_postprocess(postprocess_name = "kurtosis", "(\\\\(x) residuals(x) |> scale() |> parameters::kurtosis())()") |> 
    multitool::expand_decisions()
)



study2_prim_aim1_mult_study2 <- multitool::run_multiverse(
  .grid = 
    study2_data |> 
    multitool::add_variables("iv", vio_comp, unp_comp) |> 
    multitool::add_variables("dv", p_flanker, interference_flanker) |> 
    multitool::add_filters(
      fullscreenenter == 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2),
      scale_factor == 1,
      fullscreenexit == 0, 
      attention_interrupt_sum < 1
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardise_parameters()') |> 
    multitool::add_postprocess(postprocess_name = "skew", "(\\\\(x) residuals(x) |> scale() |> parameters::skewness())()") |> 
    multitool::add_postprocess(postprocess_name = "kurtosis", "(\\\\(x) residuals(x) |> scale() |> parameters::kurtosis())()") |> 
    multitool::expand_decisions()
)

## 3.2 Primary Aim 2: Global Local Task ----

study2_prim_aim2_mult <- multitool::run_multiverse(
  .grid = 
    study2_prim_aim2_data |> 
    multitool::add_variables("iv", vio_comp, unp_comp) |> 
    multitool::add_variables("dv", v_globloc) |> 
    multitool::add_filters(
      fullscreenenter == 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2),
      scale_factor == 1,
      fullscreenexit == 0, 
      attention_interrupt_sum < 1
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess(postprocess_name = "ss_task", hypothesis_test(terms = c("{iv}[-1,1]", "condition[-1,1]"), re.form = NA, test = NULL)) |> 
    multitool::add_postprocess(postprocess_name = "ss_adversity", hypothesis_test(terms = c("condition[-1,1]", "{iv}[-1,1]"), re.form = NA, test = NULL)) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv}[-1,1]", "condition[-1, 1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::add_postprocess(postprocess_name = "skew", "(\\\\(x) residuals(x) |> scale() |> parameters::skewness())()") |> 
    multitool::add_postprocess(postprocess_name = "kurtosis", "(\\\\(x) residuals(x) |> scale() |> parameters::kurtosis())()") |> 
    multitool::expand_decisions()
)


## 3.3 Primary Aim 3: Within-subject Global Local and Flanker ----

study2_prim_aim3_mult <- multitool::run_multiverse(
  .grid = 
    study2_prim_aim3_data |> 
    multitool::add_variables("iv", vio_comp, unp_comp) |> 
    multitool::add_variables("dv", value) |> 
    multitool::add_filters(
      fullscreenenter == 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2),
      scale_factor == 1,
      fullscreenexit == 0, 
      attention_interrupt_sum < 1
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * task + (1|id))) |>    
    multitool::add_postprocess(postprocess_name = "ss_task", hypothesis_test(terms = c("{iv}[-1,1]" , "task[-1,1]"), re.form = NA, test = NULL)) |>
    multitool::add_postprocess(postprocess_name = "ss_adversity", hypothesis_test(terms = c("task[-1,1]", "{iv}[-1,1]"), re.form = NA, test = NULL)) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "task [-1, 1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::add_postprocess(postprocess_name = "skew", "(\\\\(x) residuals(x) |> scale() |> parameters::skewness())()") |> 
    multitool::add_postprocess(postprocess_name = "kurtosis", "(\\\\(x) residuals(x) |> scale() |> parameters::kurtosis())()") |> 
    multitool::expand_decisions() 
)

save(study2_prim_aim1_mult, study2_prim_aim1_mult_study2, study2_prim_aim2_mult, study2_prim_aim3_mult,
     file = "preregistrations/3_study2/analysis_objects/primary_multiverse_objects.RData")

# 4. Process Results ------------------------------------------------------


## 4.1 Primary Aim 2: Global Local Task ----

study2_prim_aim2_points <- reveal(study2_prim_aim2_mult, .what = points_fitted, .which = points_full, .unpack_specs = "wide") |> 
  rename(level = x) |> 
  mutate(dv_iv = paste0(dv, ".", iv))

study2_prim_aim2_simslopes1 <- reveal(study2_prim_aim2_mult, .what = ss_task_fitted, .which = ss_task_full, .unpack_specs = "wide")  |> 
  select(decision, iv, dv, condition, Slope, conf.low, conf.high, p.value) |> 
  mutate(dv_iv = paste0(dv, ".", iv)) 

study2_prim_aim2_simslopes2 <- reveal(study2_prim_aim2_mult, .what = ss_adversity_fitted, .which = ss_adversity_full, .unpack_specs = "wide")  |> 
  mutate(
    adversity = case_when(
      vio_comp == -1 | unp_comp == -1 ~ -1,
      vio_comp == 1  | unp_comp ==  1 ~ 1
    )
  ) |> 
  mutate(dv_iv = paste0(dv, ".", iv))  |> 
  select(decision, dv_iv, adversity, Slope, conf.low, conf.high, p.value)
  


# ## 4.2 Primary Aim 3: Within-subject Global Local Task ----

study2_prim_aim3_points <- reveal(study2_prim_aim3_mult, .what = points_fitted, .which = points_full, .unpack_specs = 'wide') |> 
  rename(adversity = x) |> 
  mutate(dv_iv = paste0(dv, ".", iv)) 

study2_prim_aim3_simslopes1 <- reveal(study2_prim_aim3_mult, .what = ss_task_fitted, .which = ss_task_full, .unpack_specs = 'wide')  |> 
  select(decision, iv, dv, task, Slope, conf.low, conf.high, p.value) |> 
  mutate(dv_iv = paste0(dv, ".", iv)) 

study2_prim_aim3_simslopes2 <- reveal(study2_prim_aim3_mult, .what = ss_adversity_fitted, .which = ss_adversity_full, .unpack_specs = 'wide')  |> 
  mutate(
    adversity = case_when(
      vio_comp == -1 | unp_comp == -1 ~ -1,
      vio_comp == 1  | unp_comp ==  1 ~ 1
    )
  ) |> 
  select(decision, iv, dv, adversity, Slope, conf.low, conf.high, p.value) |> 
  mutate(dv_iv = paste0(dv, ".", iv)) 

# 5. Create Multiverse summaries ---------------------------------------------

## 5.1 Primary Aim 1: Pooled data ----

study2_prim_aim1_effects_sum <- reveal(study2_prim_aim1_mult, .what = model_fitted, .which = model_parameters, .unpack_specs = "wide") |> 
  filter(
    !str_detect(parameter, "study")
  ) |> 
  left_join(
    reveal(study2_prim_aim1_mult, .what = std_coef_fitted, matches("full"), .unpack_specs = 'wide') |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, parameter = Parameter, starts_with("Std"), -ends_with("warnings"), -ends_with("messages"))
  ) |> 
  mutate(dv_iv = paste0(dv, ".", iv))

study2_prim_aim1_effects_sum_study2 <- reveal(study2_prim_aim1_mult_study2, .what = model_fitted, .which = model_parameters, .unpack_specs = 'wide') |> 
  left_join(
    reveal(study2_prim_aim1_mult_study2, .what = std_coef_fitted, matches("full"), .unpack_specs = 'wide') |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, parameter = Parameter, starts_with("Std"), -ends_with("warnings"), -ends_with("messages"))
  ) |> 
  mutate(dv_iv = paste0(dv, ".", iv))

# Store the median regression effects
study2_prim_aim1_medians_sum <-
  unique(study2_prim_aim1_effects_sum$dv_iv) |> 
  map(function(x) {
    
    study2_prim_aim1_effects_sum |> 
      filter(dv_iv == x, !parameter %in% c("(Intercept)")) |> 
      group_by(dv_iv, parameter) |> 
      summarise(
        med_effect     = median(coefficient, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p < .05) / n() * 100
      ) |> 
      mutate(parameter = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(
        names_from = parameter,
        values_from = c(med_effect, med_effect_std, sum_pvalue)
      )
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum$dv_iv))

study2_prim_aim1_medians_sum_study2 <-
  unique(study2_prim_aim1_effects_sum_study2$dv_iv) |> 
  map(function(x) {
    
    study2_prim_aim1_effects_sum_study2 |> 
      filter(dv_iv == x, !parameter %in% c("(Intercept)")) |> 
      group_by(dv_iv, parameter) |> 
      summarise(
        med_effect     = median(coefficient, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(
        names_from = parameter,
        values_from = c(med_effect, med_effect_std, sum_pvalue)
      )
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum_study2$dv_iv))



# Store the influence of filter decisions
study2_prim_aim1_variance_sum <- unique(study2_prim_aim1_effects_sum$dv_iv) |> 
  map(function(x) {
    
    data <- study2_prim_aim1_effects_sum |> 
      filter(dv_iv == x) |> 
      filter(!parameter %in% c("study", "(Intercept)"), !str_detect(parameter, "study"))
    
    model <- lmer(Std_Coefficient ~ 1 + (1|scale_factor) + (1|fullscreenexit) + (1|fullscreenenter) + (1|att_noise) + (1|meta_captcha) + (1|attention_interrupt_sum), data = data)
    spec_icc <- icc_specs(model) |> 
      as_tibble() |> 
      mutate(
        percent = round(percent, 2),
        grp = case_when(
          grp == "scale_factor" ~ "Scaling",
          grp == "fullscreenexit" ~ "Fullscreen exit",
          grp == "attention_interrupt_sum" ~ "Interruptions",
          grp == 'att_noise' ~ "Noise",
          grp == "fullscreenenter" ~ "Fullscreen enter",
          grp == "meta_captcha" ~ "Captcha score",
          TRUE ~ "Residual"
        ),
        grp = factor(grp, levels = c("Residual", "Scaling", "Fullscreen exit", "Interruptions", "Captcha score","Fullscreen enter","Noise"))) 
    
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum$dv_iv))

study2_prim_aim1_variance_sum_study2 <- unique(study2_prim_aim1_effects_sum_study2$dv_iv) |> 
  map(function(x) {
    
    data <- study2_prim_aim1_effects_sum_study2 |> 
      filter(dv_iv == x) |> 
      filter(!parameter %in% c("study", "(Intercept)"), !str_detect(parameter, "study"))
    
    model <- lmer(Std_Coefficient ~ 1 + (1|scale_factor) + (1|fullscreenexit) + (1|fullscreenenter) + (1|att_noise) + (1|meta_captcha) + (1|attention_interrupt_sum), data = data)
    spec_icc <- icc_specs(model) |> 
      as_tibble() |> 
      mutate(
        percent = round(percent, 2),
        grp = case_when(
          grp == "scale_factor" ~ "Scaling",
          grp == "fullscreenexit" ~ "Fullscreen exit",
          grp == "attention_interrupt_sum" ~ "Interruptions",
          grp == 'att_noise' ~ "Noise",
          grp == "fullscreenenter" ~ "Fullscreen enter",
          grp == "meta_captcha" ~ "Captcha score",
          TRUE ~ "Residual"
        ),
        grp = factor(grp, levels = c("Residual", "Scaling", "Fullscreen exit", "Interruptions", "Captcha score","Fullscreen enter","Noise"))) 
    
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum_study2$dv_iv))


## 5.2 Primary aim 2: Global Local Task ----

study2_prim_aim2_effects_sum <- reveal(study2_prim_aim2_mult, .what = model_fitted, .which = model_parameters, .unpack_specs = "wide") |> 
  filter(
    parameter %in% c("(Intercept)", "vio_comp", "unp_comp", "condition", "vio_comp:condition", "unp_comp:condition")
  ) |> 
  left_join(
    reveal(study2_prim_aim2_mult, .what = std_coef_fitted, matches("full"), .unpack_specs = "wide") |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, parameter = Parameter, starts_with("Std"), ends_with("warnings"), ends_with("messages"))
  ) |> 
  mutate(dv_iv = paste0(dv, ".", iv))

# Store the median effects
study2_prim_aim2_medians_sum <- unique(study2_prim_aim2_effects_sum$dv_iv) |> 
  map(function(x) {
    
    study2_prim_aim2_effects_sum |> 
      filter(dv_iv == x, str_detect(parameter, ":")) |> 
      mutate(parameter = ifelse(str_detect(parameter, ":"), "interaction", "main effect")) |> 
      group_by(dv_iv, parameter) |> 
      summarise(
        med_effect = median(coefficient, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        sum_pvalue = sum(p < .05) / n() * 100
      ) |> 
      mutate(parameter = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(
        names_from = parameter,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(study2_prim_aim2_effects_sum$dv_iv))


# Store the plotting points per decision
study2_prim_aim2_points_sum <- unique(study2_prim_aim2_points$dv_iv) |> 
  map(function(x){
    
    study2_prim_aim2_points |> 
      filter(dv_iv == x) |> 
      left_join(study2_prim_aim2_effects_sum |> filter(str_detect(parameter, ":")) |>  select(decision, dv_iv, p)) |> 
      mutate(
        p.value_chr = ifelse(p <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(study2_prim_aim2_points$dv_iv))


# Store the influence of filter decisions
study2_prim_aim2_variance_sum <- unique(study2_prim_aim2_effects_sum$dv_iv) |> 
  map(function(x) {
    
    data <- study2_prim_aim2_effects_sum |> 
      filter(dv_iv == x) |> 
      filter(str_detect(parameter, ":"))
    
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
  setNames(unique(study2_prim_aim2_effects_sum$dv_iv))



## 5.3 Primary Aim 3: Within-subject Global Local and Flanker Task ----

study2_prim_aim3_effects_sum <- reveal(study2_prim_aim3_mult, .what = model_fitted, .which = model_parameters, .unpack_specs = "wide") |> 
  filter(parameter %in% c("(Intercept)", "vio_comp", "unp_comp", "task", "vio_comp:task", "unp_comp:task")) |> 
  left_join(
    reveal(study2_prim_aim3_mult, .what = std_coef_fitted, matches("full"), .unpack_specs = "wide") |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, parameter = Parameter, starts_with("Std"), ends_with("warnings"), ends_with("messages"))
  ) |> 
  mutate(dv_iv = paste0(dv, ".", iv))

# Store the median effects
study2_prim_aim3_medians_sum <- unique(study2_prim_aim3_effects_sum$dv_iv) |> 
  map(function(x) {
    
    study2_prim_aim3_effects_sum |> 
      filter(dv_iv == x, str_detect(parameter, ":")) |> 
      mutate(parameter = ifelse(str_detect(parameter, ":"), "interaction", "main effect")) |> 
      group_by(dv_iv, parameter) |> 
      summarise(
        med_effect = median(coefficient, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        MAD_effect = mad(Std_Coefficient),
        sum_pvalue = sum(p < .05) / n() * 100
      ) |> 
      mutate(parameter = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(
        names_from = parameter,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(study2_prim_aim3_effects_sum$dv_iv))


# Store the plotting points per decision
study2_prim_aim3_points_sum <- unique(study2_prim_aim3_points$dv_iv) |> 
  map(function(x){
    
    study2_prim_aim3_points |> 
      filter(dv_iv == x) |> 
      left_join(study2_prim_aim3_effects_sum |> filter(str_detect(parameter, ":")) |>  select(decision, dv_iv, p)) |> 
      mutate(
        p.value_chr = ifelse(p <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(study2_prim_aim3_points$dv_iv))


# Store the influence of filter decisions
study2_prim_aim3_variance_sum <- unique(study2_prim_aim3_effects_sum$dv_iv) |> 
  map(function(x) {
    
    data <- study2_prim_aim3_effects_sum |> 
      filter(dv_iv == x) |> 
      filter(str_detect(parameter, ":"))
    
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
  setNames(unique(study2_prim_aim3_effects_sum$dv_iv))


save(study2_prim_aim2_points, study2_prim_aim2_simslopes1, study2_prim_aim2_simslopes2, study2_prim_aim3_points, study2_prim_aim3_simslopes1, study2_prim_aim3_simslopes2,
     study2_prim_aim1_effects_sum, study2_prim_aim1_effects_sum_study2, study2_prim_aim1_medians_sum, study2_prim_aim1_medians_sum_study2, study2_prim_aim1_variance_sum, study2_prim_aim1_variance_sum_study2,
     study2_prim_aim2_effects_sum, study2_prim_aim2_points_sum,  study2_prim_aim2_medians_sum, study2_prim_aim2_variance_sum,
     study2_prim_aim3_effects_sum, study2_prim_aim3_points_sum,  study2_prim_aim3_medians_sum, study2_prim_aim3_variance_sum,
     file = "preregistrations/3_study2/analysis_objects/primary_multiverse_summaries.RData")



