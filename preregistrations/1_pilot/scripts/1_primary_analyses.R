
# Libraries ------------------------------------------------------------

library(tidyverse)
library(multitool) #remotes::install_github('https://github.com/ethan-young/multitool', force = T)
library(lmerTest)
library(ggeffects)
library(interactions)
library(ggsci)
library(parameters)

load("data/1_pilot/2_cleaned_data.Rdata")

source("preregistrations/1_pilot/scripts/custom_functions/unpack_multiverse.R")


# Flanker - lm() models
# - all SSP parameters
# - a for ml, ks and HDDM

# Flanker - lmer() models
# - v and t0 for ml, ks and HDDM
# - raw RTs


# Change - lm() models
# - v, t0 and a for ml, ks and HDDM
# - raw RTs

# Cueing - lm() models
# - a for ml, ks, and HDDM

# Cueing - lmer() models
# - v and t0 for ml, ks and HDDM
# - raw RTs


# 1. Prepare data ---------------------------------------------------------

## 1.1 Flanker - variables for lm models ----
prim_flanker_lm_data <- cleaned_data |> 
  select(id, 
         matches("_ssp_"),
         flanker_ks_a, flanker_fixed_hddm_a,
         violence_composite, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  filter(!is.infinite(flanker_ssp_interference)) 

## 1.2 Flanker - variables for lmer models ----
prim_flanker_lmer_data <- cleaned_data |> 
  select(id, 
         matches("flanker_(con|incon)_(hddm|ks)"),
         flanker_con_rt = rt_flanker_congruent, flanker_incon_rt = rt_flanker_incongruent,
         violence_composite,  
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  pivot_longer(
    cols = starts_with('flanker'),
    names_to = c("task", "condition", ".value"),
    names_pattern = "([a-z]*)_([a-z]*)_(.*)"
  ) |> 
  mutate(condition = ifelse(condition == "con", -1, 1))

## 1.3 Change - variables for lm models ----

prim_change_lm_data <- cleaned_data |> 
  select(id, 
         change_ks_a, change_ks_t0, change_ks_v,
         change_ml_a, change_ml_t0, change_ml_v,
         change_hddm_a, change_hddm_t, change_hddm_v,
         rt_change,
         violence_composite,  
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 

## 1.4 Cueing - variables for lm models ----
prim_cueing_lm_data <- cleaned_data |> 
  select(id, 
         cueing_ml_a, cueing_ks_a, cueing_fixed_hddm_a,
         violence_composite, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 


## 1.5 Cueing - variables for lmer models ----
prim_cueing_lmer_data <- cleaned_data |> 
  select(id, 
         matches("cueing_(neutral|cued)_(hddm|ks|ml)"),
         cueing_neutral_rt = rt_cueing_neutral, cueing_cued_rt = rt_cueing_cued,
         violence_composite, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  pivot_longer(
    cols = starts_with('cueing'),
    names_to = c("task", "condition", ".value"),
    names_pattern = "([a-z]*)_([a-z]*)_(.*)"
  ) |> 
  mutate(condition = ifelse(condition == 'neutral', -1, 1))


# 2. Multiverse Analyses involving violence exposure -------------------------

## 2.1 Flanker Task - lm() models ----
prim_flanker_mult_lm <- multitool::run_multiverse(
  .grid = 
    prim_flanker_lm_data |> 
    multitool::add_variables("iv", violence_composite) |> 
    multitool::add_variables("dv", flanker_ssp_a, flanker_ssp_p, flanker_ssp_t0, flanker_ssp_sda, flanker_ssp_rd, flanker_ssp_interference,
                             flanker_ks_a, flanker_fixed_hddm_a) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
  multitool::expand_decisions()
)

## 2.2 Flanker Task - lmer() models ----
prim_flanker_mult_lmer <- multitool::run_multiverse(
  .grid = 
    prim_flanker_lmer_data |> 
    multitool::add_variables("iv", violence_composite) |> 
    multitool::add_variables("dv", ks_v, ks_t0, hddm_v, hddm_t, rt) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess(postprocess_name = "ss_task", code = sim_slopes(pred = {iv}, modx = condition, modx.values = c(-1,1))) |> 
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [-1,1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)



## 2.3 Change Detection Task - lm() models ----
prim_change_mult_lm <- multitool::run_multiverse(
  .grid = 
    prim_change_lm_data |> 
    multitool::add_variables("iv", violence_composite) |> 
    multitool::add_variables("dv", change_ks_a, change_ks_t0, change_ks_v,
                                   change_ml_a, change_ml_t0, change_ml_v,
                                   change_hddm_a, change_hddm_t, change_hddm_v,
                                   rt_change) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

## 2.4 Cueing Task - lm() models ---- 

prim_cueing_mult_lm <- multitool::run_multiverse(
  .grid = 
    prim_cueing_lm_data |> 
    multitool::add_variables("iv", violence_composite) |> 
    multitool::add_variables("dv", cueing_ml_a, cueing_ks_a, cueing_fixed_hddm_a) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

## 2.5 Cueing Task - lmer() models
prim_cueing_mult_lmer <- multitool::run_multiverse(
  .grid = 
    prim_cueing_lmer_data |> 
    multitool::add_variables("iv", violence_composite) |> 
    multitool::add_variables("dv", ks_v, ks_t0, ml_v, ml_t0, hddm_v, hddm_t, rt) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [-1,1]"))) |> 
    multitool::add_postprocess(postprocess_name = "ss_task", code = sim_slopes(pred = {iv}, modx = condition, modx.values = c(-1,1))) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)



# 4. Combine results ------------------------------------------------------

primary_mult_results <- list(
  flanker = list(
    lm = prim_flanker_mult_lm,
    lmer = prim_flanker_mult_lmer
  ),
  change = list(
    lm = prim_change_mult_lm   
  ),
  cueing = list(
    lm = prim_cueing_mult_lm,
    lmer = prim_cueing_mult_lmer
  )
)

save(primary_mult_results, file = "preregistrations/1_pilot/analysis_objects/primary_mult_results.RData")



# 4. Unpack results -------------------------------------------------------

## lm() results



## lmer() results

primary_lmer_points <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(primary_mult_results[[x]]$lmer, .what = ggpredict_fitted, .which = ggpredict_full, .unpack_specs = TRUE) |> 
      rename(level = x) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })

prim_lmer_effects_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(primary_mult_results[[x]]$lmer, .what = lmer_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
      filter(
        effect == 'fixed',
        term != "(Intercept)"
      ) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv)) |> 
      left_join(
        reveal(primary_mult_results[[x]]$lmer, .what = standardize_parameters_fitted, .which = matches('full'), .unpack_specs = TRUE) |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          select(decision, term = Parameter, starts_with("Std")) 
      )
  })

prim_lmer_simslopes_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(primary_mult_results[[x]]$lmer, .what = sim_slopes_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })



# 5. Create multiverse summaries ------------------------------------------

prim_lm_effects_sum <- c("flanker", "cueing", "change") |> 
  map_df(function(x) {
    
    reveal(primary_mult_results[[x]]$lm, .what = lm_fitted, matches("tidy"), .unpack_specs = TRUE) |> 
      mutate(task = x) |> 
      left_join(
        reveal(primary_mult_results[[x]]$lm, .what = standardize_parameters_fitted, matches("full"), .unpack_specs = TRUE) |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          select(decision, term = Parameter, starts_with("Std"))
      )
  })

# Store the median regression effects
prim_lm_medians_sum <- unique(primary_lm_effects$dv) |> 
  map(function(x) {
    
    primary_lm_effects |> 
      filter(dv == x, term != "(Intercept)") |> 
      group_by(dv, term) |> 
      summarise(
        med_effect     = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(primary_lm_effects$dv))


# Store the influence of filter decisions
prim_lm_decisions_sum <- unique(primary_lm_effects$dv) |> 
  map(function(x) {
    
    primary_lm_effects |> 
      filter(dv == x) |> 
      filter(term != "(Intercept)") |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(dv, filters, setting) |> 
      summarise(sum_pvalue = sum(p.value < .05) / n() * 100) |> 
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
  setNames(unique(primary_lm_effects$dv))

prim_lm_variance_sum <- unique(primary_lm_effects$dv) |> 
  map(function(x) {
    
    data <- primary_lm_effects|> 
      filter(dv == x) |> 
      filter(term != "(Intercept)")
    
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
  setNames(unique(primary_lm_effects$dv))



# Store the median regression effects
prim_lmer_medians_sum <- unique(prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    prim_lmer_effects_sum |> 
      filter(dv == x, str_detect(term, ":")) |>
      mutate(term = ifelse(str_detect(term, ":"), "interaction", "main effect")) |> 
      group_by(dv, term) |> 
      summarise(
        med_effect = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(prim_lmer_effects_sum$dv))


# Store the plotting points per decision
prim_lmer_points_sum <- unique(primary_lmer_points$dv) |> 
  map(function(x){
    
    primary_lmer_points |> 
      filter(dv == x) |> 
      left_join(prim_lmer_effects_sum |> filter(str_detect(term, ":")) |>  select(decision, dv, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(primary_lmer_points$dv))




# Store the influence of filter decisions
prim_lmer_decisions_sum <- unique(prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    prim_lmer_effects_sum |> 
      filter(dv == x) |> 
      filter(str_detect(term, ":")) |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(dv, filters, setting) |> 
      summarise(sum_pvalue = sum(p.value < .05) / n() * 100) |> 
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
  setNames(unique(prim_lmer_effects_sum$dv))

prim_lmer_variance_sum <- unique(prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    data <- prim_lmer_effects_sum|> 
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
  setNames(unique(prim_lmer_effects_sum$dv))

  

save(prim_lm_effects_sum, prim_lm_medians_sum, prim_lm_decisions_sum, prim_lm_variance_sum, prim_lmer_simslopes_sum, prim_lmer_effects_sum, prim_lmer_medians_sum, prim_lmer_points_sum, prim_lmer_decisions_sum, prim_lmer_variance_sum, file = "preregistrations/1_pilot/analysis_objects/primary_mult_summaries.RData")


