
# Libraries ------------------------------------------------------------

library(tidyverse)
library(multitool) #remotes::install_github('https://github.com/ethan-young/multitool', force = T)
library(lmerTest)
library(ggeffects)
library(interactions)
library(ggsci)
library(parameters)
library(specr)

load("data/1_pilot/2_cleaned_data.Rdata")





# 1. Prepare data ---------------------------------------------------------

## 1.1 Flanker - variables for lm models ----
pilot_prim_flanker_lm_data <- cleaned_data |> 
  select(id, 
         matches("_ssp_"),
         flanker_ks_a, flanker_fixed_hddm_a,
         vio_comp, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  filter(!is.infinite(flanker_ssp_interference)) |> 
  mutate(
    flanker_ssp_a            = log(flanker_ssp_a),
    flanker_ssp_interference = ifelse(scale(flanker_ssp_interference) > 3.2, NA, flanker_ssp_interference))

## 1.2 Flanker - variables for lmer models ----
pilot_prim_flanker_lmer_data <- cleaned_data |> 
  select(id, 
         matches("flanker_(con|incon)_(hddm|ks)"),
         flanker_con_rt = rt_flanker_congruent, flanker_incon_rt = rt_flanker_incongruent,
         vio_comp,  
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(
    scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1),
    flanker_con_rt = log(flanker_con_rt),
    flanker_incon_rt = log(flanker_incon_rt)) |> 
  pivot_longer(
    cols = starts_with('flanker'),
    names_to = c("task", "condition", ".value"),
    names_pattern = "([a-z]*)_([a-z]*)_(.*)"
  ) |> 
  mutate(condition = ifelse(condition == "con", -1, 1))

## 1.3 Change - variables for lm models ----

pilot_prim_change_lm_data <- cleaned_data |> 
  select(id, 
         change_ks_a, change_ks_t0, change_ks_v,
         change_ml_a, change_ml_t0, change_ml_v,
         change_hddm_a, change_hddm_t, change_hddm_v,
         rt_change,
         vio_comp,  
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(
    scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1),
    rt_change = log(rt_change)) 

## 1.4 Cueing - variables for lm models ----
pilot_prim_cueing_lm_data <- cleaned_data |> 
  select(id, 
         cueing_ml_a, cueing_ks_a, cueing_fixed_hddm_a,
         vio_comp, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 


## 1.5 Cueing - variables for lmer models ----
pilot_prim_cueing_lmer_data <- cleaned_data |> 
  select(id, 
         matches("cueing_(neutral|cued)_(hddm|ks|ml)"),
         cueing_neutral_rt = rt_cueing_neutral, cueing_cued_rt = rt_cueing_cued,
         vio_comp, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(
    scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1),
    cueing_neutral_rt = log(cueing_neutral_rt),
    cueing_cued_rt = log(cueing_cued_rt)) |> 
  pivot_longer(
    cols = starts_with('cueing'),
    names_to = c("task", "condition", ".value"),
    names_pattern = "([a-z]*)_([a-z]*)_(.*)"
  ) |> 
  mutate(condition = ifelse(condition == 'neutral', -1, 1))


# 2. Multiverse Analyses involving violence exposure -------------------------

## 2.1 Flanker Task - lm() models ----
pilot_prim_flanker_mult_lm <- multitool::run_multiverse(
  .grid = 
    pilot_prim_flanker_lm_data |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", flanker_ssp_a, flanker_ssp_p, flanker_ssp_t0, flanker_ssp_interference) |> 
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
pilot_prim_flanker_mult_lmer <- multitool::run_multiverse(
  .grid = 
    pilot_prim_flanker_lmer_data |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", rt) |> 
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
pilot_prim_change_mult_lm <- multitool::run_multiverse(
  .grid = 
    pilot_prim_change_lm_data |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", change_hddm_a, change_hddm_t, change_hddm_v, rt_change) |> 
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

pilot_prim_cueing_mult_lm <- multitool::run_multiverse(
  .grid = 
    pilot_prim_cueing_lm_data |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", cueing_fixed_hddm_a) |> 
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
pilot_prim_cueing_mult_lmer <- multitool::run_multiverse(
  .grid = 
    pilot_prim_cueing_lmer_data |> 
    multitool::add_variables("iv", vio_comp) |> 
    multitool::add_variables("dv", hddm_v, hddm_t, rt) |> 
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

pilot_prim_mult_results <- list(
  flanker = list(
    lm = pilot_prim_flanker_mult_lm,
    lmer = pilot_prim_flanker_mult_lmer
  ),
  change = list(
    lm = pilot_prim_change_mult_lm   
  ),
  cueing = list(
    lm = pilot_prim_cueing_mult_lm,
    lmer = pilot_prim_cueing_mult_lmer
  )
)

save(pilot_prim_mult_results, file = "preregistrations/1_pilot/analysis_objects/primary_mult_results.RData")



# 4. Unpack results -------------------------------------------------------

## lmer() results

pilot_prim_lmer_points <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(pilot_prim_mult_results[[x]]$lmer, .what = points_fitted, .which = ggpredict_full, .unpack_specs = TRUE) |> 
      rename(level = x) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })

pilot_prim_lmer_effects_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(pilot_prim_mult_results[[x]]$lmer, .what = model_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
      filter(
        effect == 'fixed',
        term != "(Intercept)"
      ) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv)) |> 
      left_join(
        reveal(pilot_prim_mult_results[[x]]$lmer, .what = std_coef_fitted, .which = standardize_parameters_full, .unpack_specs = TRUE) |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          select(decision, term = Parameter, starts_with("Std")) 
      )
  })

pilot_prim_lmer_simslopes_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(pilot_prim_mult_results[[x]]$lmer, .what = ss_task_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })



# 5. Create multiverse summaries ------------------------------------------

pilot_prim_lm_effects_sum <- c("flanker", "cueing", "change") |> 
  map_df(function(x) {
    
    reveal(pilot_prim_mult_results[[x]]$lm, .what = model_fitted, matches("tidy"), .unpack_specs = TRUE) |> 
      mutate(task = x) |> 
      left_join(
        reveal(pilot_prim_mult_results[[x]]$lm, .what = std_coef_fitted, matches("full"), .unpack_specs = TRUE) |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          select(decision, term = Parameter, starts_with("Std"))
      )
  })

# Store the median regression effects
pilot_prim_lm_medians_sum <- unique(pilot_prim_lm_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_prim_lm_effects_sum |> 
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
  setNames(unique(pilot_prim_lm_effects_sum$dv))


# Store the influence of filter decisions
pilot_prim_lm_variance_sum <- unique(pilot_prim_lm_effects_sum$dv) |> 
  map(function(x) {
    
    data <- pilot_prim_lm_effects_sum |> 
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
  setNames(unique(pilot_prim_lm_effects_sum$dv))



# Store the median regression effects
pilot_prim_lmer_medians_sum <- unique(pilot_prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_prim_lmer_effects_sum |> 
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
  setNames(unique(pilot_prim_lmer_effects_sum$dv))


# Store the plotting points per decision
pilot_prim_lmer_points_sum <- unique(pilot_prim_lmer_points$dv) |> 
  map(function(x){
    
    pilot_prim_lmer_points |> 
      filter(dv == x) |> 
      left_join(pilot_prim_lmer_effects_sum |> filter(str_detect(term, ":")) |>  select(decision, dv, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(pilot_prim_lmer_points$dv))




# Store the influence of filter decisions
pilot_prim_lmer_decisions_sum <- unique(pilot_prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_prim_lmer_effects_sum |> 
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
  setNames(unique(pilot_prim_lmer_effects_sum$dv))

pilot_prim_lmer_variance_sum <- unique(pilot_prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    data <- pilot_prim_lmer_effects_sum|> 
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
  setNames(unique(pilot_prim_lmer_effects_sum$dv))

  

save(pilot_prim_lm_effects_sum, pilot_prim_lm_medians_sum, pilot_prim_lm_variance_sum, pilot_prim_lmer_simslopes_sum, pilot_prim_lmer_effects_sum, pilot_prim_lmer_medians_sum, pilot_prim_lmer_points_sum, pilot_prim_lmer_decisions_sum, pilot_prim_lmer_variance_sum, file = "preregistrations/1_pilot/analysis_objects/primary_mult_summaries.RData")


