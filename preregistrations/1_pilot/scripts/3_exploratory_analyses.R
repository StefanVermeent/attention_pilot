library(tidyverse)
library(magrittr)
library(broom)
library(here)
library(stats)
library(GPArotation)
library(nFactors)
library(multitool) #remotes::install_github('https://github.com/ethan-young/multitool', force = T)
library(lmerTest)
library(ggeffects)
library(interactions)
library(parameters)
library(specr)


load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))


# 1. EFA: Unpredictability ------------------------------------------------



# We conducted an EFA including all items measuring unpredictability: 1) The QUIC items, 2) the perceived unpredictability items, 
# 3) the CHAOS items, 4) the items measuring environmental change, 5) number of partners of the father and mother, 
# 6) Number of residential changes, 7) household size. 

# The measures of residential changes and number of different partners of both parents were heavily positively skewed.


efa_data <- cleaned_data %>%
  select(matches("(quic\\d\\d)|(unp\\d\\d)|(chaos\\d\\d)|(change_env\\d\\d)"), 
         unp_partners_father_binned, unp_partners_mother_binned, unp_moving_binned) %>%
  drop_na() %>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .))


# Determine optimal number of factors
ev <- eigen(cor(efa_data)) # get eigenvalues
ap <- parallel(subject=nrow(efa_data),var=ncol(efa_data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

efa_model <- factanal(efa_data, factors = 5, rotation = "oblimin")

efa_model

efa_tidy <- tidy(efa_model) %>% 
    mutate(across(starts_with("fl"), ~ifelse(. < .32, NA, .))) %>% 
  left_join(codebook %>% rename(variable = Variable)) %>% 
  arrange(fl1, fl2, fl3, fl4, fl5) %>% 
  mutate(
    Label = case_when(
      variable == "unp_moving_binned" ~ "Residential changes", 
      variable == "unp_partners_father_binned" ~ "Romantic partners - father", 
      variable == "unp_partners_mother_binned" ~ "Romantic partners - mother", 
      variable == "unp_household_size" ~ "Household size",
      variable == "unp03" ~ "My parents had a difficult divorce or separation during this time.",
      TRUE ~ Label
    ),
    var = variable,
    variable = case_when(
      str_detect(variable, "quic") ~ "QUIC - ",
      str_detect(variable, "unp\\d\\d") ~ "Perceived - ",
      str_detect(variable, "chaos") ~ "CHAOS - ",
      str_detect(variable, "change_env") ~ "Changes - ",
      TRUE ~ ""
    )
  ) %>% 
  unite(col = "Item", c(variable, Label), sep = "") %>%
  select(var, Item, fl1, fl2, fl3, fl4, fl5) %>% 
  mutate(across(starts_with("fl"), ~round(., 2))) %>%
  rename(
    `1` = fl1,
    `2` = fl2,
    `3` = fl3,
    `4` = fl4,
    `5` = fl5
  ) 

# Extract new variables from EFA model
efa_vars <- efa_tidy %>%
  mutate(label = case_when(
    !is.na(`1`)                                                     ~ "efa_daily_unp",
    !is.na(`2`) & is.na(`1`)                                        ~ "efa_routine",
    !is.na(`3`) & is.na(`1`) & is.na(`2`)                           ~ "efa_spatial_unp",
    !is.na(`4`) & is.na(`1`) & is.na(`2`) & is.na(`3`)              ~ "efa_chaos_clutter",
    !is.na(`5`) & is.na(`1`) & is.na(`2`) & is.na(`3`) & is.na(`4`) ~ "efa_social_unp",
    TRUE ~ "other"
  )) %>%
  filter(label != "other") %>%
  select(var, label)

# Add variables to cleaned dataset

cleaned_data %<>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .)) %>%
  mutate(
    efa_daily_unp     = across(efa_vars %>% filter(label == "efa_daily_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_routine       = across(efa_vars %>% filter(label == "efa_routine") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_spatial_unp   = across(efa_vars %>% filter(label == "efa_spatial_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_chaos_clutter = across(efa_vars %>% filter(label == "efa_chaos_clutter") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_social_unp    = across(efa_vars %>% filter(label == "efa_social_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
  )




# 2. Multiverse: Unpredictability -----------------------------------------

# 2.1 Prepare data ----

## 2.1.1 Flanker - variables for lm models ----
expl_flanker_lm_data <- cleaned_data |> 
  select(id, 
         matches("_ssp_"),
         flanker_ks_a, flanker_fixed_hddm_a,
         unpredictability_composite, 
         unp_quic_punp,
         unpredictability_obj,
         starts_with("efa"),
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  filter(!is.infinite(flanker_ssp_interference)) 

## 2.1.2 Flanker - variables for lmer models ----
expl_flanker_lmer_data <- cleaned_data |> 
  select(id, 
         matches("flanker_(con|incon)_(hddm|ks)"),
         flanker_con_rt = rt_flanker_congruent, flanker_incon_rt = rt_flanker_incongruent,
         unpredictability_composite,
         unp_quic_punp,
         unpredictability_obj,
         starts_with("efa"),
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  pivot_longer(
    cols = starts_with('flanker'),
    names_to = c("task", "condition", ".value"),
    names_pattern = "([a-z]*)_([a-z]*)_(.*)"
  ) |> 
  mutate(condition = ifelse(condition == "con", -1, 1))

## 2.1.3 Change - variables for lm models ----

expl_change_lm_data <- cleaned_data |> 
  select(id, 
         change_ks_a, change_ks_t0, change_ks_v,
         change_ml_a, change_ml_t0, change_ml_v,
         change_hddm_a, change_hddm_t, change_hddm_v,
         rt_change,
         unpredictability_composite, 
         unp_quic_punp,
         unpredictability_obj,
         starts_with("efa"),
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 

## 2.1.4 Cueing - variables for lm models ----
expl_cueing_lm_data <- cleaned_data |> 
  select(id, 
         cueing_ks_a, cueing_ml_a, cueing_fixed_hddm_a,
         unpredictability_composite, 
         unp_quic_punp,
         unpredictability_obj,
         starts_with("efa"),
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 


## 2.1.5 Cueing - variables for lmer models ----
expl_cueing_lmer_data <- cleaned_data |> 
  select(id, 
         matches("cueing_(neutral|cued)_(hddm|ks|ml)"),
         cueing_neutral_rt = rt_cueing_neutral, cueing_cued_rt = rt_cueing_cued,
         unpredictability_composite, 
         starts_with("efa"),
         unp_quic_punp,
         unpredictability_obj,
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  pivot_longer(
    cols = starts_with('cueing'),
    names_to = c("task", "condition", ".value"),
    names_pattern = "([a-z]*)_([a-z]*)_(.*)"
  ) |> 
  mutate(condition = ifelse(condition == 'neutral', -1, 1))


# 2.2 Multiverse Analyses involving unpredictability ----

## 2.2.1 Flanker Task - lm() models ----
expl_flanker_mult_lm <- multitool::run_multiverse(
  .grid = 
    expl_flanker_lm_data |> 
    multitool::add_variables("iv", unpredictability_composite, unp_quic_punp, unpredictability_obj) |> 
    multitool::add_variables("dv", flanker_ssp_a, flanker_ssp_p, flanker_ssp_t0, flanker_ssp_sda, flanker_ssp_rd, flanker_ssp_interference) |> 
    multitool::add_filters(
      scale_factor == 1,
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

## 2.2.2 Flanker Task - lmer() models ----
expl_flanker_mult_lmer <- multitool::run_multiverse(
  .grid = 
    expl_flanker_lmer_data |> 
    multitool::add_variables("iv", unpredictability_composite, unp_quic_punp, unpredictability_obj) |> 
    multitool::add_variables("dv", ks_v, ks_t0, hddm_v, hddm_t, rt) |> 
    multitool::add_filters(
      scale_factor == 1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [-1,1]"))) |> 
    multitool::add_postprocess("ss_task", sim_slopes(pred = {iv}, modx = condition, modx.values = c(-1,1))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |>
    multitool::expand_decisions()
)



## 2.2.3 Change Detection Task - lm() models ----
expl_change_mult_lm <- multitool::run_multiverse(
  .grid = 
    expl_change_lm_data |> 
    multitool::add_variables("iv", unpredictability_composite, unp_quic_punp, unpredictability_obj) |> 
    multitool::add_variables("dv", change_ks_a, change_ks_t0, change_ks_v,
                             change_ml_a, change_ml_t0, change_ml_v,
                             change_hddm_a, change_hddm_t, change_hddm_v,
                             rt_change) |> 
    multitool::add_filters(
      scale_factor == 1,
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

## 2.2.4 Cueing Task - lm() models ---- 

expl_cueing_mult_lm <- multitool::run_multiverse(
  .grid = 
    expl_cueing_lm_data |> 
    multitool::add_variables("iv", unpredictability_composite, unp_quic_punp, unpredictability_obj) |> 
    multitool::add_variables("dv", cueing_ml_a, cueing_ks_a, cueing_fixed_hddm_a) |> 
    multitool::add_filters(
      scale_factor == 1,
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

## 2.2.5 Cueing Task - lmer() models
expl_cueing_mult_lmer <- multitool::run_multiverse(
  .grid = 
    expl_cueing_lmer_data |> 
    multitool::add_variables("iv", unpredictability_composite, unp_quic_punp, unpredictability_obj) |> 
    multitool::add_variables("dv", ks_v, ks_t0, hddm_v, hddm_t, rt) |> 
    multitool::add_filters(
      scale_factor == 1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [-1,1]"))) |> 
    multitool::add_postprocess("ss_task", sim_slopes(pred = {iv}, modx = condition, modx.values = c(-1,1))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |>
    multitool::expand_decisions()
)



# 3. Combine results ------------------------------------------------------

exploratory_mult_results <- list(
  flanker = list(
    lm = expl_flanker_mult_lm,
    lmer = expl_flanker_mult_lmer
  ),
  change = list(
    lm = expl_change_mult_lm
  ),
  cueing = list(
    lm = expl_cueing_mult_lm,
    lmer = expl_cueing_mult_lmer
  )
)

save(exploratory_mult_results, file = "preregistrations/1_pilot/analysis_objects/exploratory_mult_results.RData")


# 4. Unpack results -------------------------------------------------------



## lmer() results

exploratory_lmer_points <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(exploratory_mult_results[[x]]$lmer, .what = ggpredict_fitted, .which = ggpredict_full, .unpack_specs = TRUE) |> 
      rename(level = x) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })



exploratory_lmer_simslopes <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(exploratory_mult_results[[x]]$lmer, .what = sim_slopes_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv, iv))
  })



# 5. Create multiverse summaries ------------------------------------------
## lm() results

expl_lm_effects_sum <- c("flanker", "cueing", "change") |> 
  map_df(function(x) {
    
    reveal(exploratory_mult_results[[x]]$lm, .what = lm_fitted, matches("tidy"), .unpack_specs = TRUE) |> 
      mutate(task = x) |> 
      left_join(
        reveal(exploratory_mult_results[[x]]$lm, .what = standardize_parameters_fitted, matches("full"), .unpack_specs = TRUE) |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          select(decision, term = Parameter, starts_with("Std")) 
      ) |> 
          unite("vars", c(task, dv, iv), remove = FALSE)
  })

# Store the median regression effects
expl_lm_medians_sum <- unique(expl_lm_effects_sum$vars) |> 
  map(function(x) {
    
    iv = expl_lm_effects_sum |> 
      filter(vars == x, term != "(Intercept)") |> pull(iv) |> unique()
    
    expl_lm_effects_sum |> 
      filter(vars == x, term != "(Intercept)") |> 
      group_by(vars, iv, term) |> 
      summarise(
        med_effect     = median(estimate, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p.value < .05) / n() * 100
      ) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(
        names_from = term,
        values_from = c(med_effect, sum_pvalue)
      ) |> 
      rename(!!"med_effect_main" := paste0("med_effect_", {{iv}}))

  }) |> 
  setNames(unique(expl_lm_effects_sum$vars))


# Store the influence of filter decisions
expl_lm_decisions_sum <- unique(expl_lm_effects_sum$vars) |> 
  map(function(x) {
    
    expl_lm_effects_sum |> 
      filter(vars == x) |> 
      filter(term != "(Intercept)") |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(vars, filters, setting) |> 
      summarise(
        sum_pvalue = sum(p.value < .05) / n() * 100) |> 
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
  setNames(unique(expl_lm_effects_sum$vars))


expl_lm_variance_sum <- unique(expl_lm_effects_sum$vars) |> 
  map(function(x) {
    
    data <- expl_lm_effects_sum|> 
      filter(vars == x) |> 
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
  setNames(unique(expl_lm_effects_sum$dv))


# Store the median regression effects
expl_lmer_effects_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(exploratory_mult_results[[x]]$lmer, .what = lmer_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
      filter(
        effect == 'fixed',
        term != "(Intercept)"
      ) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv)) |> 
      left_join(
        reveal(exploratory_mult_results[[x]]$lmer, .what = standardize_parameters_fitted, .which = matches('full'), .unpack_specs = TRUE) |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          select(decision, term = Parameter, starts_with("Std")) 
      )
  })


expl_lmer_medians_sum <- unique(expl_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    expl_lmer_effects_sum |> 
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
  setNames(unique(expl_lmer_effects_sum$dv))


# Store the plotting points per decision
expl_lmer_points_sum <- unique(exploratory_lmer_points$dv) |> 
  map(function(x){
    
    exploratory_lmer_points |> 
      filter(dv == x) |> 
      left_join(expl_lmer_effects_sum |> filter(str_detect(term, ":")) |>  select(decision, dv, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(exploratory_lmer_points$dv))




# Store the influence of filter decisions
expl_lmer_decisions_sum <- unique(expl_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    expl_lmer_effects_sum |> 
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
  setNames(unique(expl_lmer_effects_sum$dv))

expl_lmer_variance_sum <- unique(expl_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    data <- expl_lmer_effects_sum|> 
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
  setNames(unique(expl_lmer_effects_sum$dv))


save(expl_lm_effects_sum, expl_lm_medians_sum, expl_lm_decisions_sum, expl_lm_variance_sum, expl_lmer_medians_sum, expl_lmer_points_sum, expl_lmer_decisions_sum, expl_lmer_variance_sum, file = "preregistrations/1_pilot/analysis_objects/exploratory_mult_summaries.RData")
