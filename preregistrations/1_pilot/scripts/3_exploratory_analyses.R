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
library(flextable)


load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))
load("preregistrations/1_pilot/analysis_objects/hddm_model_objects.RData")

load('data/1_pilot/2_cleaned_data.RData')
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "DBDA2E-utilities.R"))

source("preregistrations/1_pilot/scripts/custom_functions/ddm_functions.R")

# set up flextable for tables
set_flextable_defaults(
  font.family = "Times", 
  font.size = 10,
  font.color = "black",
  line_spacing = 1,
  padding.bottom = 1, 
  padding.top = 1,
  padding.left = 1,
  padding.right = 1
)


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
pilot_efa_table <- efa_tidy %>%
  rename(
    "Household\nstability/conflict" = `1`,
    "Monitoring/\nneglect" = `2`,
    "Macro unp." = `3`,
    "Disorganization" = `4`,
    "People in\nhousehold" = `5`
  ) |> 
  select(-var) |> 
  flextable() |> 

  bold(i = 1:24, j = 2) |> 
  bold(i = 25:37, j = 3) |>
  bold(i = 38:44, j = 4) |>
  bold(i = 45:52, j = 5) |>
  bold(i = 53:62, j = 6) |> 
  border(i = 67, border.bottom = fp_border_default(), part = "body") |> # Add APA-style bottom border
  border(i = 1, border.bottom = fp_border_default(), part = "header") |>
  border(i = 1, border.top = fp_border_default(style = "none", width = 0), part = "header") |>
  add_header_row(
    values = " ",
    colwidths = 6
  ) |> # Add a new header row on top. We can use this new row to add the title
  flextable::compose(
    i = 1, j = 1,
    as_paragraph(as_b("Table S1. "), "Exploratory Factor Analysis on unpredictability items in the Pilot."),
    part = "header"
  ) |> 
  width(width = c(4,0.75,0.75,0.75,0.75,0.75))
  
save(pilot_efa_table, file = "preregistrations/1_pilot/analysis_objects/supp_section2.Rdata")




# 1. Prepare data ---------------------------------------------------------

## 1.1 Flanker - variables for lm models ----
pilot_expl_flanker_lm_data <- cleaned_data |> 
  select(id, 
         matches("_ssp_"),
         flanker_ks_a, flanker_fixed_hddm_a,
         unp_comp, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) |> 
  filter(!is.infinite(flanker_ssp_interference)) |> 
  mutate(
    flanker_ssp_a            = log(flanker_ssp_a),
    flanker_ssp_interference = ifelse(scale(flanker_ssp_interference) > 3.2, NA, flanker_ssp_interference))

## 1.2 Flanker - variables for lmer models ----
pilot_expl_flanker_lmer_data <- cleaned_data |> 
  select(id, 
         matches("flanker_(con|incon)_(hddm|ks)"),
         flanker_con_rt = rt_flanker_congruent, flanker_incon_rt = rt_flanker_incongruent,
         unp_comp,  
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

pilot_expl_change_lm_data <- cleaned_data |> 
  select(id, 
         change_ks_a, change_ks_t0, change_ks_v,
         change_ml_a, change_ml_t0, change_ml_v,
         change_hddm_a, change_hddm_t, change_hddm_v,
         rt_change,
         unp_comp,  
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(
    scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1),
    rt_change = log(rt_change)) 

## 1.4 Cueing - variables for lm models ----
pilot_expl_cueing_lm_data <- cleaned_data |> 
  select(id, 
         cueing_ml_a, cueing_ks_a, cueing_fixed_hddm_a,
         unp_comp, 
         matches("event_during"), 
         scale_factor,attention_interrupt_sum, att_noise, meta_captcha, fullscreenenter, fullscreenexit) |> 
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 


## 1.5 Cueing - variables for lmer models ----
pilot_expl_cueing_lmer_data <- cleaned_data |> 
  select(id, 
         matches("cueing_(neutral|cued)_(hddm|ks|ml)"),
         cueing_neutral_rt = rt_cueing_neutral, cueing_cued_rt = rt_cueing_cued,
         unp_comp, 
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


# 2. Multiverse Analyses involving Unpredictability -------------------------

## 2.1 Flanker Task - lm() models ----
pilot_expl_flanker_mult_lm <- multitool::run_multiverse(
  .grid = 
    pilot_expl_flanker_lm_data |> 
    multitool::add_variables("iv", unp_comp) |> 
    multitool::add_variables("dv", flanker_ssp_a, flanker_ssp_p, flanker_ssp_t0, flanker_ssp_interference) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

## 2.2 Flanker Task - lmer() models ----
pilot_expl_flanker_mult_lmer <- multitool::run_multiverse(
  .grid = 
    pilot_expl_flanker_lmer_data |> 
    multitool::add_variables("iv", unp_comp) |> 
    multitool::add_variables("dv", rt) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess(postprocess_name = "ss_task", hypothesis_test(terms = c("{iv}[-1,1]", "condition[-1,1]"), re.form = NA, test = NULL)) |> 
    multitool::add_postprocess(postprocess_name = "ss_adversity", hypothesis_test(terms = c("condition[-1,1]", "{iv}[-1,1]"), re.form = NA, test = NULL)) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv}[-1,1]", "condition[-1, 1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)



## 2.3 Change Detection Task - lm() models ----
pilot_expl_change_mult_lm <- multitool::run_multiverse(
  .grid = 
    pilot_expl_change_lm_data |> 
    multitool::add_variables("iv", unp_comp) |> 
    multitool::add_variables("dv", change_hddm_a, change_hddm_t, change_hddm_v, rt_change) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

## 2.4 Cueing Task - lm() models ---- 

pilot_expl_cueing_mult_lm <- multitool::run_multiverse(
  .grid = 
    pilot_expl_cueing_lm_data |> 
    multitool::add_variables("iv", unp_comp) |> 
    multitool::add_variables("dv", cueing_fixed_hddm_a) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lm", lm({dv} ~ {iv})) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

## 2.5 Cueing Task - lmer() models
pilot_expl_cueing_mult_lmer <- multitool::run_multiverse(
  .grid = 
    pilot_expl_cueing_lmer_data |> 
    multitool::add_variables("iv", unp_comp) |> 
    multitool::add_variables("dv", hddm_v, hddm_t, rt) |> 
    multitool::add_filters(
      scale_factor == -1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}) |> as.numeric())') |>
    multitool::add_model("lmer", lmer({dv} ~ {iv} * condition + (1|id))) |>
    multitool::add_postprocess(postprocess_name = "ss_task", hypothesis_test(terms = c("{iv}[-1,1]", "condition[-1,1]"), re.form = NA, test = NULL)) |> 
    multitool::add_postprocess(postprocess_name = "ss_adversity", hypothesis_test(terms = c("condition[-1,1]", "{iv}[-1,1]"), re.form = NA, test = NULL)) |>
    multitool::add_postprocess("points", ggpredict(terms = c("{iv}[-1,1]", "condition[-1, 1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)



# 4. Combine results ------------------------------------------------------

pilot_expl_mult_results <- list(
  flanker = list(
    lm = pilot_expl_flanker_mult_lm,
    lmer = pilot_expl_flanker_mult_lmer
  ),
  change = list(
    lm = pilot_expl_change_mult_lm   
  ),
  cueing = list(
    lm = pilot_expl_cueing_mult_lm,
    lmer = pilot_expl_cueing_mult_lmer
  )
)

save(pilot_expl_mult_results, file = "preregistrations/1_pilot/analysis_objects/exploratory_mult_results.RData")



# 4. Unpack results -------------------------------------------------------

## lmer() results

pilot_expl_lmer_points <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(pilot_expl_mult_results[[x]]$lmer, .what = points_fitted, .which = points_full, .unpack_specs = 'wide') |> 
      rename(level = x) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })

pilot_expl_lmer_effects_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(pilot_expl_mult_results[[x]]$lmer, .what = model_fitted, .which = model_parameters, .unpack_specs = 'wide') |> 
      filter(
        effects == 'fixed',
        parameter != "(Intercept)"
      ) |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv)) |> 
      left_join(
        reveal(pilot_expl_mult_results[[x]]$lmer, .what = std_coef_fitted, .which = std_coef_full, .unpack_specs = 'wide') |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          filter(Parameter != "(Intercept)") |> 
          rename(parameter = Parameter) |> 
          select(decision, parameter, Std_Coefficient, Std_CI_low, Std_CI_high) 
      )
  })

pilot_expl_lmer_simslopes_sum <- c("flanker", "cueing") |> 
  map_df(function(x) {
    
    reveal(pilot_expl_mult_results[[x]]$lmer, .what = ss_task_fitted, .which = ss_task_full, .unpack_specs = 'wide') |> 
      mutate(task = x) |> 
      unite("dv", c(task, dv))
  })



# 5. Create multiverse summaries ------------------------------------------

pilot_expl_lm_effects_sum <- c("flanker", "cueing", "change") |> 
  map_df(function(x) {
    
    reveal(pilot_expl_mult_results[[x]]$lm, .what = model_fitted, .which = model_parameters, .unpack_specs = 'wide') |> 
      mutate(task = x) |> 
      left_join(
        reveal(pilot_expl_mult_results[[x]]$lm, .what = std_coef_fitted, .which = std_coef_full, .unpack_specs = 'wide') |> 
          rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
          filter(Parameter != "(Intercept)") |> 
          rename(parameter = Parameter) |> 
          select(decision, parameter, Std_Coefficient, Std_CI_low, Std_CI_high) 
      )
  })

# Store the median regression effects
pilot_expl_lm_medians_sum <- unique(pilot_expl_lm_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_expl_lm_effects_sum |> 
      filter(dv == x, parameter != "(Intercept)") |> 
      group_by(dv, parameter) |> 
      summarise(
        med_effect     = median(coefficient, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p < .05) / n() * 100
      ) |> 
      mutate(parameter = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(
        names_from = parameter,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(pilot_expl_lm_effects_sum$dv))


# Store the influence of filter decisions
pilot_expl_lm_variance_sum <- unique(pilot_expl_lm_effects_sum$dv) |> 
  map(function(x) {
    
    data <- pilot_expl_lm_effects_sum |> 
      filter(dv == x) |> 
      filter(parameter != "(Intercept)")
    
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
  setNames(unique(pilot_expl_lm_effects_sum$dv))



# Store the median regression effects
pilot_expl_lmer_medians_sum <- unique(pilot_expl_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_expl_lmer_effects_sum |> 
      filter(dv == x, str_detect(parameter, ":")) |>
      mutate(parameter = ifelse(str_detect(parameter, ":"), "interaction", "main effect")) |> 
      group_by(dv, parameter) |> 
      summarise(
        med_effect = median(coefficient, na.rm = T),
        med_effect_std = median(Std_Coefficient, na.rm = T),
        sum_pvalue = sum(p < .05) / n() * 100
      ) |> 
      mutate(parameter = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(
        names_from = parameter,
        values_from = c(med_effect, sum_pvalue)
      )
  }) |> 
  setNames(unique(pilot_expl_lmer_effects_sum$dv))


# Store the plotting points per decision
pilot_expl_lmer_points_sum <- unique(pilot_expl_lmer_points$dv) |> 
  map(function(x){
    
    pilot_expl_lmer_points |> 
      filter(dv == x) |> 
      left_join(pilot_expl_lmer_effects_sum |> filter(str_detect(parameter, ":")) |>  select(decision, dv, p)) |> 
      mutate(
        p.value_chr = ifelse(p <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(pilot_expl_lmer_points$dv))




# Store the influence of filter decisions
pilot_expl_lmer_decisions_sum <- unique(pilot_expl_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_expl_lmer_effects_sum |> 
      filter(dv == x) |> 
      filter(str_detect(parameter, ":")) |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(dv, filters, setting) |> 
      summarise(sum_pvalue = sum(p < .05) / n() * 100) |> 
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
  setNames(unique(pilot_expl_lmer_effects_sum$dv))

pilot_expl_lmer_variance_sum <- unique(pilot_expl_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    data <- pilot_expl_lmer_effects_sum|> 
      filter(dv == x) |> 
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
  setNames(unique(pilot_expl_lmer_effects_sum$dv))



save(pilot_expl_lm_effects_sum, pilot_expl_lm_medians_sum, pilot_expl_lm_variance_sum, pilot_expl_lmer_simslopes_sum, pilot_expl_lmer_effects_sum, pilot_expl_lmer_medians_sum, pilot_expl_lmer_points_sum, pilot_expl_lmer_decisions_sum, pilot_expl_lmer_variance_sum, file = "preregistrations/1_pilot/analysis_objects/exploratory_mult_summaries.RData")


