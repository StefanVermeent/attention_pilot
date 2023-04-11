library(tidyverse)
library(magrittr)
library(broom)
library(here)
library(lmerTest)
library(stats)
library(GPArotation)
library(nFactors)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(ggeffects)
library(interactions)
library(parameters)



# 1. EFA analysis of unpredictability items ----------------------------------

# Load pilot data
load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))

cleaned_data_pilot <- cleaned_data %>%
  rename(
    unp_male_fig_rom_binned = unp_partners_mother_binned,
    unp_female_fig_rom_binned = unp_partners_father_binned 
  )

source(here("preregistrations", "1_pilot", "scripts", "3_exploratory_analyses", "1_efa.R"))

efa_tidy_pilot <- efa_tidy %>%
  rename(
    pilot_f1 = `1`,
    pilot_f2 = `2`,
    pilot_f3 = `3`,
    pilot_f4 = `4`,
    pilot_f5 = `5`
  ) %>%
  mutate(
    var = ifelse(var == "unp_partners_mother_binned", "unp_male_fig_rom_binned", var),
    var = ifelse(var == "unp_partners_father_binned", "unp_female_fig_rom_binned", var)
  ) %>%
  select(-Item)


# We conducted an EFA including all items measuring unpredictability: 1) The QUIC items, 2) the perceived unpredictability items, 
# 3) the CHAOS items, 4) the items measuring environmental change, 5) number of partners of the father and mother, 
# 6) Number of residential changes, 7) household size. 

# The measures of residential changes and number of different partners of both parents were heavily positively skewed.
cleaned_data <- read_csv("data/2_study1/2_cleaned_data.csv")
load(here("data", "2_study1", "0_self_report_raw.Rdata"))

cleaned_data_study1 <- cleaned_data

efa_data <- cleaned_data %>%
  select(matches("(quic\\d\\d)|(unp\\d\\d)|(chaos\\d\\d)|(change_env\\d\\d)"), 
         unp_male_fig_rom_binned, unp_female_fig_rom_binned, unp_moving_binned) %>%
  drop_na() 


# Determine optimal number of factors
ev <- eigen(cor(efa_data)) # get eigenvalues
ap <- parallel(subject=nrow(efa_data),var=ncol(efa_data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

efa_model <- factanal(efa_data, factors = 5, rotation = "oblimin")

efa_model

efa_tidy_study1 <- tidy(efa_model) %>% 
  #  mutate(across(starts_with("fl"), ~ifelse(. < .32, NA, .))) %>% 
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
    study1_f1 = fl1,
    study1_f2 = fl2,
    study1_f3 = fl4,
    study1_f4 = fl3,
    study1_f5 = fl5
  ) 


# Plot factor loadings
efa_data_combined <- full_join(
  efa_tidy_pilot %>%
    pivot_longer(starts_with("pilot"), names_to = "Factor", values_to = "Loading_pilot") %>%
    mutate(Factor = str_replace_all(Factor, "pilot_", "")),
  
  efa_tidy_study1 %>%
    pivot_longer(starts_with("study1"), names_to = "Factor", values_to = "Loading_study1") %>%
    mutate(Factor = str_replace_all(Factor, "study1_", ""))
) %>%
  mutate(cutoff = case_when(
    Loading_pilot > .32 & Loading_study1 <= .32 ~ "Pilot only",
    Loading_pilot <= .32 & Loading_study1 > .32 ~ "Study 1 only",
    Loading_pilot > .32 & Loading_study1 > .32 ~ "Both studies",
    Loading_pilot <= .32 & Loading_study1 <= .32 ~ "Neither study"
  )) %>%
  filter(cutoff != "Neither study") %>%
  mutate(cutoff = factor(cutoff, levels = c("Both studies", "Pilot only", "Study 1 only"))) 


efa_data_combined %>%
  mutate(
    Factor = case_when(
      Factor == "f1" ~ "Factor 1 - Household stability/conflict",
      Factor == "f2" ~ "Factor 2 - Monitoring/neglect",
      Factor == "f3" ~ "Factor 3 - Macro unpredictability",
      Factor == "f4" ~ "Factor 4 - Clutter/disorganization",
      Factor == "f5" ~ "Factor 5 - People in household",
    )
  ) %>%
  ggplot(aes(Loading_pilot, Loading_study1, color = cutoff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = var), nudge_y = 0.05, nudge_x = 0.05) +
  theme_classic() +
  scale_color_manual(values = c("#198B27", "black", "darkgrey")) +
  facet_wrap(~Factor) +
  labs(
    x = "\nFactor Loading - Pilot study",
    y = "Factor Loading - Study 1\n",
    color = "Loading > .32"
  )

# Compute variables based on EFA for pilot data and join with study data
pilot_efa_vars <- efa_data_combined %>%
  select(var, Factor, Loading_pilot) %>%
  mutate(Loading_pilot = ifelse(Loading_pilot <= .32, NA, Loading_pilot)) %>%
  drop_na(Loading_pilot) %>%
  pivot_wider(names_from = "Factor", values_from = "Loading_pilot") %>%
  mutate(label = case_when(
    !is.na(f1)                                                 ~ "efa_stability_conflict",
    !is.na(f2) & is.na(f1)                                     ~ "efa_monitoring_neglect",
    !is.na(f3) & is.na(f1) & is.na(f2)                         ~ "efa_spatial_unp",
    !is.na(f4) & is.na(f1) & is.na(f2) & is.na(f3)             ~ "efa_clutter_disorganization",
    !is.na(f5) & is.na(f1) & is.na(f2) & is.na(f3) & is.na(f4) ~ "efa_social_unpredictability",
    TRUE ~ "other"
  ))

cleaned_data_pilot %<>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .)) %>%
  mutate(
    efa_daily_unp     = across(pilot_efa_vars %>% filter(label == "efa_stability_conflict") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_routine       = across(pilot_efa_vars %>% filter(label == "efa_monitoring_neglect") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_spatial_unp   = across(pilot_efa_vars %>% filter(label == "efa_spatial_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_chaos_clutter = across(pilot_efa_vars %>% filter(label == "efa_clutter_disorganization") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_social_unp    = across(pilot_efa_vars %>% filter(label == "efa_social_unpredictability") %>% pull(var)) %>% rowMeans(., na.rm = T),
  )


# Compute variables based on EFA for pilot data and join with study data
study1_efa_vars <- efa_data_combined %>%
  select(var, Factor, Loading_study1) %>%
  mutate(Loading_study1 = ifelse(Loading_study1 <= .32, NA, Loading_study1)) %>%
  drop_na(Loading_study1) %>%
  pivot_wider(names_from = "Factor", values_from = "Loading_study1") %>%
  mutate(label = case_when(
    !is.na(f1)                                                 ~ "efa_stability_conflict",
    !is.na(f2) & is.na(f1)                                     ~ "efa_monitoring_neglect",
    !is.na(f3) & is.na(f1) & is.na(f2)                         ~ "efa_spatial_unp",
    !is.na(f4) & is.na(f1) & is.na(f2) & is.na(f3)             ~ "efa_clutter_disorganization",
    !is.na(f5) & is.na(f1) & is.na(f2) & is.na(f3) & is.na(f4) ~ "efa_social_unpredictability",
    TRUE ~ "other"
  ))

cleaned_data_study1 %<>%
  mutate(
    efa_daily_unp     = across(study1_efa_vars %>% filter(label == "efa_stability_conflict") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_routine       = across(study1_efa_vars %>% filter(label == "efa_monitoring_neglect") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_spatial_unp   = across(study1_efa_vars %>% filter(label == "efa_spatial_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_chaos_clutter = across(study1_efa_vars %>% filter(label == "efa_clutter_disorganization") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_social_unp    = across(study1_efa_vars %>% filter(label == "efa_social_unpredictability") %>% pull(var)) %>% rowMeans(., na.rm = T),
  )

cleaned_data_pilot %>%
  select(starts_with("efa"), vio_comp = violence_composite, unp_comp = unpredictability_composite) %>%
  cor(., use = "complete.obs") %>%
  corrplot::corrplot(method = "number")

cleaned_data_study1 %>%
  select(starts_with("efa"), vio_comp, unp_comp) %>%
  cor(., use = "complete.obs") %>%
  corrplot::corrplot(method = "number")



# 2. Multiverse Analyses with EFA factors ------------------------------------

# Pilot study
pilot_data <- read_csv("data/1_pilot/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         unp_comp = unpredictability_composite, rt_flanker_congruent, rt_flanker_incongruent, acc_flanker_congruent, acc_flanker_incongruent,
         a_flanker = flanker_ssp_a, t0_flanker = flanker_ssp_t0, p_flanker = flanker_ssp_p, interference_flanker = flanker_ssp_interference) %>%
  mutate(
    id = str_c("pilot_", id),
    study = -1,
    counterbalance = 'pilot',
    scale_factor = ifelse(round(scale_factor, 4) == 0.3081, 0,1)
  )


# Study 1

study1_data <-  read_csv("data/2_study1/2_cleaned_data.csv") |> 
  select(id, scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, meta_captcha,
         unp_comp, rt_flanker_congruent_std, rt_flanker_incongruent_std, acc_flanker_congruent_std, acc_flanker_incongruent_std,
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
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 0,1)
  )

# Combine data of pilot study and study 1
expl_ssp_data_pooled <- bind_rows(pilot_data, study1_data) |> 
  filter(is.finite(interference_flanker)) |> 
  mutate(rt_diff = rt_flanker_incongruent - rt_flanker_congruent)



expl_ssp_data_enh <- cleaned_data_study1 %>%
  select(id, unp_pcunp_quic_comp, unp_subj_comp, unp_obj_comp, unp_comp, 
         starts_with("efa"), matches("^(a|t0|p|sda|rd|interference)_flanker_(std|enh)"),
         scale_factor, fullscreenenter, meta_captcha, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  
  pivot_longer(
    cols = matches("flanker"),
    names_to = c(".value", "condition"),
    names_pattern = "(^.*_flanker)(.*)"
  ) %>%
  mutate(
    condition = ifelse(condition == "_std", 0, 1),
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 1, -1))

expl_ssp_data_deg <- cleaned_data_study1 %>%
  select(id, unp_pcunp_quic_comp, unp_subj_comp, unp_obj_comp, unp_comp, 
         starts_with("efa"), matches("^(a|t0|p|sda|rd|interference)_flanker_(std|deg)"),
         scale_factor, fullscreenenter, meta_captcha, fullscreenexit, attention_interrupt_sum, att_noise) %>%
  pivot_longer(
    cols = matches("flanker"),
    names_to = c(".value", "condition"),
    names_pattern = "(^.*_flanker)(.*)"
  ) %>%
  mutate(
    condition = ifelse(condition == "_std", 0, 1),
    scale_factor = ifelse(round(scale_factor, 4) == 0.9007, 1, -1)
  )

## 2.1 Run models ----
expl_flanker_mult_pooled <- multitool::run_multiverse(
  .grid = 
    expl_ssp_data_pooled |> 
    multitool::add_variables("iv", unp_comp) |> 
    multitool::add_variables("dv", a_flanker, p_flanker, t0_flanker, interference_flanker) |> 
    multitool::add_filters(
      scale_factor == 1,
      (scale_factor == 1 & study == 1) | study == -1,
      (scale_factor == 1 & study == -1) | study == 1,
      fullscreenenter == 1,
      fullscreenexit == 0,
      attention_interrupt_sum < 1,
      meta_captcha > 0.4,
      att_noise %in% c(0,1,2)
    ) |>
    multitool::add_preprocess(process_name = "scale_iv",  'mutate({iv} = scale({iv}))') |>
    multitool::add_model("lm", lm({dv} ~ {iv} + study)) |>
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

expl_flanker_mult_enh <- multitool::run_multiverse(
  .grid = 
    expl_ssp_data_enh |> 
    multitool::add_variables("iv", unp_pcunp_quic_comp, unp_subj_comp, unp_obj_comp, unp_comp) |> 
    multitool::add_variables("dv", a_flanker, p_flanker, t0_flanker, interference_flanker) |> 
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
    multitool::add_postprocess(postprocess_name = "ss_task", code = sim_slopes(pred = {iv}, modx = condition, modx.values = c(0,1))) |> 
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [0,1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)


expl_flanker_mult_deg <- multitool::run_multiverse(
  .grid = 
    expl_ssp_data_deg |> 
    multitool::add_variables("iv", unp_pcunp_quic_comp, unp_subj_comp, unp_obj_comp, unp_comp) |> 
    multitool::add_variables("dv", a_flanker, p_flanker, t0_flanker, interference_flanker) |> 
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
    multitool::add_postprocess(postprocess_name = "ss_task", code = sim_slopes(pred = {iv}, modx = condition, modx.values = c(0,1))) |> 
    multitool::add_postprocess("points", ggpredict(terms = c("{iv} [-1,1]", "condition [0,1]"))) |> 
    multitool::add_postprocess(postprocess_name = "std_coef", 'standardize_parameters()') |> 
    multitool::expand_decisions()
)

save(expl_flanker_mult_enh, expl_flanker_mult_deg, file = "preregistrations/2_study1/analysis_objects/exploratory_mult_results.RData")


## 2.2 Process Results ----


### 2.2.3 Standard - Enhanced comparison ----
exploratory_ssp_points_enh <- reveal(expl_flanker_mult_enh, .what = ggpredict_fitted, .which = ggpredict_full, .unpack_specs = T) |> 
  rename(level = x) |> 
  unite("vars", c(iv, dv), sep = "-", remove = FALSE)



exploratory_ssp_simslopes_enh <- reveal(expl_flanker_mult_enh, .what = sim_slopes_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) 

### 2.2.4 Standard - Degraded comparison ----
exploratory_ssp_points_deg <- reveal(expl_flanker_mult_deg, .what = ggpredict_fitted, .which = ggpredict_full, .unpack_specs = T) |> 
  rename(level = x) |> 
  unite("vars", c(iv, dv), sep = "-", remove = FALSE)

exploratory_ssp_simslopes_deg <- reveal(expl_flanker_mult_enh, .what = sim_slopes_fitted, .which = sim_slopes_tidy, .unpack_specs = TRUE) 

## 2.3. Create Multiverse summaries ----

## 5.1 Pooled data ----

expl_ssp_pooled_effects_sum <- reveal(expl_flanker_mult_pooled, .what = lm_fitted, .which = lm_tidy, .unpack_specs = TRUE) |> 
  filter(
    !term %in% c("study")
  ) |> 
  left_join(
    reveal(expl_flanker_mult_pooled, .what = standardize_parameters_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  )

# Store the median regression effects
expl_ssp_pooled_medians_sum <- unique(expl_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    expl_ssp_pooled_effects_sum |> 
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
  setNames(unique(expl_ssp_pooled_effects_sum $dv))


# Store the influence of filter decisions
expl_ssp_pooled_decisions_sum <- unique(expl_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    expl_ssp_pooled_effects_sum |> 
      filter(dv == x) |> 
      filter(!term %in% c("study", "(Intercept)")) |> 
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
  setNames(unique(expl_ssp_pooled_effects_sum$dv))


expl_ssp_pooled_variance_sum <- unique(expl_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    data <- expl_ssp_pooled_effects_sum |> 
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
  setNames(unique(expl_ssp_pooled_effects_sum$dv))


### 2.3.1 Standard - Enhanced Comparisons ----

expl_ssp_enh_effects_sum <- reveal(expl_flanker_mult_enh, .what = lmer_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
  filter(
    effect == 'fixed',
    term != "(Intercept)"
  ) |> 
  left_join(
    reveal(expl_flanker_mult_enh, .what = standardize_parameters_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  ) |> 
  unite("vars", c(iv, dv), sep = "-", remove = FALSE)

# Store the median effects
expl_ssp_enh_medians_sum <- unique(expl_ssp_enh_effects_sum$vars) |> 
  map(function(x) {
    
    expl_ssp_enh_effects_sum |> 
      filter(vars == x, str_detect(term, ":")) |> 
      mutate(term = ifelse(str_detect(term, ":"), "interaction", "main effect")) |> 
      group_by(vars, term) |> 
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
  setNames(unique(expl_ssp_enh_effects_sum$vars))


# Store the plotting points per decision
expl_ssp_enh_points_sum <- unique(exploratory_ssp_points_enh$vars) |> 
  map(function(x){
    
    exploratory_ssp_points_enh |> 
      filter(vars == x) |> 
      left_join(expl_ssp_enh_effects_sum |> filter(str_detect(term, ":")) |>  select(decision, vars, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(exploratory_ssp_points_enh$vars))


# Store the influence of filter decisions
expl_ssp_enh_decisions_sum <- unique(expl_ssp_enh_effects_sum$vars) |> 
  map(function(x) {
    
    expl_ssp_enh_effects_sum |> 
      filter(vars == x) |> 
      filter(str_detect(term, ":")) |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(vars, filters, setting) |> 
      summarise(sum_pvalue = sum(p.value < .05) / n() * 100 ) |> 
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
  setNames(unique(expl_ssp_enh_effects_sum$vars))

expl_ssp_enh_variance_sum <- unique(expl_ssp_enh_effects_sum$vars) |> 
  map(function(x) {
    
    data <- expl_ssp_enh_effects_sum |> 
      filter(vars == x) |> 
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
  setNames(unique(expl_ssp_enh_effects_sum$vars))

### 2.3.2 Standard - Degraded comparison ----

expl_ssp_deg_effects_sum <- reveal(expl_flanker_mult_deg, .what = lmer_fitted, .which = lmer_tidy, .unpack_specs = TRUE) |> 
  filter(
    effect == 'fixed',
    term != "(Intercept)"
  ) |> 
  left_join(
    reveal(expl_flanker_mult_deg, .what = standardize_parameters_fitted, matches("full"), .unpack_specs = TRUE) |> 
      rename_with(.cols = c("CI", "CI_low", "CI_high"), ~str_c("Std_", .)) |> 
      select(decision, term = Parameter, starts_with("Std"))
  ) |> 
  unite("vars", c(iv, dv), sep = "-", remove = FALSE)

# Store the median effects
expl_ssp_deg_medians_sum <- unique(expl_ssp_deg_effects_sum$vars) |> 
  map(function(x) {
    
    expl_ssp_deg_effects_sum |> 
      filter(vars == x, str_detect(term, ":")) |> 
      mutate(term = ifelse(str_detect(term, ":"), "interaction", "main effect")) |> 
      group_by(vars, term) |> 
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
  setNames(unique(expl_ssp_deg_effects_sum$vars))



# Store the plotting points per decision
expl_ssp_deg_points_sum <- unique(exploratory_ssp_points_deg$vars) |> 
  map(function(x){
    
    exploratory_ssp_points_deg |> 
      filter(vars == x) |> 
      left_join(expl_ssp_deg_effects_sum  |> filter(str_detect(term, ":")) |>  select(decision, vars, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value <.05, "sig", "non-sig"))
  }) |> 
  setNames(unique(exploratory_ssp_points_deg$vars))


# Store the influence of filter decisions
expl_ssp_deg_decisions_sum <- unique(expl_ssp_deg_effects_sum$vars) |> 
  map(function(x) {
    
    expl_ssp_deg_effects_sum |> 
      filter(vars == x) |> 
      filter(str_detect(term, ":")) |> 
      pivot_longer(
        c(scale_factor, fullscreenenter, fullscreenexit, attention_interrupt_sum, meta_captcha, att_noise),
        names_to = "filters",
        values_to = "setting"
      ) |> 
      group_by(vars, filters, setting) |> 
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
  setNames(unique(expl_ssp_deg_effects_sum$vars))


expl_ssp_deg_variance_sum <- unique(expl_ssp_deg_effects_sum$vars) |> 
  map(function(x) {
    
    data <- expl_ssp_deg_effects_sum |> 
      filter(vars == x) |> 
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
  setNames(unique(expl_ssp_deg_effects_sum$vars))

save(exploratory_ssp_points_enh, exploratory_ssp_simslopes_enh, exploratory_ssp_points_deg, exploratory_ssp_simslopes_deg, 
     expl_ssp_pooled_effects_sum, expl_ssp_pooled_medians_sum, expl_ssp_pooled_decisions_sum, expl_ssp_pooled_variance_sum,
     expl_ssp_enh_effects_sum, expl_ssp_enh_medians_sum, expl_ssp_enh_points_sum, expl_ssp_enh_decisions_sum, expl_ssp_enh_variance_sum,
     expl_ssp_deg_effects_sum, expl_ssp_deg_medians_sum, expl_ssp_deg_points_sum, expl_ssp_deg_decisions_sum, expl_ssp_deg_variance_sum,
     file = "preregistrations/2_study1/analysis_objects/exploratory_multiverse_summaries.RData")

## 3. Potential effect moderators ----

# Addresses secondary aim 2:
# Explore the role of state anxiety, hunger, and sleep deprivation as potential moderators of the relationship between adversity 
# and attention performance. Anxiety might enhance attention performance by making participants more vigilant. Conversely, hunger 
# and sleep deprivation could have a negative effect on performance.

cleaned_data |> 
  select(stai_s_mean, hungry, sleep, vio_comp, unp_comp, 
         p_flanker_std, p_flanker_enh, p_flanker_deg,
         interference_flanker_std, interference_flanker_enh, interference_flanker_deg) |> 
  cor(use = 'complete.obs') |>
  corrplot::corrplot(method = "number")


## 4. Correlations with Temporal orientation ----

#Addresses secondary aim 3:
# Explore bivariate correlations between measures of adversity, attention, and measures of temporal orientation 
# (i.e., impulsivity and future orientation).

cleaned_data |> 
  select(fos_fo_mean, fos_pa_mean, fos_tp_mean, fos_fc_mean, impuls_mean, vio_comp, unp_comp, 
         p_flanker_std, p_flanker_enh, p_flanker_deg,
         interference_flanker_std, interference_flanker_enh, interference_flanker_deg,
         a_flanker_std, a_flanker_enh, a_flanker_deg) |> 
  cor(use = 'complete.obs') |>
  corrplot::corrplot(method = "number")


## 5. Correlations between adversity and current depressive symptoms

# Addresses secondary aim 4:
# Explore the correlation between current depressive symptoms and retrospective measures of adversity, 
# which might reflect a negativity bias in recalling past events driven by current depressive symptoms 
# [@nivison_2021].

cleaned_data |> 
  select(depression_mean, vio_comp, unp_comp) |> 
  cor(use = 'complete.obs') |>
  corrplot::corrplot(method = "number")
