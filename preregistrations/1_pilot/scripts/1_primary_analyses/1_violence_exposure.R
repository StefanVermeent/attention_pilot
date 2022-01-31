
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(here)
library(lme4)
library(broom.mixed)
library(lmerTest)
library(performance)
library(ggeffects)
library(interactions)


load(here("data", "1_pilot", "2_cleaned_data.Rdata"))

analysis_data <- cleaned_data %>%
  select(id, matches("^rt_(change|cueing|flanker)"), contains(c("EZ", "_ml_")), starts_with("acc"), violence_composite, matches("event_during"), scale_factor) %>%
  mutate(across(matches("^rt_(change|cueing|flanker)"), ~ . * 1000)) %>%
  mutate(violence_composite_z   = scale(violence_composite) %>% as.numeric(),
         scale_factor_sum       = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)
         ) 


# Reaction Time and Accuracy Analyses -------------------------------------

# Violence on Change Detection - RT
primary_vio_raw_change <- list(analysis_data) %>%
  map(., function(x) {
  
    fit_rt  = lm(data = x, rt_change ~ violence_composite_z + scale_factor) 
    fit_acc = lm(data = x, acc_change ~ violence_composite_z + scale_factor) 
    
    mod_standardized_rt <- standardize_parameters(fit_rt)
    mod_standardized_acc <- standardize_parameters(fit_acc)
    
    return(
      list(
        data               = df,
        model              = list(rt = fit_rt, acc = fit_acc),
        model_tidy         = list(rt = broom::tidy(fit_rt), acc = broom::tidy(fit_acc)),
        model_standardized = list(rt = mod_standardized_rt, acc = mod_standardized_acc)
      ))
  })


# Violence on Cued Attention - RT

primary_vio_raw_cueing <- list(analysis_data) %>%
  map(function(x) {
    
    x %<>%
      rename_with(.cols = starts_with("rt_"), ~str_replace_all(., "^rt_.*_", "")) %>%
      select(id, cued, neutral, violence_composite_z, scale_factor) %>%
      pivot_longer(cols = c("neutral", "cued"), names_to = "condition", values_to = "rt") %>%
      mutate(condition_sum = ifelse(condition == "neutral", -1, 1)) 
    
    fit = lmer(data = x, rt ~ scale_factor + violence_composite_z*condition_sum + (1|id))
    mod_standardized <- standardize_parameters(fit)
    
    # Marginal effects
    mod_effects      <- ggpredict(model = fit, terms = c("violence_composite_z [-1,1]", "condition_sum [-1,1]"))
    # Simple slopes
    mod_ss_violence  <- sim_slopes(fit, pred = "condition_sum",  modx = "violence_composite_z", modx.values = c(-1,1))
    mod_ss_condition <- sim_slopes(fit, pred = "violence_composite_z", modx = "condition_sum", modx.values = c(-1,1))
    
    list(
      data                = df,
      model               = fit,
      model_tidy          = broom.mixed::tidy(fit),
      model_effects       = mod_effects,
      model_standardized  = mod_standardized,
      model_fit_stats     = model_performance(fit),
      model_simple_slopes = list(vio = mod_ss_violence, condition = mod_ss_condition)
    )
    })
  

# Violence on Flanker - RT

primary_vio_raw_flanker <- list(analysis_data) %>%
  map(function(x) {
    
    x %<>%
      rename_with(.cols = starts_with("rt_"), ~str_replace_all(., "^rt_.*_", "")) %>%
      select(id, congruent, incongruent, violence_composite_z, scale_factor) %>%
      pivot_longer(cols = c("congruent", "incongruent"), names_to = "condition", values_to = "rt") %>%
      mutate(condition_sum = ifelse(condition == "congruent", -1, 1)) 
    
    fit = lmer(data = x, rt ~ scale_factor + violence_composite_z*condition_sum + (1|id))
    mod_standardized <- standardize_parameters(fit)
    
    # Marginal effects
    mod_effects      <- ggpredict(model = fit, terms = c("violence_composite_z [-1,1]", "condition_sum [-1,1]"))
    # Simple slopes
    mod_ss_violence  <- sim_slopes(fit, pred = "condition_sum",  modx = "violence_composite_z", modx.values = c(-1,1))
    mod_ss_condition <- sim_slopes(fit, pred = "violence_composite_z", modx = "condition_sum", modx.values = c(-1,1))
    
    list(
      data                = df,
      model               = fit,
      model_tidy          = broom.mixed::tidy(fit),
      model_effects       = mod_effects,
      model_standardized  = mod_standardized,
      model_fit_stats     = model_performance(fit),
      model_simple_slopes = list(vio = mod_ss_violence, condition = mod_ss_condition)
    )
  })



# DDM Analyses ------------------------------------------------------------

primary_vio_ddm_change <- list(analysis_data) %>%
  
  map(function(x) {
    
    x %>%
      filter(!change_ml_bad_fit)
    
    fit_v = lm(data = x, change_EZ_v ~ violence_composite_z + scale_factor) 
    fit_a = lm(data = x, change_EZ_a ~ violence_composite_z + scale_factor) 
    fit_t0 = lm(data = x, change_EZ_t0 ~ violence_composite_z + scale_factor) 

    
    mod_standardized_v <- standardize_parameters(fit_v)
    mod_standardized_a <- standardize_parameters(fit_a)
    mod_standardized_t0 <- standardize_parameters(fit_t0)
    
    return(
      list(
        data               = x,
        model              = list(v = fit_v, a = fit_a, t0 = fit_t0),
        model_tidy         = list(v = broom::tidy(fit_v), a = broom::tidy(fit_a), t0 = broom::tidy(fit_t0)),
        model_standardized = list(v = mod_standardized_v, a = mod_standardized_a, t0 = mod_standardized_t0)
      ))
  })



primary_vio_ddm_cueing <- list("v", "a", "t0") %>%
  
  map(function(x) {
    
    df <- analysis_data %>%
      select(id, violence_composite_z, glue("cueing_cued_EZ_{x}"), glue("cueing_neutral_EZ_{x}"), scale_factor) %>%
      pivot_longer(c(glue("cueing_cued_EZ_{x}"), glue("cueing_neutral_EZ_{x}")),
                   names_to = "condition", values_to = 'parameter') %>%
      mutate(
        condition_sum = ifelse(condition == glue("cueing_cued_EZ_{x}"), 1, -1)
      )
    
    fit = lmer(data = df, parameter ~ violence_composite_z*condition_sum + scale_factor + (1|id)) 
    summary(fit)
    
    # Marginal effects
    mod_effects      <- ggpredict(model = fit, terms = c("violence_composite_z [-1,1]", "condition_sum [-1,1]"))
    # Simple slopes
    mod_ss_violence  <- sim_slopes(fit, pred = "condition_sum",  modx = "violence_composite_z", modx.values = c(-1,1))
    mod_ss_condition <- sim_slopes(fit, pred = "violence_composite_z", modx = "condition_sum", modx.values = c(-1,1))
   
    return(
      list(
        data               = df,
        model              = fit,
        model_tidy         =broom::tidy(fit),
       
        model_effects       = mod_effects,
        model_fit_stats     = model_performance(fit_v),
        model_simple_slopes = list(vio = mod_ss_violence, condition = mod_ss_condition)
      ))
  })



primary_vio_ddm_cueing[[1]]$model_effects %>%
  rename(violence = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(violence), color = factor(violence))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Neutral", "Cued")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Violence exposure\n",
    x = "\nCondition",
    y = "v"
  )


save(
  primary_vio_raw_change,
  primary_vio_raw_cueing,
  primary_vio_raw_flanker,
  primary_vio_ddm_change,
  file = here("data", "1_pilot", "3_primary_analyses_results.RData")
  )
