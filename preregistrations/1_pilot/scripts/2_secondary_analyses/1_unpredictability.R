
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


primary_analyses_data <- cleaned_data %>%
  select(id, matches("^rt_(change|cueing|flanker)"), unpredictability_composite) %>%
  mutate(across(matches("^rt_(change|cueing|flanker)"), ~scale(log(.)) %>% as.numeric()))



# Effect of unpredictability on RTs -----------------------------------------------

secondary_unp_raw <- list("change", "cueing", "flanker") %>%
  map(function(x) {
    df <- cleaned_data %>%
      drop_na(matches(str_glue("^rt_{x}")), unpredictability_composite) %>%
      select(id, matches(str_glue("^rt_{x}")), starts_with("acc"), unpredictability_composite, matches("event_during"), scale_factor) %>%
      mutate(
        unpredictability_composite_z = scale(unpredictability_composite) %>% as.numeric(),
        scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 
    
    
    if(x == "change") {
      df %<>%
        select(everything(), event_during_task = matches(str_glue("event_during_{x}"))) %>%
        filter(!event_during_task) %>%
        mutate(rt_change = rt_change*1000)
      #  mutate(rt_log = log(rt_change+1),
      #        event_during_task = ifelse(event_during_task == FALSE, -1, 1)) 
      
      fit_rt  = lm(data = df, rt_change ~ unpredictability_composite_z + scale_factor) 
      fit_acc = lm(data = df, acc_change ~ unpredictability_composite_z + scale_factor) 
      
      mod_standardized_rt <- standardize_parameters(fit_rt)
      mod_standardized_acc <- standardize_parameters(fit_acc)
      
      return(
        list(
          data               = df,
          model              = list(rt = fit_rt, acc = fit_acc),
          model_tidy         = list(rt = broom::tidy(fit_rt), acc = broom::tidy(fit_acc)),
          model_standardized = list(rt = mod_standardized_rt, acc = mod_standardized_acc)
        ))
    }
    
    if(x != "change") {
      # Conditions in long format
      df %<>%
        rename_with(.cols = starts_with("rt_"), ~str_replace_all(., "^rt_.*_", "")) %>%
        select(everything(), event_during_task = matches(str_glue("event_during_{x}"))) %>%
        pivot_longer(cols = !matches("(id|unpredictability|acc|event|scale)"), names_to = "condition", values_to = "rt") %>%
        filter(!event_during_task) %>%
        mutate(
          rt = rt * 1000,
          #   rt_log        = rt+1),
          condition_sum = ifelse(condition == "congruent" | condition == "neutral", -1, 1)#,
          # event_during_task = ifelse(event_during_task == FALSE, -1, 1)
        ) 
      
      fit = lmer(data = df, rt ~ scale_factor + unpredictability_composite_z*condition_sum + (1|id))
      mod_standardized <- standardize_parameters(fit)
      
      # Marginal effects
      mod_effects              <- ggpredict(model = fit, terms = c("unpredictability_composite_z [-1,1]", "condition_sum [-1,1]"))
      # Simple slopes
      mod_ss_unpredictability  <- sim_slopes(fit, pred = "condition_sum",  modx = "unpredictability_composite_z", modx.values = c(-1,1))
      mod_ss_condition         <- sim_slopes(fit, pred = "unpredictability_composite_z", modx = "condition_sum", modx.values = c(-1,1))
      
      list(
        data                = df,
        model               = fit,
        model_tidy          = broom.mixed::tidy(fit),
        model_effects       = mod_effects,
        model_standardized  = mod_standardized,
        model_fit_stats     = model_performance(fit),
        model_simple_slopes = list(vio = mod_ss_unpredictability, condition = mod_ss_condition)
      )
    }
  }) 






# Effect of Poverty on RTs -----------------------------------------------

secondary_pov_raw <- list("change", "cueing", "flanker") %>%
  map(function(x) {
    df <- cleaned_data %>%
      drop_na(matches(str_glue("^rt_{x}")), poverty_composite) %>%
      select(id, matches(str_glue("^rt_{x}")), starts_with("acc"), poverty_composite, matches("event_during"), scale_factor) %>%
      mutate(
        poverty_composite_z = scale(poverty_composite) %>% as.numeric(),
        scale_factor = ifelse(round(scale_factor, 4) != '0.3081', -1, 1)) 
    
    
    if(x == "change") {
      df %<>%
        select(everything(), event_during_task = matches(str_glue("event_during_{x}"))) %>%
        mutate(rt_log = log(rt_change),
               event_during_task = ifelse(event_during_task == FALSE, -1, 1)) 
      
      fit_rt  = lm(data = df, rt_log ~ poverty_composite_z + event_during_task + scale_factor) 
      fit_acc = lm(data = df, acc_change ~ poverty_composite_z + event_during_task + scale_factor) 
      
      return(
        list(
          data       = df,
          model      = list(rt = fit_rt, acc = fit_acc),
          model_tidy = list(rt = broom::tidy(fit_rt), acc = broom::tidy(fit_acc))
        ))
    }
    
    if(x != "change") {
      # Conditions in long format
      df %<>%
        rename_with(.cols = starts_with("rt_"), ~str_replace_all(., "^rt_.*_", "")) %>%
        select(everything(), -starts_with("acc"), event_during_task = matches(str_glue("event_during_{x}"))) %>%
        pivot_longer(cols = !matches("(id|poverty|event|scale)"), names_to = "condition", values_to = "rt") %>%
        mutate(
          rt_log        = log(rt),
          condition_sum = ifelse(condition == "congruent" | condition == "neutral", -1, 1),
          event_during_task = ifelse(event_during_task == FALSE, -1, 1)) 
      
      fit = lmer(data = df, rt_log ~ event_during_task + scale_factor + poverty_composite_z*condition_sum + (1|id))
      
      # Marginal effects
      mod_effects      <- ggpredict(model = fit, terms = c("poverty_composite_z [-1,1]", "condition_sum [-1,1]"))
      # Simple slopes
      mod_ss_poverty  <- sim_slopes(fit, pred = "condition_sum",  modx = "poverty_composite_z", modx.values = c(-1,1))
      mod_ss_condition <- sim_slopes(fit, pred = "poverty_composite_z", modx = "condition_sum", modx.values = c(-1,1))
      
      list(
        data              = df,
        model             = fit,
        model_tidy        = broom.mixed::tidy(fit),
        mod_effects       = mod_effects,
        mod_fit_stats     = model_performance(fit),
        mod_simple_slopes = list(vio = mod_ss_poverty, condition = mod_ss_condition)
      )
    }
  }) 





save(secondary_unp_raw, secondary_pov_raw, file = here("data", "1_pilot", "3_secondary_analyses_results.RData"))
