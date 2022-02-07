# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)

load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "efa_data.Rdata"))

cleaned_data %<>%
  left_join(efa_variables)


# IVs ---------------------------------------------------------------------

txt_ivs <- 
  cleaned_data %>%
  summarize(
    across(
      .cols = matches("unp_mean|chaos_mean|quic_.*_mean|unpredictability_composite|violence_(composite|mean)|fighting_mean|
                      poverty_composite|ses_(subj|obj)_mean|edu_parents_recoded|income_past_recoded|class_past_recoded|
                      impuls_mean|fos_(fo|pa|fc|tp)_mean|stai_s_mean|depression_mean"), 
      list(mean = mean, sd = sd, min = min, max = max), na.rm=T, .names = "{.fn}.{.col}")
  ) %>% 
  mutate(across(everything(), ~round(.x, 2))) %>% 
  pivot_longer(everything()) %>% 
  separate(name, into = c("stat","var"), sep = "\\.") %>% 
  pivot_wider(names_from = var, values_from = c(value)) %>% 
  select(-stat) %>% 
  map(function(x) list(mean = x[1], sd = x[2]))

# DVs ---------------------------------------------------------------------

txt_dvs <- 
  cleaned_data %>%
  summarize(
    across(
      .cols = matches("^rt_|_ml_|_EZ_|(a|t0|p|rd|sda)_flanker)|^efa_"), 
      list(mean = mean, sd = sd), na.rm=T, .names = "{.fn}.{.col}")
  ) %>% 
  mutate(across(everything(), ~round(.x, 2))) %>% 
  pivot_longer(everything()) %>% 
  separate(name, into = c("stat","var"), sep = "\\.") %>% 
  pivot_wider(names_from = var, values_from = c(value)) %>% 
  select(-stat) %>% 
  map(function(x) list(mean = x[1], sd = x[2]))


# Correlations ------------------------------------------------------------

txt_cors <- 
  map(
    c(
      "(violence|fighting)_mean",
      "ses_subj_mean_recoded|ses_obj_mean"
    ), 
    function(x){
      corrs <- 
        cleaned_data %>% 
        select(matches(x)) %>% 
        psych::corr.test()
      
      if(is.matrix(corrs$n)){
        n <- corrs$n[1,2]
      }else{
        n <- corrs$n
      }
      
      print(corrs$r)
      
      list(r = corrs$r[1,2] %>% round(2), n = n)
    }) %>% 
  set_names(c("vio")) 


## Correlations ----

save(
  txt_ivs,
  txt_dvs,
  txt_cors,
  file = "preregistrations/1_pilot/staged_objects.Rmd"
)
