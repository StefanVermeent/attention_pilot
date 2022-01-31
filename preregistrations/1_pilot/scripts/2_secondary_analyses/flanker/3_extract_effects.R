# Setup -------------------------------------------------------------------
# Libraries
library(tidyverse)
library(here)

# Load results
load(here("data", "1_pilot", "2_secondary_analyses", "flanker", "2_multiverse_results.Rdata"))

# Raw effect size data ----------------------------------------------------
secondary_effects_flanker <- 
  secondary_unp_flanker %>% 
  map_df(function(multiverse){
    
    # All parameters from multiverse
    params <-
      bind_cols(
        n = multiverse$n_model,
        left_join(
          multiverse$model_tidy %>% select(mod_term:mod_p.value), 
          multiverse$model_std_effects %>% rename_with(~paste0("mod_",.x)), 
          by = c("mod_term" = "mod_Parameter")
        ),
        multiverse$specifications %>% 
          select(-spec_expr) %>% 
          pivot_wider(names_from = "spec_var", values_from = "name") %>% 
          rename_with(.cols = !spec_number, ~paste0("spec_",.x))
      )}) %>% 
  rename_with(tolower) %>% 
  filter(!str_detect(mod_term, "^sd__")) %>% 
  group_by(mod_term, spec_dv_type) %>% 
  mutate(
    dv         = spec_dv_type,
    dv_group   = ifelse(dv %in% c("RT", "RT (log)"), "Raw scores", "DDM"),
    spec_rank  = as.numeric(fct_reorder(as_factor(spec_number), mod_std_coefficient)),
    median_dbl = median(mod_std_coefficient),
    median_chr = median_dbl %>% round(2) %>% as.character,
    pval_prop  = (sum(mod_p.value < .05)/(512/8)) * 100
  ) %>% 
  ungroup() %>% 
  mutate(
    mod_sig        = case_when(mod_p.value <  .05 & mod_std_coefficient > 0 ~ "pos-sig",
                               mod_p.value <  .05 & mod_std_coefficient < 0 ~ "neg-sig",
                               mod_p.value >= .05 ~ "non"),
    mod_term_num   = case_when(str_detect(mod_term, "Intercept")  ~ -1,
                               mod_term == "unpredictability_composite_c" ~ 0,
                               mod_term == "condition"            ~ 0,
                               str_detect(mod_term, ":")          ~ 1
                               ),
    mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
                               mod_term == "unpredictability_composite_c" ~ "unpredictability",
                               mod_term == "condition"            ~ "Task condition",
                               str_detect(mod_term, ":")          ~ "Interaction",
                               ),
    mod_term_group = factor(mod_term_num,
                            levels = c(-1,0,1), 
                            labels = c("Intercept","Main Effect","Interaction")),
    mod_term_unique = case_when(mod_term == "unpredictability_composite_c" ~ "unp",
                                mod_term == "unpredictability_composite_c:condition_sum" ~ "unp~symbol('\\264')~Task-Condition",
                                T ~ mod_term_label),
    mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num),
    pval_prop      = paste0(round(pval_prop,2),"% of ps"," < .05"),
    pval_prop      = fct_reorder(pval_prop, mod_term_num))

# Medians -----------------------------------------------------------------
secondary_effects_medians <- 
  secondary_effects_flanker %>% 
  select(dv, dv_group, contains("_term"), contains("median_")) %>% 
  distinct()

# Interaction Data --------------------------------------------------------
secondary_effects_points <- 
  secondary_unp_flanker %>% 
  map_df(function(multiverse){
        specs <- multiverse$specifications %>% 
          select(-spec_expr) %>% 
          pivot_wider(names_from = "spec_var", values_from = "name") %>% 
          rename_with(.cols = !spec_number, ~paste0("spec_",.x))
        
        bind_cols(multiverse$model_effects, n = multiverse$n, specs) %>% tibble()
      }) %>% 
  left_join(
    secondary_effects_flanker %>% 
      filter(str_detect(mod_term_label, "Interaction")) %>% 
      select(spec_number, mod_sig)
  ) %>%
  mutate(
    dv         = spec_dv_type,
    dv_group   = ifelse(dv %in% c("RT", "RT (log)"), "Raw scores", "DDM"),
  )

# Simple Slopes -----------------------------------------------------------
secondary_simple_slopes <- 
  secondary_unp_flanker %>% 
  map_df(function(multiverse){
    
    dv = multiverse$specifications %>% filter(spec_var == "dv_type") %>% pull(spec_expr)
    
    if(dv %in% c("rt_raw", "rt_log")) {
        specs <- 
          multiverse$specifications %>% 
          select(-spec_expr) %>% 
          pivot_wider(names_from = "spec_var", values_from = "name") %>% 
          rename_with(.cols = !spec_number, ~paste0("spec_",.x))
        
        bind_cols(
          specs,
          bind_rows(
            multiverse$simple_slopes[[1]][[3]] %>% 
              rename_with(~c("level","beta","beta_se","beta_lo","beta_hi","t_value","p_value")) %>% 
              mutate(level = ifelse(level == -1, "congruent", "incongruent")), 
            multiverse$simple_slopes[[2]][[3]] %>% 
              rename_with(~c("level","beta","beta_se","beta_lo","beta_hi","t_value","p_value")) %>% 
              mutate(level = ifelse(level == -1, "low", "high")),
          )
        )
    } else {
      specs <- multiverse$specifications
    }
      }) %>%
  mutate(
    dv         = spec_dv_type,
    dv_group   = ifelse(dv %in% c("RT", "RT (log)"), "Raw scores", "DDM")
  
  )

# save data ---------------------------------------------------------------
save(
  secondary_effects_flanker,
  secondary_effects_medians,
  secondary_effects_points,
  secondary_simple_slopes,
  file = here("data", "1_pilot", "2_secondary_analyses", "flanker", "3_multiverse_extracted_effects.Rdata")
)
