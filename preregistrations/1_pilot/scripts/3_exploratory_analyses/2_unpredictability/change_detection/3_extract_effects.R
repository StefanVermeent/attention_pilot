# Setup -------------------------------------------------------------------
# Libraries
library(tidyverse)

# Load results
load(here("data", "1_pilot", "3_exploratory_analyses", "change_detection", "2_multiverse_results.Rdata")) 

# Raw effect size data ----------------------------------------------------
exploratory_effects_change <- 
  exploratory_change %>% 
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
        iv         = spec_iv_type,
        dv         = spec_dv_type,
        iv_group   = case_when(
          iv %in% c("Fighting", "Violence") ~ "Violence",
          iv %in% c("CHAOS", "Subjective unpredictability", "Objective unpredictability") ~ "Unpredictability",
          iv %in% c("Subjective SES", "Objective SES", "Poverty composite") ~ "Poverty"
        ),
        dv_group   = ifelse(dv %in% c("RT", "RT (log)", "Accuracy"), "Raw scores", "DDM"),
        spec_rank  = as.numeric(fct_reorder(as_factor(spec_number), mod_std_coefficient)),
        median_dbl = median(mod_std_coefficient),
        median_chr = median_dbl %>% round(2) %>% as.character,
        pval_prop  = (sum(mod_p.value < .05)/(768/6)) * 100
      ) %>% 
      ungroup() %>% 
      mutate(
        mod_sig        = case_when(mod_p.value <  .05 & mod_std_coefficient > 0 ~ "pos-sig",
                                   mod_p.value <  .05 & mod_std_coefficient < 0 ~ "neg-sig",
                                   mod_p.value >= .05 ~ "non"),
        mod_term_num   = ifelse(str_detect(mod_term, "Intercept"), -1, 0),
        mod_term_label = ifelse(str_detect(mod_term, "Intercept"),"Intercept", iv),
        mod_term_group = factor(mod_term_num,
                                levels = c(-1,0), 
                                labels = c("Intercept","Main Effect")),
        mod_term_unique = case_when(
          mod_term == "fighting_mean_c" ~ "fighting", 
          mod_term == "violence_mean_c" ~ "vio",
          mod_term == "chaos_mean_c" ~ "chaos",
          mod_term == "unpredictability_subj_c" ~ "unp_subj",
          mod_term == "unpredictability_obj_c" ~ "unp_obj",
          mod_term == "ses_subj_mean_c" ~ "ses_subj",
          mod_term == "ses_obj_mean_c" ~ "ses_obj",
          mod_term == "poverty_composite_c" ~ "pov",
          TRUE ~ mod_term_label),
        mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num),
        pval_prop      = paste0(round(pval_prop,2),"% of ps"," < .05"),
        pval_prop      = fct_reorder(pval_prop, mod_term_num),
        )

# Medians -----------------------------------------------------------------
exploratory_effects_medians <- 
  exploratory_effects_change %>% 
  select(dv, dv_group, iv, iv_group, contains("_term"), contains("median_")) %>% 
  distinct()

exploratory_effects_points <- 
  exploratory_change %>% 
  map_df(function(multiverse){
    specs <- multiverse$specifications %>% 
      select(-spec_expr) %>% 
      pivot_wider(names_from = "spec_var", values_from = "name") %>% 
      rename_with(.cols = !spec_number, ~paste0("spec_",.x))
    
    bind_cols(multiverse$model_effects, n = multiverse$n, specs) %>% tibble()
  }) %>% 
  left_join(
    exploratory_effects_change %>% 
      filter(!str_detect(mod_term_label, "Intercept")) %>% 
      select(spec_number, mod_sig)
  )

# save data ---------------------------------------------------------------
save(
  exploratory_effects_change,
  exploratory_effects_medians,
  exploratory_effects_points,
  file = here("data", "1_pilot", "3_exploratory_analyses", "change_detection", "3_multiverse_extracted_effects.Rdata")
)
