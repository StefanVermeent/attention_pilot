
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(here)

load(here("data", "1_pilot", "2_cleaned_data.Rdata"))

multiverse_data <- cleaned_data %>%
  mutate( unpredictability_subj = across(c(matches("unp\\d\\d"), matches("quic\\d\\d"))) %>% rowMeans(., na.rm = TRUE)) %>%
  select(id, scale_factor, fullscreenexit, unpredictability_composite,
         contains("change"), -change_data_long, -starts_with("change_env"),
         -starts_with("rt_var"),
         fighting_mean, violence_mean, chaos_mean, unpredictability_subj, unpredictability_obj,
         ses_subj_mean, ses_obj_mean, poverty_composite) %>%
  mutate(
    rt_change             = rt_change * 1000,
    rt_change_log         = log(rt_change)
  ) %>%
  # Center IVs
  mutate(across(c(fighting_mean, violence_mean, chaos_mean, unpredictability_subj, unpredictability_obj,
                  ses_subj_mean, ses_obj_mean, poverty_composite), ~scale(., scale = F), .names = "{.col}_c"))

# arbitrary decision grid -------------------------------------------------

spec_grid <- 
  expand_grid(
    dv_type         = c(0, 1, 2, 3, 4,5),
    iv_type         = c(0, 1, 2, 3, 4, 5, 6, 7),
    no_resize       = c(0,1),
    no_fullscreen   = c(0,1),
    exit_fullscreen = c(0,1),
    captcha         = c(0,1),
    interrupted     = c(0,1),
    noise           = c(0,1),
    outliers        = c(0,1),
  ) %>%
  rownames_to_column("spec_number") %>%
  mutate(spec_number = as.numeric(spec_number)) %>%
  pivot_longer(!spec_number, names_to = "spec_var", values_to = "spec_value") %>% 
  left_join(
    tribble(
      ~spec_var,         ~spec_value, ~spec_expr,                                                      ~name,
      "dv_type",         0,           "rt_change",                                                     "RT",
      "dv_type",         1,           "rt_change_log",                                                 "RT (log)",
      "dv_type",         2,           "acc_change",                                                    "Accuracy",
      "dv_type",         3,           "change_ml_v",                                                   "Drift rate (v)",
      "dv_type",         4,           "change_ml_a",                                                   "Boundary separation (a)",
      "dv_type",         5,           "change_ml_t0",                                                  "Non-decision time (t0)",
      "iv_type",         0,           "fighting_mean_c",                                               "Fighting",
      "iv_type",         1,           "violence_mean_c",                                               "Violence",
      "iv_type",         2,           "chaos_mean_c",                                                  "CHAOS",
      "iv_type",         3,           "unpredictability_subj_c",                                       "Subjective unpredictability",
      "iv_type",         4,           "unpredictability_obj_c",                                        "Objective unpredictability",
      "iv_type",         5,           "ses_subj_mean_c",                                               "Subjective SES",
      "iv_type",         6,           "ses_obj_mean_c",                                                "Objective SES",
      "iv_type",         7,           "poverty_composite_c",                                           "Poverty composite",
      "no_resize",       0,           "scale_factor > 0",                                              "All scalers",
      "no_resize",       1,           "round(scale_factor, 4) != '0.3081'",                            "Correct scalers Only",
      "no_fullscreen",   0,           "fullscreenenter %in% c(0,1)",                                   "Include no fullscr. enter",
      "no_fullscreen",   1,           "fullscreenenter == 1",                                          "Exclude no fullscr. enter",
      "exit_fullscreen", 0,           "fullscreenexit %in% c(0,1)",                                    "Include fullscr. exit",
      "exit_fullscreen", 1,           "fullscreenexit == 0",                                           "Exclude fullscr. exit",
      "captcha",         0,           "meta_captcha > 0",                                              "All captcha scores",
      "captcha",         1,           "meta_captcha > 0.4",                                            "Captcha score > 0.4",
      "interrupted",     0,           "attention_interrupt_sum %in% c(0,1,2)",                         "All interruptions",
      "interrupted",     1,           "attention_interrupt_sum < 1",                                   "No extreme interruptions",
      "noise",           0,           "att_noise %in% c(0,1,2,3,4)",                                   "All noise levels",
      "noise",           1,           "att_noise %in% c(0,1,2)",                                       "No high noise levels",
      "outliers",        0,           "< 10",                                                          "Include all RTs",
      "outliers",        1,           "< 3.2",                                                         "Exclude RTs > 3.2SD",
    ),
    by = c("spec_var", "spec_value")
  )

# Multiverse dataset list -----------------------------------------------------
## This loop applies specifications from the grid from above to the data
## We proceed in 3 steps
### 1. Split the grid by specification
### 2. Apply the expression associated with the specifications for each variable to the data
### 3. Arrange the data in long form for mixed-models (if mixed models are used)
### 4. Let the user know the data were created and return a list of the data in long and wide form, the specs, and the n

## Datasets in wide format ----

multi_data_list <-
  # 1
  spec_grid %>% 
  split(.$spec_number) %>% 
  map(function(x){
    spec_number <- unique(x$spec_number)
    spec_expressions <- 
      x %>% 
      select(spec_var, spec_expr) %>% 
      pivot_wider(names_from = "spec_var", values_from = "spec_expr")
    
    # 2
    data <-
      multiverse_data %>% 
      filter(
        eval(parse(text = paste(spec_expressions$no_resize))), 
        eval(parse(text = paste(spec_expressions$exit_fullscreen))),
      ) %>% 
      drop_na(spec_expressions$dv_type, spec_expressions$iv_type) %>%
      mutate(mt_dataset = spec_number)
      
    
    results <- list(
      n              = nrow(data),
      data_analysis  = data,
      specifications = x %>% select(-spec_value)
    )
    
    # 4
    message("dataset ", spec_number, " created")
    results
  })
  
# Save environment --------------------------------------------------------
save(multi_data_list, file = here("data", "1_pilot", "3_exploratory_analyses", "change_detection", "1_multiverse_objects.Rdata"))
