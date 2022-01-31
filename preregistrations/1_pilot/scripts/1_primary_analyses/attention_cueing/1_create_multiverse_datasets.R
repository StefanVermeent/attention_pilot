
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(here)

load(here("data", "1_pilot", "2_cleaned_data.Rdata"))

# Transfrom data to long format and prepare for analyses
multiverse_data <- cleaned_data %>%
  select(id, scale_factor, fullscreenenter, fullscreenexit, meta_captcha, attention_interrupt_sum, att_noise, violence_composite,
         contains("cueing"), -cueing_data_long, -starts_with(c("rt_var", "acc_", "event_during")), -contains("_ml_")) %>%
  rename(cueing_neutral_rt_raw = rt_cueing_neutral, cueing_cued_rt_raw = rt_cueing_cued) %>%
  pivot_longer(starts_with("cueing"),
               names_to = c("condition", ".value"),
               names_pattern = "([a-z]*_[a-z]*_)(.*)"
               ) %>%
  mutate(
    condition            = ifelse(condition == "cueing_cued_", "cued", "neutral"),
    condition_sum        = ifelse(condition == "neutral", -1, 1),
    rt_raw               = rt_raw * 1000,
    rt_log               = log(rt_raw)
  )

# arbitrary decision grid -------------------------------------------------

spec_grid <- 
  expand_grid(
    dv_type         = c(0, 1, 2, 3, 4),
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
      "dv_type",         0,           "rt_raw",                                                        "RT",
      "dv_type",         1,           "rt_log",                                                        "RT (log)",
      "dv_type",         2,           "EZ_v",                                                          "Drift rate (v)",
      "dv_type",         3,           "EZ_a",                                                          "Boundary separation (a)",
      "dv_type",         4,           "EZ_t0",                                                         "Non-decision time (t0)",
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
      "outliers",        0,           "< 10",                                                          "Include all scores",
      "outliers",        1,           "< 3.2",                                                         "Exclude scores > 3.2SD",
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
        eval(parse(text = paste(spec_expressions$no_fullscreen))), 
        eval(parse(text = paste(spec_expressions$exit_fullscreen))),
        eval(parse(text = paste(spec_expressions$captcha))),
        eval(parse(text = paste(spec_expressions$interrupted))),
        eval(parse(text = paste(spec_expressions$noise))),
        eval(parse(text = paste0("scale(", spec_expressions$dv_type, ") ", spec_expressions$outliers)))
      ) %>% 
      drop_na(spec_expressions$dv_type) %>%
      mutate(
        violence_composite_c = scale(violence_composite, scale = FALSE) %>% as.numeric,
        mt_dataset = spec_number
      )
    
    
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
save(multi_data_list, file = here("data", "1_pilot", "multiverse", "attention_cueing", "1_multiverse_objects.Rdata"))
