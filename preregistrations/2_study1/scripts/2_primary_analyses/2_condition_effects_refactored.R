
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(here)
library(lme4)


# Load data files ---------------------------------------------------------
source(here("preregistrations", "2_study1", "scripts", "custom_functions", "functions_analyses.R"))
load(here("data", "2_study1", "2_cleaned_data.Rdata"))



# Prepare data for analyses -----------------------------------------------
primary_enh_data <- cleaned_data %>%
  select(id, scale_factor, dems_edu, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, counterbalance, 
         vio_comp, matches("^((a|t0|p|sda|rd|interference|rt)_(flanker|flanker_(congruent|incongruent))_(enh|std))")) %>%
  mutate(
    scale_factor             = ifelse(round(scale_factor, 4) == 0.9007, 0, 1),
    rtdiff_flanker_std       = rt_flanker_congruent_std - rt_flanker_incongruent_std,
    rtdiff_flanker_enh       = rt_flanker_congruent_enh - rt_flanker_incongruent_enh
    # vio_comp_c               = scale(vio_comp, scale = F) %>% as.numeric
  ) %>%
  # Long format for analyses
  pivot_longer(matches("^((a|t0|p|sda|rd|interference|rtdiff)_flanker_(enh|std))"),
               names_to = c(".value", "condition"),
               names_pattern = "(^.*_flanker)(.*)") %>%
  # Sum-code conditions
  mutate(condition = ifelse(condition == "_std", 0, 1)) %>%
  group_by(condition) %>%
  # Calculate standardized dv measures
  mutate(across(matches("(a|t0|p|sda|rd|interference|rtdiff)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
  ) %>%
  ungroup()


primary_deg_data <- cleaned_data %>%
  select(id, scale_factor, dems_edu, fullscreenenter, fullscreenexit, attention_interrupt_sum, att_noise, counterbalance, 
         vio_comp, matches("^((a|t0|p|sda|rd|interference|rt)_(flanker|flanker_(congruent|incongruent))_(deg|std))")) %>%
  mutate(
    scale_factor             = ifelse(round(scale_factor, 4) == 0.9007, 0, 1),
    rtdiff_flanker_std       = rt_flanker_congruent_std - rt_flanker_incongruent_std,
    rtdiff_flanker_deg       = rt_flanker_congruent_deg - rt_flanker_incongruent_deg
    # vio_comp_c               = scale(vio_comp, scale = F) %>% as.numeric
  ) %>%
  # Long format for analyses
  pivot_longer(matches("^((a|t0|p|sda|rd|interference|rtdiff)_flanker_(deg|std))"),
               names_to = c(".value", "condition"),
               names_pattern = "(^.*_flanker)(.*)") %>%
  # Sum-code conditions
  mutate(condition = ifelse(condition == "_std", 0, 1)) %>%
  group_by(condition) %>%
  # Calculate standardized dv measures
  mutate(across(matches("(a|t0|p|sda|rd|interference|rtdiff)_flanker"), 
                function(x) {scale(x) %>% as.numeric}, 
                .names = "{.col}_z")
  ) %>%
  ungroup()




# Multiverse for enhanced condition ---------------------------------------

## Filters ----
primary_enh_filter_grid <- 
  create_filter_grid(
    .df = primary_enh_data,
    scale_factor == 1,
    fullscreenenter == 1,
    attention_interrupt_sum < 1,
    att_noise %in% c(0,1,2)
  )

## Dependent variables ----
primary_enh_var_grid <- 
  create_var_grid(
    .df = primary_enh_data,
    dv = c(p_flanker, t0_flanker, a_flanker, interference_flanker, rtdiff_flanker)
  )

## Model specification ----
primary_enh_mod_grid <- 
  create_model_grid(
    lmer({dv} ~ vio_comp_c * condition + (1|id))
  )

## Preprocessing steps ----
primary_enh_preprocess <- 
  create_preprocess(
    mutate(vio_comp_c = scale(vio_comp, scale = F) |> as.numeric())
  )

primary_enh_postprocess <- NULL

## Create full grid ----
primary_enh_full_grid <- 
  combine_all_grids(
    .df            = primary_enh_data,
    filter_grid    = primary_enh_filter_grid, 
    var_grid       = primary_enh_var_grid,
    model_grid     = primary_enh_mod_grid,
    preprocessing  = primary_enh_preprocess,
    postprocessing = primary_enh_postprocess
    )


multiverse <- run_multiverse(
  .grid = primary_enh_full_grid,
  save_model = FALSE
)



report_universe_console(multiverse, 50)

# Unpack multiverse -------------------------------------------------------

unpacked_multiverse <- multiverse %>%
  unnest(c(filters, variables, lmer)) %>%
  unnest(lmer_result_tidy) %>%
  filter(term %in% c("vio_comp_c", "vio_comp_c:condition"))

# Proportion p-values
unpacked_multiverse %>%
  group_by(dv) %>%
  summarise(
    median_beta = median(estimate),
    p_prop = sum(p.value < .05)/n()*100)



# Multiverse for degraded condition ---------------------------------------


## Filters ----
primary_deg_filter_grid <- 
  create_filter_grid(
    .df = primary_deg_data,
    scale_factor == 1,
    fullscreenenter == 1,
    attention_interrupt_sum < 1,
    att_noise %in% c(0,1,2)
  )

## Dependent variables ----
primary_deg_var_grid <- 
  create_var_grid(
    .df = primary_deg_data,
    dv = c(p_flanker, t0_flanker, a_flanker, interference_flanker, rtdiff_flanker)
  )

## Model specification ----
primary_deg_mod_grid <- 
  create_model_grid(
    lmer({dv} ~ vio_comp_c * condition + (1|id))
  )

## Preprocessing steps ----
primary_deg_preprocess <- 
  create_preprocess(
    mutate(vio_comp_c = scale(vio_comp, scale = F) |> as.numeric())
  )

primary_deg_postprocess <- NULL

## Create full grid ----
primary_deg_full_grid <- 
  combine_all_grids(
    .df            = primary_deg_data,
    filter_grid    = primary_deg_filter_grid, 
    var_grid       = primary_deg_var_grid,
    model_grid     = primary_deg_mod_grid,
    preprocessing  = primary_deg_preprocess,
    postprocessing = primary_deg_postprocess
  )


multiverse_deg <- run_multiverse(
  .grid = primary_deg_full_grid,
  save_model = FALSE
)


# Unpack multiverse -------------------------------------------------------

unpacked_multiverse_deg <- multiverse_deg %>%
  unnest(c(filters, variables, lmer)) %>%
  unnest(lmer_result_tidy) %>%
  filter(term %in% c("vio_comp_c", "vio_comp_c:condition"))

# Proportion p-values
unpacked_multiverse %>%
  group_by(dv) %>%
  summarise(
    median_beta = median(estimate),
    p_prop = sum(p.value < .05)/n()*100)
