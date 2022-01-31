# Setup -------------------------------------------------------------------
# Libraries
library(tidyverse)
library(lmerTest)
library(here)
library(broom)
library(broom.mixed)
library(tidymodels)
library(ggeffects)
library(interactions)
library(effectsize)
library(furrr)
library(tictoc)

source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_analyses.R"))


# Data
load(here("data", "1_pilot", "2_secondary_analyses", "flanker", "1_multiverse_objects.Rdata"))

cores = parallel::detectCores()
plan("multisession", workers = cores-4)

tic()
secondary_unp_flanker <- 
  multi_data_list %>%
  future_map(function(x) {
    
    spec_number <- unique(x$specifications$spec_number)
    dv <<- x$specifications %>% filter(spec_var == "dv_type") %>% pull(spec_expr)
    
    if(dv %in% c("rt_raw", "rt_log")) {
      
      data_raw <- x$data_analysis$data_raw
      
 
      # Model object
      mod      <- lmer(str_c(dv, " ~ unpredictability_composite_c * condition_sum + (1|id)"), data=data_raw)
      # Tidy Model
      mod_tidy <- broom.mixed::tidy(mod) %>% rename_all(~paste0("mod_",.))
      #Standardized coefficients
      mod_std <- standardize_parameters(mod)
      
      # Create a data.frame with predicted effects at high and low predictor value for each task type
      mod_effects <- ggpredict(mod, terms = c("unpredictability_composite_c [-1,1]", "condition_sum [-1,1]"))
      ss_task <- sim_slopes(mod, pred = "unpredictability_composite_c", modx = "condition_sum", modx.values = c(-1,1))
      ss_unp  <- sim_slopes(mod, pred = "condition_sum", modx = "unpredictability_composite_c", modx.values = c(-1,1))
      
      results <- list(
        n                 = x$n,
        n_model           = nrow(mod@frame),
        n_model_obs       = mod@frame %>% distinct(id) %>% nrow,
        data_analysis     = data_raw,
        data_model        = mod@frame,
        specifications    = x$specifications,
        model             = mod,
        model_tidy        = mod_tidy,
        model_std_effects = mod_std,
        model_effects     = mod_effects,
        simple_slopes     = list(task = ss_task, unp = ss_unp)
      )
    }
    
    if(dv %in% c("a_flanker", "t0_flanker", "p_flanker", "rd_flanker", "sda_flanker", "interference")) {
      
      data_ssp <- x$data_analysis$data_ssp
      
     
      # Model object
      mod      <- lm(str_c(dv, " ~ unpredictability_composite_c"), data=data_ssp)
      # Tidy Model
      mod_tidy <- broom.mixed::tidy(mod) %>% rename_all(~paste0("mod_",.))
      #Standardized coefficients
      mod_std <- standardize_parameters(mod)

      results <- list(
        n                 = nrow(data),
        n_model           = nrow(mod$model),
        data_analysis     = data_ssp,
        specifications    = x$specifications,
        model             = mod,
        model_tidy        = mod_tidy,
        model_std_effects = mod_std
      )
    }
     
     message("dataset ", spec_number, " analyzed")
     results
  })
toc()


save(secondary_unp_flanker, file=here("data", "1_pilot", "2_secondary_analyses", "flanker", "2_multiverse_results.Rdata"))
