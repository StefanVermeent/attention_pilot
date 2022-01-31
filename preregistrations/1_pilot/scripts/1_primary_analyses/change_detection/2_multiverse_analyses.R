# Setup -------------------------------------------------------------------
# Libraries
library(tidyverse)
library(lmerTest)
library(broom)
library(tidymodels)
library(ggeffects)
library(interactions)
library(effectsize)
library(furrr)
library(toc)

source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_analyses.R"))


# Data
load(here("data", "1_pilot", "multiverse", "change_detection", "multiverse_objects.Rdata"))

plan("multisession")

tic()
primary_vio_change <- 
  multi_data_list %>%
  future_map(function(x) {
    
    spec_number <- unique(x$specifications$spec_number)
    
    data <- x$data_analysis
    dv <- x$specifications %>% filter(spec_var == "dv_type") %>% pull(spec_expr)
    
    formula <- as.formula(str_glue("{dv} ~ violence_composite_c"))
    
    # Model object
    mod      <- lm(formula, data=data)
    # Tidy Model 
    mod_tidy <- broom::tidy(mod) %>% rename_all(~paste0("mod_",.))
    # Standardized coefficients
    mod_std <- standardize_parameters(mod)
    
    results <- list(
      n                 = nrow(data),
      n_model           = nrow(mod$model),
      data_analysis     = data,
      # data_model        = mod$model,
      specifications    = x$specifications,
      model             = mod,
      model_tidy        = mod_tidy,
      model_std_effects = mod_std 
    )

    message("dataset ", spec_number, " analyzed")
    results
  })
  toc()
  
  
save(primary_vio_change, file=here("data", "1_pilot", "multiverse", "change_detection", "2_multiverse_results.Rdata"))
