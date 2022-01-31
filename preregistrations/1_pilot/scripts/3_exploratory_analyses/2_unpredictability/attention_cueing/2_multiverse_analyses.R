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
load(here("data", "1_pilot", "multiverse", "attention_cueing", "multiverse_objects.Rdata"))

cores = parallel::detectCores()
plan("multisession", workers = cores-4)

tic()
primary_vio_cueing <- 
  multi_data_list %>%
  future_map(function(x) {
    
    spec_number <- unique(x$specifications$spec_number)
    
    data <- x$data_analysis
    dv <<- x$specifications %>% filter(spec_var == "dv_type") %>% pull(spec_expr)
  
   #  formula <- as.formula(str_glue("{dv} ~ violence_composite_c * condition_sum + (1|id)"))
    
     # Model object
     mod      <- lmer(str_c(dv, " ~ violence_composite_c * condition_sum + (1|id)"), data=data)

     # Tidy Model
     mod_tidy <- broom.mixed::tidy(mod) %>% rename_all(~paste0("mod_",.))

     #Standardized coefficients
     mod_std <- standardize_parameters(mod)

     # Create a data.frame with predicted effects at high and low predictor value for each task type
     mod_effects <- ggpredict(mod, terms = c("violence_composite_c [-1,1]", "condition_sum [-1,1]"))

     ss_task <- sim_slopes(mod, pred = "violence_composite_c", modx = "condition_sum", modx.values = c(-1,1))
     ss_unp  <- sim_slopes(mod, pred = "condition_sum", modx = "violence_composite_c", modx.values = c(-1,1))
    
     results <- list(
       n                 = x$n,
       n_model           = nrow(mod@frame),
       n_model_obs       = mod@frame %>% distinct(id) %>% nrow,
       data_analysis     = data,
       data_model        = mod@frame,
       specifications    = x$specifications,
       model             = mod,
       model_tidy        = mod_tidy,
       model_std_effects = mod_std,
       model_effects     = mod_effects,
       simple_slopes     = list(task = ss_task, unp = ss_unp)
     )
     
     message("dataset ", spec_number, " analyzed")
     results
  })
toc()


save(primary_vio_cueing, file=here("data", "1_pilot", "multiverse", "attention_cueing", "2_multiverse_results.Rdata"))
