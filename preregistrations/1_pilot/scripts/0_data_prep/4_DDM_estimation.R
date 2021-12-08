
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(here)
library(RWiener)
library(mvtnorm)
library(data.table)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "1_task_data_clean.Rdata"))

# DDM helper functions
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_DDM.R"))



# Change Detection Task ---------------------------------------------------

## Pre-treatment ----

change_DDM_setup <- change_data_clean_average %>%
  select(change_data_long) %>%
  unnest(change_data_long) %>%
  select(id, rt, correct, condition) %>%
  mutate(rt = rt / 1000) 

# Test whether RTs significantly differ between 'same' and 'different' trials
change_DDM_setup %>%
  group_by(id, condition) %>%
  summarise(mean_rt = mean(log(rt), na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = "condition", values_from = "mean_rt") %>%
  do(broom::tidy(t.test(.$same, .$different, data=., paired=T)))

## Set Fast-DM settings ----

fast_dm_settings(task = "change", 
                 method = "ml", 
                 depend = "", 
                 format = "TIME RESPONSE")

## Write individual datafiles ----
write_DDM_files(data = change_DDM_setup, vars = c("rt", "correct"), task = "change") 

## Compute DDM parameters ----
execute_fast_dm(task = "change")

## Read DDM results ----
change_DDM_results <- read_DDM(task = "change")

## Remove individual files from folder
remove_DDM_files()

## Change model fit ----

## Simulate data ----

change_sim_params <- simulate_DDM_parameters(data = change_DDM_results, nsim = 5000)

pmap(change_sim_params, function(n,a,t0,z,v, row) {
  rwiener(n,a,t0,z,v) %>%
    as_tibble() %>%
  mutate(resp = ifelse(resp == "upper", 1, 0)) %>%
    write_delim(file = here("data", "1_pilot", "DDM", str_c("par_sim", row, ".dat")), col_names = FALSE)
})

execute_fast_dm(task = "change")

### Read simulation results ----
change_simulation_fit_vector <- read_DDM(task = "change") %>%
  select(fit) %>%
  pull() 


change_DDM_results %<>%
  mutate(change_ml_bad_fit = ifelse(fit < quantile(change_simulation_fit_vector, .05)[[1]], TRUE, FALSE)) %>%
  select(
    id,
    change_ml_a = a,
    change_ml_v = v,
    change_ml_t0 = t0,
    change_ml_fit = fit,
    change_ml_bad_fit
  )

# Attention Cueing Task ---------------------------------------------------

## Pre-treatment ----

cueing_DDM_setup <- cueing_data_clean_average %>%
  select(cueing_data_long) %>%
  unnest(cueing_data_long) %>%
  select(id, rt, correct, condition) %>%
  mutate(rt = rt / 1000) 


# Model 1: All parameters dependent on condition --------------------------

fast_dm_settings(task = "cueing", 
                 model_version = "_full_independent", 
                 method = "ml", 
                 depend = c("depends v condition", "depends a condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

## Write individual datafiles ----
write_DDM_files(data = cueing_DDM_setup, vars = c("rt", "correct", "condition"), task = "cueing") 

## Compute DDM parameters ----
execute_fast_dm(task = "cueing", model_version = "_full_independent")

## Read DDM results ----
cueing_DDM_results_mod1 <- read_DDM(task = "cueing", model_version = "_full_independent")

## Remove individual files from folder
remove_DDM_files()



# Model 2: a is fixed across conditions -----------------------------------

fast_dm_settings(task = "cueing", 
                 model_version = "_v_t0_independent", 
                 method = "ml", 
                 depend = c("depends v condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

## Write individual datafiles ----
write_DDM_files(data = cueing_DDM_setup, vars = c("rt", "correct", "condition"), task = "cueing") 

## Compute DDM parameters ----
execute_fast_dm(task = "cueing", model_version = "_v_t0_independent")

## Read DDM results ----
cueing_DDM_results_mod2 <- read_DDM(task = "cueing", model_version = "_v_t0_independent") %>%
  mutate(a_neutral = a, a_cued = a) %>%
  select(-a)

## Remove individual files from folder
remove_DDM_files()


# Cueing model fit --------------------------------------------------------

## Simulate data ----

cueing_sim_params <- simulate_DDM_parameters(data = cueing_DDM_results_mod2, nsim = 5000)


save(
  change_DDM_results, 
  cueing_DDM_results_mod1, 
  cueing_DDM_results_mod2,
  file = here("data", "1_pilot", "1_DDM_objects.Rdata"))
