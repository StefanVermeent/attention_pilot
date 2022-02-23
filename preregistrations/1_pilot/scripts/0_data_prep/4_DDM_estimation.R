
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

# Pre-treatment
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

# Write individual datafiles
write_DDM_files(data = change_DDM_setup, vars = c("rt", "correct"), task = "change") 

## Model 1: 7-parameter model: v, a, t0, z, and inter-trial variability parameters ----

# Set Fast-DM settings

fast_dm_settings(task = "change", 
                 model_version = "_mod1",
                 method = "ml",
                 d = "",
                 zr = "",
                 szr = "", sv = "", st0 = "",
                 depend = "", 
                 format = "TIME RESPONSE")

# Compute DDM parameters
execute_fast_dm(task = "change", model_version = "_mod1")

# Read DDM results
change_DDM_results_mod1 <- read_DDM(task = "change", model_version = "_mod1") %>%
  left_join(change_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (7 * log(n_trials)))

change_bic_mod1 <- mean(change_DDM_results_mod1$bic, na.rm = TRUE)


# Model 2: 5-parameter model (inter-trial variability parameters fixed to 0) ----

# #Set Fast-DM settings

fast_dm_settings(task = "change", 
                 model_version = "_mod2",
                 method = "ml",
                 zr = "",
                 d = "",
                 szr = 0, sv = 0, st0 = 0,
                 depend = "", 
                 format = "TIME RESPONSE")

# Compute DDM parameters
execute_fast_dm(task = "change", model_version = "_mod2")

# Read DDM results
change_DDM_results_mod2 <- read_DDM(task = "change", model_version = "_mod2") %>%
left_join(change_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (4 * log(n_trials)))

change_bic_mod2 <- mean(change_DDM_results_mod2$bic, na.rm = TRUE)


## Model 3: 4-parameter model (z fixed to 0.5); a varies across conditions ----

# Set Fast-DM settings

fast_dm_settings(task = "change", 
                 model_version = "_mod3",
                 method = "ml",
                 zr = 0.5,
                 d = "",
                 szr = 0, sv = 0, st0 = 0,
                 format = "TIME RESPONSE")

# Compute DDM parameters
execute_fast_dm(task = "change", model_version = "_mod3")

# Read DDM results
change_DDM_results_mod3 <- read_DDM(task = "change", model_version = "_mod3") %>%
  left_join(change_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

change_bic_mod3 <- mean(change_DDM_results_mod3$bic, na.rm = TRUE)


## Model 4: 3-parameter model ----

# Set Fast-DM settings

fast_dm_settings(task = "change", 
                 model_version = "_mod4",
                 method = "ml",
                 zr = 0.5,
                 d = 0,
                 szr = 0, sv = 0, st0 = 0,
                 format = "TIME RESPONSE")

# Compute DDM parameters
execute_fast_dm(task = "change", model_version = "_mod4")

# Read DDM results
change_DDM_results_mod4 <- read_DDM(task = "change", model_version = "_mod4") %>%
  left_join(change_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

change_bic_mod4 <- mean(change_DDM_results_mod4$bic, na.rm = TRUE)


glue(
"Change Detection Model Versions:

The BIC value of model 1 is {change_bic_mod1}
The BIC value of model 2 is {change_bic_mod2}
The BIC value of model 3 is {change_bic_mod3}
The BIC value of model 4 is {change_bic_mod4}
")

### Remove individual files from folder
remove_DDM_files()

## Change model fit ----

## Simulate data ----

change_sim_params <- simulate_DDM_parameters(data = change_DDM_results_mod4, nsim = 5000)

pmap(change_sim_params, function(n,a,t0,z,v, row) {
  rwiener(n,a,t0,z,v) %>%
    as_tibble() %>%
  mutate(resp = ifelse(resp == "upper", 1, 0)) %>%
    write_delim(file = here("data", "1_pilot", "DDM", str_c("par_sim", row, ".dat")), col_names = FALSE)
})

execute_fast_dm(task = "change", model_version = "_mod4")

### Read simulation results ----
change_simulation_fit_vector <- read_DDM(task = "change", model_version = "_mod4") %>%
  select(fit) %>%
  pull() 


change_DDM_results_mod4 %<>%
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

## Write individual datafiles ----
write_DDM_files(data = cueing_DDM_setup, vars = c("rt", "correct", "condition"), task = "cueing") 


## Model overview ----
## 1. 8-parameter model; a varies across conditions
## 2. 5-parameter model (inter-trial variability parameters fixed to 0); a varies across conditions
## 3. 4-parameter model (z fixed to 0.5); a varies across conditions
## 4. 3-parameter model (d fixed to 0); a varies across conditions
## 5. 8-parameter model; a is constrained to be equal across conditions
## 6. 5-parameter model (inter-trial variability parameters fixed to 0); a is constrained to be equal across conditions
## 7. 4-parameter model (d fixed to 0); a is constrained to be equal across conditions
## 8. 3-parameter model (z fixed to 0.5); a is constrained to be equal across conditions


## Model 1: 8-parameter model; a varies across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod1",
                 method = "ml",
                 d = "",
                 zr = "",
                 szr = "", sv = "", st0 = "",
                 depend = c("depends v condition", "depends a condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod1")

# Read DDM results
cueing_DDM_results_mod1 <- read_DDM(task = "cueing", model_version = "_mod1") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod1 <- mean(cueing_DDM_results_mod1$bic, na.rm = TRUE)



# Model 2: 5-parameter model (inter-trial variability parameters fixed to 0); a varies across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod2",
                 method = "ml",
                 d = "",
                 zr = "",
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends a condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod2")

# Read DDM results
cueing_DDM_results_mod2 <- read_DDM(task = "cueing", model_version = "_mod2") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod2 <- mean(cueing_DDM_results_mod2$bic, na.rm = TRUE)



## Model 3: 4-parameter model (z fixed to 0.5); a varies across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod3",
                 method = "ml",
                 d = "",
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends a condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod3")

# Read DDM results
cueing_DDM_results_mod3 <- read_DDM(task = "cueing", model_version = "_mod3") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod3 <- mean(cueing_DDM_results_mod3$bic, na.rm = TRUE)


## Model 4: 3-parameter model (d fixed to 0); a varies across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod4",
                 method = "ml",
                 d = 0,
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends a condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod4")

# Read DDM results
cueing_DDM_results_mod4 <- read_DDM(task = "cueing", model_version = "_mod4") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod4 <- mean(cueing_DDM_results_mod4$bic, na.rm = TRUE)



## Model 5: 8-parameter model; a is fixed to be equal across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod5",
                 method = "ml",
                 d = "",
                 zr = "",
                 szr = "", sv = "", st0 = "",
                 depend = c("depends v condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod5")

# Read DDM results
cueing_DDM_results_mod5 <- read_DDM(task = "cueing", model_version = "_mod5") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod5 <- mean(cueing_DDM_results_mod5$bic, na.rm = TRUE)



# Model 6: 5-parameter model (inter-trial variability parameters fixed to 0); a is fixed to be equal across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod6",
                 method = "ml",
                 d = "",
                 zr = "",
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod6")

# Read DDM results
cueing_DDM_results_mod6 <- read_DDM(task = "cueing", model_version = "_mod6") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod6 <- mean(cueing_DDM_results_mod6$bic, na.rm = TRUE)



## Model 7: 4-parameter model (z fixed to 0.5); a is fixed to be equal across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod7",
                 method = "ml",
                 d = "",
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod7")

# Read DDM results
cueing_DDM_results_mod7 <- read_DDM(task = "cueing", model_version = "_mod7") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod7 <- mean(cueing_DDM_results_mod7$bic, na.rm = TRUE)


## Model 8: 3-parameter model (d fixed to 0); a is fixed to be equal across conditions ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod8",
                 method = "ml",
                 d = 0,
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod8")

# Read DDM results
cueing_DDM_results_mod8 <- read_DDM(task = "cueing", model_version = "_mod8") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

cueing_bic_mod8 <- mean(cueing_DDM_results_mod8$bic, na.rm = TRUE)


glue(
"Attention Cueing Model Versions:
The BIC value of model 1 is {cueing_bic_mod1}
The BIC value of model 2 is {cueing_bic_mod2}
The BIC value of model 3 is {cueing_bic_mod3}
The BIC value of model 4 is {cueing_bic_mod4}
The BIC value of model 5 is {cueing_bic_mod5}
The BIC value of model 6 is {cueing_bic_mod6}
The BIC value of model 7 is {cueing_bic_mod7}
The BIC value of model 8 is {cueing_bic_mod8}
")

cueing_DDM_results_mod8 %<>%
  select(
    id,
    cueing_cued_ml_a = a,
    cueing_cued_ml_v = v_cued,
    cueing_cued_ml_t0 = t0_cued,
    cueing_neutral_ml_a = a,
    cueing_neutral_ml_v = v_neutral,
    cueing_neutral_ml_t0 = t0_neutral,
    cueing_ml_fit = fit
  )


# Cueing model fit --------------------------------------------------------
# 
# ## Simulate data ----
# 
# cueing_sim_params <- simulate_DDM_parameters(data = cueing_DDM_results_mod8, nsim = 5000)


save(
  change_DDM_results_mod1, change_bic_mod1,
  change_DDM_results_mod2, change_bic_mod2,
  change_DDM_results_mod3, change_bic_mod3,
  change_DDM_results_mod4, change_bic_mod4,
  
  cueing_DDM_results_mod1, cueing_bic_mod1,
  cueing_DDM_results_mod2, cueing_bic_mod2,
  cueing_DDM_results_mod3, cueing_bic_mod3,
  cueing_DDM_results_mod4, cueing_bic_mod4,
  cueing_DDM_results_mod5, cueing_bic_mod5,
  cueing_DDM_results_mod6, cueing_bic_mod6,
  cueing_DDM_results_mod7, cueing_bic_mod7,
  cueing_DDM_results_mod8, cueing_bic_mod8,
  file = here("data", "1_pilot", "1_DDM_objects.Rdata"))
