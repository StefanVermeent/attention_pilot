
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(here)
library(RWiener)
library(data.table)


# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "1_task_data_clean.Rdata"))

# DDM helper functions
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_DDM.R"))



# 1. Standard DDM Models --------------------------------------------------

# 1.1. Change Detection Task ---------------------------------------------------

## Prepare data ----
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


## Model 2: 5-parameter model (inter-trial variability parameters fixed to 0) ----

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

change_DDM_results_mod4 %<>%
  select(
    id,
    change_ml_a = a,
    change_ml_v = v,
    change_ml_t0 = t0
  )


## Model 5: KS estimation with st0 freely esimated (not preregistered) ----

# Set Fast-DM settings

fast_dm_settings(task = "change", 
                 model_version = "_mod5",
                 method = "ks",
                 d = 0,
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = "",
                 format = "TIME RESPONSE")

# Compute DDM parameters
execute_fast_dm(task = "change", model_version = "_mod5")

# Read DDM results
change_DDM_results_mod5 <- read_DDM(task = "change", model_version = "_mod5") %>%
  left_join(change_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (7 * log(n_trials)))

change_bic_mod5 <- mean(change_DDM_results_mod5$bic, na.rm = TRUE)

change_DDM_results_mod5 %<>%
  select(
    id,
    change_ks_a = a,
    change_ks_v = v,
    change_ks_t0 = t0,
    change_ks_st0 = st0
  )



## Remove individual files from folder ----
remove_DDM_files()

## Model fit ----

glue(
  "Change Detection Model Versions:

The BIC value of model 1 is {change_bic_mod1}
The BIC value of model 2 is {change_bic_mod2}
The BIC value of model 3 is {change_bic_mod3}
The BIC value of model 4 is {change_bic_mod4}
The BIC value of model 5 is {change_bic_mod5}
")



# 1.2. Attention Cueing Task ---------------------------------------------------

## Prepare data ----

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
## 9. KS estimation with st0 freely esimated (not preregistered)


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



## Model 2: 5-parameter model (inter-trial variability parameters fixed to 0); a varies across conditions ----

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



## Model 6: 5-parameter model (inter-trial variability parameters fixed to 0); a is fixed to be equal across conditions ----

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

cueing_DDM_results_mod8 %<>%
  select(
    id,
    cueing_ml_a = a,
    cueing_cued_ml_v = v_cued,
    cueing_cued_ml_t0 = t0_cued,
    cueing_neutral_ml_v = v_neutral,
    cueing_neutral_ml_t0 = t0_neutral,
    cueing_ml_fit = fit
  )


## Model 9: KS estimation with st0 freely esimated (not preregistered) ----

fast_dm_settings(task = "cueing", 
                 model_version = "_mod9",
                 method = "ks",
                 d = 0,
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = "",
                 depend = c("depends v condition", "depends t0 condition", "depends st0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "cueing", model_version = "_mod9")

# Read DDM results
cueing_DDM_results_mod9 <- read_DDM(task = "cueing", model_version = "_mod9") %>%
  left_join(cueing_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (7 * log(n_trials)))

cueing_bic_mod9 <- mean(cueing_DDM_results_mod9$bic, na.rm = TRUE)

cueing_DDM_results_mod9 %<>%
  select(
    id,
    cueing_ks_a = a,
    cueing_cued_ks_v = v_cued,
    cueing_cued_ks_t0 = t0_cued,
    cueing_neutral_ks_v = v_neutral,
    cueing_neutral_ks_t0 = t0_neutral,
    cueing_ks_fit = fit
  )

## Remove individual files from folder ----
remove_DDM_files()




# 1.3 Flanker Task (not preregistered) ------------------------------------

## Prepare data ----

flanker_DDM_setup <- flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  unnest(flanker_data_long) %>%
  select(id, rt, correct, congruency) %>%
  mutate(rt = rt / 1000) 

## Write individual datafiles ----
write_DDM_files(data = flanker_DDM_setup, vars = c("rt", "correct", "congruency"), task = "flanker") 


## Model 1: 3-parameter model (d fixed to 0) ----

fast_dm_settings(task = "flanker", 
                 model_version = "_mod1",
                 method = "ml",
                 d = 0,
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = 0,
                 depend = c("depends v condition", "depends t0 condition"), 
                 format = "TIME RESPONSE condition")

# Compute DDM parameters
execute_fast_dm(task = "flanker", model_version = "_mod1")

# Read DDM results
flanker_DDM_results_mod1 <- read_DDM(task = "flanker", model_version = "_mod1") %>%
  left_join(flanker_DDM_setup %>% group_by(id) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (3 * log(n_trials))) 

flanker_bic_mod1 <- mean(flanker_DDM_results_mod1$bic, na.rm = TRUE)

flanker_DDM_results_mod1 %<>%
  select(
    id,
    flanker_ml_a = a,
    flanker_con_ml_v = v_congruent,
    flanker_con_ml_t0 = t0_congruent,
    flanker_incon_ml_v = v_incongruent,
    flanker_incon_ml_t0 = t0_incongruent,
    flanker_ml_fit = fit
  )


## Model 2: KS estimation ----

fast_dm_settings(task = "flanker", 
                 model_version = "_mod2",
                 method = "ks",
                 d = 0,
                 zr = 0.5,
                 szr = 0, sv = 0, st0 = "",
                 depend = c("depends v congruency", "depends t0 congruency"), 
                 format = "TIME RESPONSE congruency")

# Compute DDM parameters
execute_fast_dm(task = "flanker", model_version = "_mod2")

# Read DDM results
flanker_DDM_results_mod2 <- read_DDM(task = "flanker", model_version = "_mod2") %>%
  left_join(flanker_DDM_setup %>% group_by(id, congruency) %>% summarise(n_trials = n())) %>%
  mutate(bic = (-2 * fit) + (7 * log(n_trials)))

flanker_bic_mod2 <- mean(flanker_DDM_results_mod2$bic, na.rm = TRUE)

flanker_DDM_results_mod2 %<>%
  select(
    id,
    flanker_ks_a = a,
    flanker_con_ks_v = v_congruent,
    flanker_con_ks_t0 = t0_congruent,
    flanker_incon_ks_v = v_incongruent,
    flanker_incon_ks_t0 = t0_incongruent,
    flanker_ks_fit = fit
  )

## Remove individual files from folder ----
remove_DDM_files()


## Model fit ----
glue(
  "Flanker Model Versions:
The BIC value of model 1 is {flanker_bic_mod1}
")



# 2. SSP Model (Flanker only) ---------------------------------------------

# Tidy Flanker Data -------------------------------------------------------

flanker_SDDM_setup <- flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x) {
      
      x %>%
        mutate(rt=rt/1000) %>%
        rename(subject = id, accuracy = correct) %>%
        select(subject, congruency, accuracy, rt)
    }) 
  )


# In case processing was interrupted, we check which subjects were already processed and skip them
processed_files <- list.files(here("data", "1_pilot")) %>%
  str_subset("ssp_fit") %>%
  str_replace_all("ssp_fit_|.csv", "") %>%
  as.numeric()

flanker_SDDM_setup %<>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x){
      
      if(unique(x$subject) %in% processed_files) {
        return(NA)
      } else {
        return(x)
      }
    })
  ) %>%
  filter(!is.na(flanker_data_long))

# Fit SSP Model -----------------------------------------------------------

# Number of cores for parallel processing
cores <- parallel::detectCores()

# set random seed so user can reproduce simulation outcome
set.seed(42)

# during the fit, how many sets of starting parameters should be used?
n_start_parms <- 50

# what should the variance across starting parameters be?
var_start_parms <- 20

# how many trials to simulate during each iteration of the fit routine whilst
# exploring multiple starting parameters?
n_first_pass <- 1000

# how many trials to simulate during the final fit routine?
n_final_pass <- 50000


# Initiate parallel processing
plan(multisession, workers = cores - 2)

# Find best starting parameters for each subject

ssp_results <- 
  flanker_SDDM_setup %>%
  mutate(
    results = future_map(flanker_data_long,
                         function(x) {
                           
                           message("we are getting past the start")
                           tic()
                           # Find best starting parameters
                           best_starting_parms <-
                             fitMultipleSSP(x, var = var_start_parms,
                                            nParms = n_start_parms,
                                            nTrials = n_first_pass,
                                            multipleSubjects = FALSE)
                           
                           message("We got pas the best starting parms function")
                           
                           
                           # Perform final fit using best starting parameters
                           final_fit <-
                             fitSSP(x, parms = best_starting_parms$bestParameters,
                                    nTrials = n_final_pass, multipleSubjects = FALSE)
                           time <- toc()
                           
                           message("We got pas the final fit")
                           
                           final_fit_results <-
                             tibble(
                               subject    = unique(x$subject),
                               start_a    = best_starting_parms$bestParameters[1],
                               start_t0   = best_starting_parms$bestParameters[2],
                               start_p    = best_starting_parms$bestParameters[3],
                               start_rd   = best_starting_parms$bestParameters[4],
                               start_sda  = best_starting_parms$bestParameters[5],
                               start_g2   = best_starting_parms$g2,
                               start_bbic = best_starting_parms$bBIC,
                               
                               a          = final_fit$bestParameters[1],
                               t0         = final_fit$bestParameters[2],
                               p          = final_fit$bestParameters[3],
                               rd         = final_fit$bestParameters[4],
                               sda        = final_fit$bestParameters[5],
                               g2         = final_fit$g2,
                               bbic       = final_fit$bBIC
                             )
                           
                           # Backup data
                           write_csv(final_fit_results, here("data", "1_pilot", "ssp", str_c("ssp_fit_", unique(x$subject), ".csv")))
                           
                           message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                           
                           return(final_fit_results)
                           
                         },
                         .options = furrr_options(seed = TRUE)
    ))




flanker_ssp_results <- list.files(here("data", "1_pilot", "ssp"), pattern = "^ssp_fit", full.names = TRUE) %>%
  map_df(function(x) read_csv(x)) %>%
  rename(id = subject) %>%
  select(-starts_with('start_'), -starts_with('g2'), -starts_with('bbic')) |> 
  rename_with(.cols = !matches("id"), ~str_replace_all(.x, ., str_c("flanker_ssp_", .))) |> 
  mutate(flanker_ssp_interference = flanker_ssp_sda / flanker_ssp_rd)



# 3. Save objects ---------------------------------------------------------

save(
  # DDM estimation (ML and KS)
  change_DDM_results_mod1, change_bic_mod1,
  change_DDM_results_mod2, change_bic_mod2,
  change_DDM_results_mod3, change_bic_mod3,
  change_DDM_results_mod4, change_bic_mod4,
  change_DDM_results_mod5, change_bic_mod5,
  
  cueing_DDM_results_mod1, cueing_bic_mod1,
  cueing_DDM_results_mod2, cueing_bic_mod2,
  cueing_DDM_results_mod3, cueing_bic_mod3,
  cueing_DDM_results_mod4, cueing_bic_mod4,
  cueing_DDM_results_mod5, cueing_bic_mod5,
  cueing_DDM_results_mod6, cueing_bic_mod6,
  cueing_DDM_results_mod7, cueing_bic_mod7,
  cueing_DDM_results_mod8, cueing_bic_mod8,
  cueing_DDM_results_mod9, cueing_bic_mod9,
  
  flanker_DDM_results_mod1, flanker_bic_mod1,
  flanker_DDM_results_mod2, flanker_bic_mod2,
  
  # SSP estimation (flanker only)
  flanker_ssp_results,
  
  file = "preregistrations/1_pilot/analysis_objects/DDM_objects.Rdata")


