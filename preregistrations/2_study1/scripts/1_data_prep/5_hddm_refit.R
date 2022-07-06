
# 1. Libraries ---------------------------------------------------------------

# R
library(reticulate)
library(tidyverse)

source("preregistrations/2_study1/scripts/custom_functions/functions_analyses.R")
load("data/2_study1/1_task_data_clean.Rdata")

# Python
# Note: if this script is not run using the stefanvermeent/flanker_project Docker image, a Python 3.7 Conda environment with the relevant dependencies first
# has to be created. See the README file of the Github Repository of this project for more information.
reticulate::use_condaenv("py37")
hddm <- reticulate::import("hddm")
reticulate::import("pandas", as = "pd")
reticulate::import("matplotlib.pyplot", as = "plt")


# 2. Load data ---------------------------------------------
hddm_data <- flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x) {
      
      
      x %>%
        mutate(rt=rt/1000) %>%
        select(-response) %>%
        rename(subj_idx = id, response = correct) %>%
        select(subj_idx, condition, congruency, response, rt)
    }) 
  ) %>%
  unnest(flanker_data_long)

hddm_data_std <- hddm_data %>% filter(condition == "standard", subj_idx %in% 1:10)
hddm_data_enh <- hddm_data %>% filter(condition == "enhanced")
hddm_data_deg <- hddm_data %>% filter(condition == "degraded")


# 3. HDDM --------------------------------------------------------------------

## 3.1 Models ----

  # a. Parameters fixed (i.e., equal) across congruency levels; standard condition
  # b. Parameters freely estimated across congruency levels; standard condition


## 3.2 Standard condition: Model a ---- 

### 3.2.1 Initiate and fit model
py_run_string("mod_fixed_std = r.hddm.HDDM(data = r.hddm_data_std, bias = False)")            
py_run_string("mod_fixed_std.find_starting_values()")                                 
py_run_string("hddm_fixed_std = mod_fixed_std.sample(500, burn=10)")    

### 3.2.2 Extract parameter estimates and fit statistics
hddm_fixed_std_parms <- parse_hddm_stats(py$hddm_fixed_std$stats()) 
hddm_fixed_std_dic   <- py$hddm_fixed_std$DIC 
hddm_fixed_std_bpic  <- py$hddm_fixed_std$BPIC

### 3.2.3 Extract traces
hddm_fixed_std_traces <- parse_hddm_traces(stats_object = "hddm_fixed_std")


## 3.3 Standard condition: Model b ---- 

### 3.3.1 Initiate and fit model
py_run_string("mod_free_std = r.hddm.HDDM(data = r.hddm_data_std, bias = False, depends_on = ({'v':'congruency', 't':'congruency'}))") 
py_run_string("mod_free_std.find_starting_values()")                                                                                  
py_run_string("hddm_free_std = mod_free_std.sample(500, burn=10)")                                                                    

### 3.3.2 Extract parameter estimates and fit statistics
hddm_free_std_parms <- parse_hddm_stats(py$hddm_free_std$stats()) 
hddm_free_std_dic   <- py$hddm_free_std$DIC 
hddm_free_std_bpic  <- py$hddm_free_std$BPIC

### 3.3.3 Extract traces
hddm_free_std_traces <- parse_hddm_traces(stats_object = "hddm_free_std", parms = c('v(congruent)', 'v(incongruent)', 'v_std',
                                                                                    'a', 'a_std',
                                                                                    't(congruent)', 't(incongruent)', 't_std'))


save(hddm_fixed_std_parms, hddm_fixed_std_dic, hddm_fixed_std_bpic, hddm_fixed_std_traces,
     hddm_free_std_parms, hddm_free_std_dic, hddm_free_std_bpic, hddm_free_std_traces,
     file = "data/2_study1/hddm_objects.RData")

