
# 1. Libraries ---------------------------------------------------------------

# R
library(reticulate)
library(tidyverse)

source("preregistrations/2_study1/scripts/custom_functions/functions_analyses.R")
load("data/2_study1/1_task_data_clean.Rdata")

# Python
reticulate::import("hddm")
reticulate::import("pandas", as = "pd")
reticulate::import("matplotlib.pyplot", as = "plt")


# 2. Set Conda env and load data ---------------------------------------------

# Note: setting the conda environment is only necessary when running this file locally, not when it's run inside the Docker container.
use_condaenv("py37")

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
  )


# 3. HDDM --------------------------------------------------------------------

## 3.1 Models ----

  # a. Parameters fixed (i.e., equal) across congruency levels; standard condition
  # b. Parameters freely estimated across congruency levels; standard condition


## 3.2 Standard condition: Model a ---- 

py_run_string("mod_fixed_std = hddm.HDDM(data = r.data, bias = False)")                  # Specify model
py_run_string("mod_fixed_std.find_starting_values()")                                    # Find starting values
py_run_string("hddm_fixed_std = mod_fixed_std.sample(100000, burn=50000, thin = 10)")    # Sample from model

py_run_string("hddm_fixed_std")


hddm_fixed_std_parms <- parse_hddm_stats(hddm_fixed_std$hddm_fixed_std$stats()) 
hddm_fixed_std_dic   <- hddm_fixed_std$hddm_fixed_std$DIC 
hddm_fixed_std_bpic  <- hddm_fixed_std$hddm_fixed_std$BPIC


py_run_string("v_trace = hddm_fixed_std.trace('v')[:]")
py_run_string("v_std_trace = hddm_fixed_std.trace('v_std')[:]")
py_run_string("a_trace = hddm_fixed_std.trace('a')[:]")
py_run_string("a_std_trace = hddm_fixed_std.trace('a_std')[:]")
py_run_string("t_trace = hddm_fixed_std.trace('t')[:]")
py_run_string("t_std_trace = hddm_fixed_std.trace('t_std')[:]")


py$v_trace %>%
  as_tibble() %>%
  mutate(run = 1:n()) %>%
  ggplot() +
  geom_line(aes(run, value)) +
  theme_classic()

py$v_trace %>%
  as_tibble() %>%
  mutate(run = 1:n()) %>%
  ggplot() +
  geom_histogram(aes(value)) +
  theme_classic()


## 3.2 Standard condition: Model b ---- 

py_run_string("mod_free_std = hddm.HDDM(data = r.data, bias = False)")                  # Specify model
py_run_string("mod_free_std.find_starting_values()")                                    # Find starting values
py_run_string("hddm_free_std = mod_free_std.sample(100000, burn=50000, thin = 10)")    # Sample from model


hddm_free_std_parms <- parse_hddm_stats(hddm_free_std$hddm_free_std$stats()) 
