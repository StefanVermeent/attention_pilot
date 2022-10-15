
# 1. Libraries ---------------------------------------------------------------

# R
library(reticulate)
#library(multitool)
#library(tidyverse)

source("preregistrations/2_study1/scripts/custom_functions/functions_analyses.R")
#load("data/2_study1/1_task_data_clean.Rdata")

# Python
# Note: if this script is not run using the stefanvermeent/flanker_project Docker image, a Python 3.7 Conda environment with the relevant dependencies first
# has to be created. See the README file of the Github Repository of this project for more information.
reticulate::use_condaenv("py38")
hddm <- reticulate::import('hddm')
reticulate::py_run_string("import hddm")
#reticulate::import("hddm", convert = TRUE)
reticulate::import("pandas")
reticulate::import("matplotlib.pyplot", as = "plt")





# Load traces -------------------------------------------------------------

py_run_string("traces_std = hddm.load('data/2_study1/traces_std.db')")$traces_std
py_run_string("traces_enh = hddm.load('data/2_study1/traces_enh.db')")$traces_enh
py_run_string("traces_deg = hddm.load('data/2_study1/traces_deg.db')")$traces_deg

traces_std <- py$traces_std[c('a', 'v(congruent)', 'v(incongruent)', 't(congruent)', 't(incongruent)')]
traces_enh <- py$traces_enh[c('a', 'v(congruent)', 'v(incongruent)', 't(congruent)', 't(incongruent)')]
traces_deg <- py$traces_deg[c('a', 'v(congruent)', 'v(incongruent)', 't(congruent)', 't(incongruent)')]

# Load models -------------------------------------------------------------

model_std <- py$hddm$load('data/2_study1/hddm_enh')
py_run_string("model_std = hddm.load('data/2_study1/hddm_std')")
py_run_string("model_enh = hddm.load('data/2_study1/hddm_enh')")
py_run_string("model_deg = hddm.load('data/2_study1/hddm_deg')")

model_std <- py$model_std$values 

model_enh <- py$model_enh$values %>%
  as_tibble() %>%
  pivot_longer(everything(), names_to = "parm", values_to = "value") %>%
  filter(str_detect(parm, "subj")) %>%
  separate(col = parm, into = c("parameter", "id"), sep = "\\.") %>%
  mutate(
    parameter = case_when(
      parameter == "a_subj" ~ "a_hddm_enh",
      parameter == "v_subj(congruent)" ~ "v_con_hddm_enh",
      parameter == "v_subj(incongruent)" ~ "v_incon_hddm_enh",
      parameter == "t_subj(congruent)" ~ "t_con_hddm_enh",
      parameter == "t_subj(incongruent)" ~ "t_incon_hddm_enh"
    )
  ) %>%
  pivot_wider(names_from = "parameter", values_from = 'value')

model_deg <- py$model_deg$values %>%
  as_tibble() %>%
  pivot_longer(everything(), names_to = "parm", values_to = "value") %>%
  filter(str_detect(parm, "subj")) %>%
  separate(col = parm, into = c("parameter", "id"), sep = "\\.") %>%
  mutate(
    parameter = case_when(
      parameter == "a_subj" ~ "a_hddm_deg",
      parameter == "v_subj(congruent)" ~ "v_con_hddm_deg",
      parameter == "v_subj(incongruent)" ~ "v_incon_hddm_deg",
      parameter == "t_subj(congruent)" ~ "t_con_hddm_deg",
      parameter == "t_subj(incongruent)" ~ "t_incon_hddm_deg"
    )
  ) %>%
  pivot_wider(names_from = "parameter", values_from = "value")
  
#TODO, merge this script with the multiverse analysis script so that the model objects do not have to be saved
# For this, I have to fix the multitool installation on the Docker image.

save(model_deg, model_enh, traces_deg, traces_enh, file = "data/2_study1/hddm_objects.RData")

