
# Introduction ------------------------------------------------------------

# Estimated DDM parameters are not perfectly correlated with the true underlying parameter values.
# Here, I attempt to get an estimate of the correlation between simulated DDM parameters and recovered parameters with varying number of trials and with 
# two different estimation techniques: Maximum Likelihood and Kolmogorov-Smirnoff.
# These correlations can be used in the subsequent power analysis to quickly simulate "empirical" DDM parameters, which will prevent us from having to run thousands of
# DDM models.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(furrr)
library(RWiener)
library(here)
library(data.table)

source(here('scripts', 'custom_functions', 'functions_power.R'))

# DDM Parameters ----------------------------------------------------------

n_subjects = 500
n_trials = c(10, 20, 30, 40, 50, 60)
#drift_mean = 1.5
#boundary_mean = 1.2


# Create RT dataset -------------------------------------------------------

set.seed(42)

DDM_data <- expand.grid(subject = 1:n_subjects, trials = n_trials) %>%
  mutate(
    drift_rate = runif(n(), min = 1, max = 4.9),
    boundary_sep = runif(n(), 0.6, 1.9),
    tau = 0.3,
    beta = 0.5,
    id = str_c("subj", subject, "trials", trials, sep = "_")
  )



# Simulate Reaction Time Data ---------------------------------------------

pwalk(DDM_data[c("trials", "boundary_sep", "tau", "beta", "drift_rate", "id")], generate_RTs)


# Recover DDM Parameters --------------------------------------------------

# fast-dm runs via the command prompt. Follow the following steps to recalculate DDM parameters:

# 1. Open the command prompt and set the working directory to <path/to/attention_pilot>/data/0_simulation
# 2. Run the DDM model once for both estimation techniques by typing the following commands one after the other: 
# fast-dm.exe exp_ks.ctl
# fast-dm.exe exp_ml.ctl



# Read Recovered DDM Parameters -------------------------------------------

DDM_data_recovered <- bind_rows(
  read_table(here('data', '0_simulation', 'ddm_pars_ks.lst')),
  read_table(here('data', '0_simulation', 'ddm_pars_ml.lst'))
) %>%
  rename(id = dataset) %>%
  # Join simulated DDM parameters
  left_join(DDM_data %>% select(id, trials, drift_rate, boundary_sep)) %>%
  mutate(
    trials = as.numeric(str_extract_all(id, "[0-9]+$"))
  ) %>%
  group_by(method, trials) %>%
  # Calculate correlation between simulated and recovered parameters
  mutate(
    R_drift = round(cor(drift_rate, v), 3),
    R_bound = round(cor(boundary_sep, a), 3)
  ) %>%
  rename(
    `Simulated Drift Rate` = drift_rate,
    `Simulated Boundary Separation` = boundary_sep,
    `Recovered Drift Rate` = v,
    `Recovered Boundary Separation` = a
  )


# Plot Correlations -------------------------------------------------------


## Drift Rate
DDM_recovery_plot <- DDM_data_recovered %>%
  ggplot(aes(`Simulated Drift Rate`, `Recovered Drift Rate`, color = factor(trials))) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(method~trials, scales = "free") +
  stat_cor(method="pearson")

ggsave(DDM_recovery_plot, here("plots", "0_simulation_DDM_recovery_plot.png"), width = 12, height = 8)
