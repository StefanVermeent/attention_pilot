# Author: Stefan Vermeent, Utrecht University
# E-mail: p.c.s.vermeent@gmail.com


# Introduction ------------------------------------------------------------

# This is a power analysis for the cognitive tasks in the pilot, where I calculate the required number of participants given a certain number of trials.
# As the DDM is fit to each individual dataset, running a power analysis with several hundreds of iterations per subject/condition would take a substantial amount of time.
# In order to prevent having to fit the DDM that many times, the power analysis below consists of two steps.

# First, I calculate the correlation between simulated and recovered DDM parameters for varying numbers of trials and across two common estimation techniques: Maximum Likelihood and Kolmogorov-Smirnoff.. 
# As estimated DDM parameters are not perfectly correlated with the true, data-generating parameter values (especially with fewer trials), this correlation gives an
# estimate of the average "imprecision" of estimated DDM parameters. I do this for the Drift Rate and the Boundary Separation parameters, which are of main interest
# to us.

# Next, I use these correlation values to simulate generate two sets of DDM parameters: the "true", data-generating parameter values, and the "recovered" parameter values
# which correlate to a fixed extend with the "true" parameters, as informed by the first part of the analysis. Thus, this approach allows us to run a power analysis
# on the DDM parameters without having to fit the DDM tens- to hundreds of thousands of times.


# Estimated DDM parameters are not perfectly correlated with the true underlying parameter values, especially with fewer trials.
# Here, I attempt to get an estimate of the correlation between simulated DDM parameters and recovered parameters with varying number of trials and with 
# two different estimation techniques: Maximum Likelihood and Kolmogorov-Smirnoff.
# These correlations can be used in the subsequent power analysis to quickly simulate "empirical" DDM parameters, which will prevent us from having to run thousands of
# DDM models.




# Part I: Estimation of DDM Precision -------------------------------------



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(ggpubr)
library(furrr)
library(parallel)
library(RWiener)
library(here)
library(data.table)
library(faux)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(tictoc)

cores <- parallel::detectCores()

source(here('scripts', 'custom_functions', 'functions_power.R'))

# DDM Parameters ----------------------------------------------------------

n_subjects = 500
n_trials = c(10, 20, 30, 40, 50, 60)


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



# Read In Recovered DDM Parameters -------------------------------------------

DDM_data_recovered <- bind_rows(
  read.table(here('data', '0_simulation', 'ddm_pars_ks.lst'), header = TRUE),
  read.table(here('data', '0_simulation', 'ddm_pars_ml.lst'), header = TRUE)
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
    abs_error_drift = abs(drift_rate - v),
    abs_error_boundary = abs(boundary_sep - a)
  ) %>%
  rename(
    `Simulated Drift Rate` = drift_rate,
    `Simulated Boundary Separation` = boundary_sep,
    `Recovered Drift Rate` = v,
    `Recovered Boundary Separation` = a
  )


# Plot Correlations -------------------------------------------------------

## Drift Rate
DDM_driftrate_recovery_cor_plot <- DDM_data_recovered %>%
  ggplot(aes(`Simulated Drift Rate`, `Recovered Drift Rate`, color = `Recovered Boundary Separation`)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(method~trials, scales = "free") +
  stat_cor(method="pearson") +
  labs(
    title = "Correlation Between Simulated and Recovered Drift Rate Under Varying # of Trials",
    color = "Nr. Of Trials"
  )

ggsave(plot = DDM_driftrate_recovery_cor_plot, filename = here("plots", "0_simulation_driftrate_recovery_cor.png"), width = 15, height = 8)


## Boundary separation
DDM_boundary_recovery_cor_plot <- DDM_data_recovered %>%
  ggplot(aes(`Simulated Boundary Separation`, `Recovered Boundary Separation`, color = `Recovered Drift Rate`)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(method~trials, scales = "free") +
  stat_cor(method="pearson") +
  labs(
    title = "Correlation Between Simulated and Recovered Boundary Separation Under Varying # of Trials",
    color = "Drift Rate"
  )

ggsave(plot = DDM_boundary_recovery_cor_plot, filename = here("plots", "0_simulation_boundary_recovery_cor.png"), width = 15, height = 8)



# Plot Absolute Errors ----------------------------------------------------

## Drift Rate
DDM_driftrate_recovery_error_plot <- DDM_data_recovered %>%
  ggplot(aes(factor(trials), abs_error_drift, fill = factor(trials))) +
  geom_violin() +
  facet_grid(method~trials, scales = "free") +
  labs(
    title = "Absolute Error in Drift Rate Recovery Under Varying # of Trials",
    x = "Trials",
    y = "Absolute Error in Drift Rate Estimate",
    fill = "Nr. Of Trials"
  )

ggsave(plot = DDM_boundary_recovery_error_plot, filename = here("plots", "0_simulation_driftrate_recovery_error.png"), width = 15, height = 8)


## Boundary Separation
DDM_boundary_recovery_error_plot <- DDM_data_recovered %>%
  ggplot(aes(factor(trials), abs_error_boundary, fill = factor(trials))) +
  geom_violin() +
  facet_grid(method~trials, scales = "free") +
  labs(
    title = "Absolute Error in Boundary Separation Recovery Under Varying # of Trials",
    x = "Trials",
    y = "Absolute Error in Boundary Separation Estimate",
    fill = "Nr. Of Trials"
  )

ggsave(plot = DDM_boundary_recovery_error_plot, filename = here("plots", "0_simulation_boundary_recovery_error.png"), width = 15, height = 8)




# Part II: Power Analysis -------------------------------------------------

# The analyses under Part I indicate that the recovered drift rate values correlate with >=.90 with the simulated, "true" values starting at 30 trials.
# For the boundary separation, the correlation is ~ .80 starting at 30 trials. The boundary separation plots show a heteroscedastistic pattern, with lower
# precision for higher values.

# 







# Simulate data for power analysis ----------------------------------------


# Simulation parameters
intercept = 0      # Intercept
b1_cond = 0.1        # Fixed effect of condition
b2_adversity = 0.1   # Fixed effect of adversity
b3_interaction = 0.1 # Fixed effect of interaction
intercept_sd = 2     # Random intercept SD for subjects
#sigma_sd = 2.2       # Error SD


# Grid containing the simulation parameters to loop over
simulation_grid <- expand_grid(
  n_subjects = c(300, 400, 500, 600),
  DDM_recovery_correlation = c(.60, .75, .80, .85, .90, .95),
  sigma_sd = c(0.2, 0.5, 1, 1.5, 2),
  n_sim = 1:500
) 

# Use multiple cores for simulation
plan(multisession, workers = cores - 2)

# Loop over simulation grid to simulate random sets across parameter space
simulation_data <- 
  simulation_grid %>%
  future_pmap(.f = function(n_subjects, DDM_recovery_correlation, sigma_sd, n_sim) {
    
    data <- add_random(subjects = n_subjects) %>%
      add_within("subjects", condition = c("cued", "neutral")) %>%
      add_ranef("subjects", intercept_s = intercept_sd) %>%
      add_ranef(sigma = sigma_sd) %>%
      add_contrast("condition", "sum", colnames = "condition_sum") %>%
      mutate(
        adversity = rnorm(n(), 0, 1),
        drift_rate_true = intercept + intercept_s + (b1_cond * condition_sum) + (b2_adversity * adversity) + (b3_interaction*condition_sum*adversity) + sigma,
        drift_rate_recov = rnorm_pre(drift_rate_true, mu = mean(drift_rate_true), sd = sd(drift_rate_true), r = DDM_recovery_correlation, empirical = TRUE),
        n_sim = n_sim,
        r = DDM_recovery_correlation,
        sigma_sim = sigma_sd
      )
    
  },
  .options = furrr_options(seed = TRUE)
  )





# Power analysis ----------------------------------------------------------
plan(multisession, workers = cores - 2)

tic()
power_results <- simulation_data %>%
  future_map(function(x) {
    
    # DV = "true" DDM parameter value
    fit_true <- suppressMessages(lmerTest::lmer(data = x, drift_rate_true ~ condition_sum*adversity + (1|subjects)))
    p_main_effect_true <- coef(summary(fit_true))['adversity', 'Pr(>|t|)']
    p_interaction_true <- coef(summary(fit_true))['condition_sum:adversity', 'Pr(>|t|)']
    
  
    # DV = "recovered" DDM parameter value
    fit_recov <- lmerTest::lmer(data = x, drift_rate_recov ~ condition_sum*adversity + (1|subjects))
    p_main_effect_recov <- coef(summary(fit_recov))['adversity', 'Pr(>|t|)']
    p_interaction_recov <- coef(summary(fit_recov))['condition_sum:adversity', 'Pr(>|t|)']
    

    
    results <- list(
      n_subject = nrow(x)/2,
      n_sim = x$n_sim[1],
      r = x$r[1],
      sigma = x$sigma_sim[1],
      p_main_effect_true = p_main_effect_true,
      p_interaction_true = p_interaction_true,
      p_main_effect_recov = p_main_effect_recov,
      p_interaction_recov = p_interaction_recov
    )
    

    
    results
  })
toc()


power_results_df <- bind_rows(power_results)

write_csv(power_results_df, "power_results_df_lowsigma")

power_results_df %<>%
  group_by(n_subject, r, sigma) %>%
  summarise(
    `True Main Effect` = (sum(p_main_effect_true < .05) / n()) * 100,
    `Recovered Main Effect` = (sum(p_main_effect_recov < .05) / n()) * 100,
    `True Interaction` = (sum(p_interaction_true < .05) / n()) * 100,
    `Recovered Interaction` = (sum(p_interaction_recov < .05) / n()) * 100,   
  ) %>%
  ungroup() %>%
  pivot_longer(-c(n_subject, r, sigma), names_to = "effect", values_to = "power")


power_plot <- ggplot(power_results_df, aes(factor(n_subject), factor(r), fill = power)) +
  geom_tile() +
  geom_text(aes(label = power)) +
  facet_grid(sigma~effect) +
  labs(
    x = "Sample Size",
    y = "Correlation of Recovered DDM parameter",
  )

ggsave(power_plot, file = here("plots", "power_plot3.png"), width = 15, height = 15)
  
  
  
    
  
  










