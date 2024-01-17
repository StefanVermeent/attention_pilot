
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(here)
library(ggeffects)
library(lmerTest)
library(runjags)
load("data/3_study2/1_task_data_clean.Rdata")
load("data/3_study2/1_self_report_clean.Rdata")

source("preregistrations/3_study2/scripts/custom_functions/DBDA2E-utilities.R")


# Load data files ---------------------------------------------------------
hddm_globloc_data <- globloc_data_clean_average |> 
  select(globloc_data_long) |> 
  unnest(globloc_data_long) |> 
  select(id, rt, rule, correct) |> 
  drop_na(rt) |> 
  mutate(
    rt           = rt / 1000,
    rule = ifelse(rule == "local", 1, 2)
  )
  

# congruency: congruent (1) vs incongruent (2)
# condition: standard (1) vs enhanced (2) vs degraded (3)

  
mod <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[rule[t], subject[t]], 
                   0.5, 
                   delta[rule[t], subject[t]])
  }
  
  for (s in 1:nSubjects) {
    for (rule in 1:nRule) {

      tau[rule, s] ~ dnorm(muTau[rule], precTau) T(.0001, 1)
      delta[rule, s] ~ dnorm(muDelta[rule] , precDelta) T(-10, 10)
      
    }
      alpha[s] ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  
  #priors
  for (rule in 1:nRule){ 
    
      muTau[rule] ~ dunif(.0001, 1)
      muDelta[rule] ~ dunif(-10, 10)
  
  } 
  muAlpha ~ dunif(.1, 5)
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
  
}"

initfunction <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(2, .01, .05),
    muDelta = runif(2, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}


# 2. Global-Local HDDM estimation -------------------------------------------------------

## 2.1 Run HDDM model----

# Create numeric participant IDs
globloc_id_matches <- hddm_globloc_data |> 
  distinct(id) |> 
  mutate(id_num = 1:n())

hddm_globloc_data <- hddm_globloc_data |> 
  left_join(globloc_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
globloc_y              <- round(ifelse(hddm_globloc_data$correct == 0, (hddm_globloc_data$rt*-1), hddm_globloc_data$rt),3)
yInit                  <- rep(NA, length(globloc_y))
globloc_rule        <- as.numeric(hddm_globloc_data$rule)

#Create numbers for JAGS
globloc_nTrials    <- nrow(hddm_globloc_data)
globloc_nSubjects  <- length(unique(hddm_globloc_data$id))
globloc_nRule    <- max(globloc_rule)

#Create a list of the data; this gets sent to JAGS
globloc_datalist <- list(y = globloc_y, subject = hddm_globloc_data$id_num, 
                         rule = globloc_rule, 
                         nTrials = globloc_nTrials, nRule = globloc_nRule,
                         nSubjects = globloc_nSubjects)


# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
hddm_globloc_mod1 <- run.jags(method = "parallel",
                              model = mod,
                              monitor = parameters,
                              data = globloc_datalist,
                              inits = initfunction,
                              n.chains = nChains,
                              adapt = 1000, #how long the samplers "tune"
                              burnin = 2000, #how long of a burn in
                              sample = 10000,
                              thin = 10, #thin if high autocorrelation to avoid huge files
                              modules = c("wiener", "lecuyer"),
                              summarise = F,
                              plots = F)

save(
  hddm_globloc_mod1,
  file = 'preregistrations/3_study2/analysis_objects/hddm_model_objects.RData')



## 2.2 Extract results ----

#Convert the runjags object to a coda format
mcmc_globloc_mod1 <- as.matrix(as.mcmc.list(hddm_globloc_mod1), chains = F) |> 
  as_tibble()

hddm_globloc_results_mod1 <- mcmc_globloc_mod1 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id_num'), sep = "\\[") |> 
  mutate(id_num = str_remove(id_num, pattern = "\\]$")) |> 
  separate(id_num, into = c('condition', 'id_num')) |> 
  mutate(id_num = ifelse(parameter == 'alpha', condition, id_num)) |> 
  mutate(
    id_num = as.numeric(id_num),
    
    condition = case_when(
      parameter == 'alpha' ~ 'fixed',
      parameter %in% c('delta', 'tau') & condition == 1 ~ 'local',
      parameter %in% c('delta', 'tau') & condition == 2 ~ 'global',
    ),
    parameter = case_when(
      parameter == 'alpha' ~ 'hddm_a',
      parameter == 'delta' ~ 'hddm_v',
      parameter == 'tau' ~ 'hddm_t0'
    )
  ) |> 
  left_join(
    hddm_globloc_data |> 
      select(id, id_num) |> 
      distinct()
  ) |> 
  ungroup() |> 
  unite("parameter", c(parameter, condition)) |> 
  pivot_wider(names_from = 'parameter', values_from = 'estimated') |> 
  select(-id_num)


save(
  mcmc_globloc_mod1, hddm_globloc_results_mod1,
  file = 'preregistrations/3_study2/analysis_objects/hddm_globloc_model_objects.RData')


hddm_globloc_results_mod1 |> 
  left_join(self_report_clean |> select(id, unp_comp, vio_comp)) |> 
  select(-id) |> 
  mutate(across(everything(), ~scale(.) |> as.numeric())) |> 
  cor(use = 'complete.obs') |> 
  corrplot::corrplot(method = 'number')

mcmc_globloc_mod1 |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:10000, 3),
    chains = rep(1:3, each = 10000))  |> 
  pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value') |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )


mcmc_globloc_mod1 |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:10000, 3),
    chains = rep(1:3, each = 10000))  |> 
  pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value') |> 
ggplot(aes(value, fill = parameter)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  ) +
  guides(fill = 'none')

## 2.3 Assess fit ----

set.seed(388532)

hddm_globloc_data_sim <- 
  hddm_globloc_results_mod1 |> 
  pmap_dfr(function(id, hddm_a_fixed, hddm_v_local, hddm_v_global, hddm_t0_local, hddm_t0_global){
    
    bind_rows(
    local <- RWiener::rwiener(n = 32, alpha = hddm_a_fixed, tau = hddm_t0_local, beta = 0.5, delta = hddm_v_local) |> as_tibble() |> mutate(id = id, rule = 1),
    global <- RWiener::rwiener(n = 32, alpha = hddm_a_fixed, tau = hddm_t0_global, beta = 0.5, delta = hddm_v_global) |> as_tibble() |> mutate(id = id, rule = 2)
    ) |> 
      rename(rt = q, correct = resp) |> 
      mutate(correct = ifelse(correct == 'upper', 1, 0))
  })


hddm_mod1_fit <- 
  hddm_globloc_data_sim |> 
  group_by(id, rule) |> 
  summarise(
    quan25 = quantile(rt, probs = 0.25),
    quan50 = quantile(rt, probs = 0.50),
    quan75 = quantile(rt, probs = 0.75),
    mean_acc_sim = sum(correct == 1)/n()
  ) |> 
  ungroup() |> 
  pivot_longer(c(quan25, quan50, quan75), names_to = "quantile", values_to = 'simulated') |> 
  left_join(
    hddm_globloc_data |> 
      group_by(id, rule) |> 
      summarise(
        quan25 = quantile(rt, probs = 0.25),
        quan50 = quantile(rt, probs = 0.50),
        quan75 = quantile(rt, probs = 0.75),
        mean_acc_data = sum(correct == 1)/n()
      ) |> 
      ungroup() |> 
      pivot_longer(c(quan25, quan50, quan75), names_to = "quantile", values_to = 'data')
  ) |> 
  mutate(
    rule = ifelse(rule == 1, "Local", "Global"),
    rt_resid = abs(simulated - data) |> scale() |> as.numeric(),
    acc_resid = abs(mean_acc_sim - mean_acc_data) |> scale() |> as.numeric(),
    resid_chr = ifelse(rt_resid > 3.2 | rt_resid < -3.2 | acc_resid > 3.2 | acc_resid < -3.2, "Outlier", "Not an outlier")
  ) 

ggplot(hddm_mod1_fit) +
  geom_point(aes(simulated, data, color = resid_chr)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(rule~quantile)
  
ggplot(hddm_mod1_fit) +
  geom_point(aes(mean_acc_sim, mean_acc_data, color = resid_chr)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0.594, linetype = "dashed") +
  facet_wrap(~rule)


## 2.4 Refit Global-Local without outliers ----

globloc_outliers <- hddm_mod1_fit |> 
  filter(rt_resid > 3.2 | rt_resid < -3.2 | acc_resid > 3.2 | acc_resid < -3.2) |> 
  pull(id) |> 
  unique()

hddm_globloc_data_mod2 <- 
  hddm_globloc_data |>
  left_join(globloc_id_matches) |>
  filter(!id %in% globloc_outliers)


# Create numeric participant IDs
globloc_id_matches <- hddm_globloc_data_mod2 |> 
  distinct(id) |> 
  mutate(id_num = 1:n())



# Store RTs and condition per trial (incorrect RTs are coded negatively)
globloc_y_mod2              <- round(ifelse(hddm_globloc_data_mod2$correct == 0, (hddm_globloc_data_mod2$rt*-1), hddm_globloc_data_mod2$rt),3)
yInit_mod2                  <- rep(NA, length(globloc_y_mod2))
globloc_rule_mod2           <- as.numeric(hddm_globloc_data_mod2$rule)

#Create numbers for JAGS
globloc_nTrials_mod2    <- nrow(hddm_globloc_data_mod2)
globloc_nSubjects_mod2  <- length(unique(hddm_globloc_data_mod2$id))
globloc_nRule_mod2      <- max(globloc_rule_mod2)

#Create a list of the data; this gets sent to JAGS
globloc_datalist_mod2 <- list(y = globloc_y_mod2, subject = hddm_globloc_data_mod2$id_num, 
                              rule = globloc_rule_mod2, 
                              nTrials = globloc_nTrials_mod2, nRule = globloc_nRule_mod2,
                              nSubjects = globloc_nSubjects_mod2)


# JAGS Specifications

initfunction <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(2, .01, .05),
    muDelta = runif(2, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit_mod2,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run

nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
hddm_globloc_mod2 <- run.jags(method = "parallel",
                              model = mod,
                              monitor = parameters,
                              data = globloc_datalist_mod2,
                              inits = initfunction,
                              n.chains = nChains,
                              adapt = 1000, #how long the samplers "tune"
                              burnin = 2000, #how long of a burn in
                              sample = 1000,
                              thin = 10, #thin if high autocorrelation to avoid huge files
                              modules = c("wiener", "lecuyer"),
                              summarise = F,
                              plots = F)

## 2.5 Extract results ----

#Convert the runjags object to a coda format
mcmc_globloc_mod2 <- as.matrix(as.mcmc.list(hddm_globloc_mod2), chains = F) |> 
  as_tibble()

hddm_globloc_results_mod2 <- mcmc_globloc_mod2 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id_num'), sep = "\\[") |> 
  mutate(id_num = str_remove(id_num, pattern = "\\]$")) |> 
  separate(id_num, into = c('condition', 'id_num')) |> 
  mutate(id_num = ifelse(parameter == 'alpha', condition, id_num)) |> 
  mutate(
    id_num = as.numeric(id_num),
    
    condition = case_when(
      parameter == 'alpha' ~ 'fixed',
      parameter %in% c('delta', 'tau') & condition == 1 ~ 'local',
      parameter %in% c('delta', 'tau') & condition == 2 ~ 'global',
    ),
    parameter = case_when(
      parameter == 'alpha' ~ 'hddm_a',
      parameter == 'delta' ~ 'hddm_v',
      parameter == 'tau' ~ 'hddm_t0'
    )
  ) |> 
  left_join(
    hddm_globloc_data_mod2 |> 
      select(id, id_num) |> 
      distinct()
  ) |> 
  ungroup() |> 
  unite("parameter", c(parameter, condition)) |> 
  pivot_wider(names_from = 'parameter', values_from = 'estimated') |> 
  select(-id_num)

save(
  hddm_globloc_mod1,
  file = 'preregistrations/3_study2/analysis_objects/hddm_model_objects.RData')
