load('data/1_pilot/2_cleaned_data.RData')
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "DBDA2E-utilities.R"))

flanker_clean <- cleaned_data |> 
  select(flanker_data_long) |> 
  unnest(flanker_data_long) |> 
  select(id, rt, correct, congruency)

change_clean <- cleaned_data |> 
  select(change_data_long) |> 
  unnest(change_data_long) |> 
  select(id, rt, correct)

cueing_clean <- cleaned_data |> 
  select(cueing_data_long) |> 
  unnest(cueing_data_long) |> 
  select(id, rt, correct, condition)


# 1. Overview of Model Specifications ----------------------------------------

## 1.1 Standard model, no condition effects ----

# Note: This model will be used for the Change Detection Task

mod_base_1con <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[subject[t]], 
                   0.5, 
                   delta[subject[t]])
  }
  for (s in 1:nSubjects) {
   
    tau[s]  ~ dnorm(muTau, precTau) T(.0001, 1)
    delta[s] ~ dnorm(muDelta, precDelta) T(-5, 5)
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
    
  }
  
  #priors
  muTau ~ dunif(.0001, 1)
  muDelta ~ dunif(-5, 5)
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"

## 1.2 Standard model including condition effects ----

# Note: This model will be used for the Flanker Task and Attention Cueing Task

mod_base_2con <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[condition[t], subject[t]], 
                   0.5, 
                   delta[condition[t], subject[t]])
  }
  for (s in 1:nSubjects) {
    for (c in 1:nCon) {
      tau[c, s]  ~ dnorm(muTau[c], precTau) T(.0001, 1)
      delta[c, s] ~ dnorm(muDelta[c] , precDelta) T(-5, 5)
    }
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  #priors
  for (c in 1:nCon){ 
    muTau[c] ~ dunif(.0001, 1)
    muDelta[c] ~ dunif(-5, 5)
  } 
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"


initfunction_1con <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(1, .01, .05),
    muDelta = runif(1, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}

initfunction_2con <- function(chain){
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


# 2. DDM estimation -------------------------------------------------------

## 2.1 Flanker Task ----

# Create numeric participant IDs
flanker_id_matches <- flanker_clean |> 
  distinct(id) |> 
  mutate(id_num = 1:n())

flanker_clean <- flanker_clean |> 
  mutate(congruency = ifelse(congruency == 'congruent', 1, 2)) |>
  left_join(flanker_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
flanker_y          <- round(ifelse(flanker_clean$correct == 0, (flanker_clean$rt*-1), flanker_clean$rt),3)
yInit              <- rep(NA, length(flanker_y))
flanker_condition  <- as.numeric(flanker_clean$congruency)

#Create numbers for JAGS
flanker_nTrials    <- nrow(flanker_clean)
flanker_nSubjects  <- length(unique(flanker_clean$id))
flanker_nCondition <- max(flanker_condition)

#Create a list of the data; this gets sent to JAGS
flanker_datalist <- list(y = flanker_y, subject = flanker_clean$id_num, con = flanker_condition,
                         nTrials = flanker_nTrials, nCon = flanker_nCondition,
                         nSubjects = flanker_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
hddm_flanker_mod1 <- run.jags(method = "parallel",
                             model = mod_base_2con,
                             monitor = parameters,
                             data = flanker_datalist,
                             inits = initfunction_2con,
                             n.chains = nChains,
                             adapt = 1000, #how long the samplers "tune"
                             burnin = 2000, #how long of a burn in
                             sample = 10000,
                             thin = 10, #thin if high autocorrelation to avoid huge files
                             modules = c("wiener", "lecuyer"),
                             summarise = F,
                             plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_flanker_mod1 <- as.matrix(as.mcmc.list(hddm_flanker_mod1), chains = F) |> 
  as_tibble()

hddm_flanker_data <- mcmc_flanker_mod1 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id_num'), sep = "\\[") |> 
  mutate(
    id_num = str_remove(id_num, pattern = "\\]$"),
    id_num = ifelse(parameter %in% c('delta', 'tau'),
                    str_replace_all(id_num, "([0-9]*),([0-9]*)", "\\2,\\1"),
                    id_num
    )
  ) |> 
  separate(id_num, into = c('id_num', 'condition')) |> 
  mutate(
    id_num = as.numeric(id_num),
    parameter = case_when(
      parameter == 'alpha' ~ 'flanker_fixed_hddm_a',
      parameter == 'tau' & condition == 1 ~ 'flanker_con_hddm_t',
      parameter == 'tau' & condition == 2 ~ 'flanker_incon_hddm_t',
      parameter == 'delta' & condition == 1 ~ 'flanker_con_hddm_v',
      parameter == 'delta' & condition == 2 ~ 'flanker_incon_hddm_v',
    )) |> 
  left_join(
    flanker_clean |> 
      select(id, id_num) |> 
      distinct()
  ) |> 
  select(-id_num, -condition) |> 
  pivot_wider(names_from = parameter, values_from = estimated)



## 2.2 Change Detection Task ----

# Create numeric participant IDs
change_id_matches <- change_clean |> 
  distinct(id) |> 
  mutate(id_num = 1:n())

change_clean <- change_clean |> 
  left_join(change_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
change_y          <- round(ifelse(change_clean$correct == 0, (change_clean$rt*-1), change_clean$rt),3)
yInit              <- rep(NA, length(change_y))

#Create numbers for JAGS
change_nTrials    <- nrow(change_clean)
change_nSubjects  <- length(unique(change_clean$id))

#Create a list of the data; this gets sent to JAGS
change_datalist <- list(y = change_y, subject = change_clean$id_num, 
                         nTrials = change_nTrials, 
                         nSubjects = change_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
hddm_change_mod1 <- run.jags(method = "parallel",
                              model = mod_base_1con,
                              monitor = parameters,
                              data = change_datalist,
                              inits = initfunction_1con,
                              n.chains = nChains,
                              adapt = 1000, #how long the samplers "tune"
                              burnin = 2000, #how long of a burn in
                              sample = 10000,
                              thin = 10, #thin if high autocorrelation to avoid huge files
                              modules = c("wiener", "lecuyer"),
                              summarise = F,
                              plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_change_mod1 <- as.matrix(as.mcmc.list(hddm_change_mod1), chains = F) |> 
  as_tibble()

hddm_change_data <- mcmc_change_mod1 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id_num'), sep = "\\[") |> 
  mutate(
    id_num = str_remove(id_num, pattern = "\\]$"),
    id_num = ifelse(parameter %in% c('delta', 'tau'),
                    str_replace_all(id_num, "([0-9]*),([0-9]*)", "\\2,\\1"),
                    id_num
    )
  ) |> 
  mutate(
    id_num = as.numeric(id_num),
    parameter = case_when(
      parameter == 'alpha' ~ 'change_hddm_a',
      parameter == 'tau' ~ 'change_hddm_t',
      parameter == 'delta' ~ 'change_hddm_v'
    )) |> 
  left_join(
    change_clean |> 
      select(id, id_num) |> 
      distinct()
  ) |> 
  select(-id_num) |> 
  pivot_wider(names_from = parameter, values_from = estimated)




## 2.3 Attention Cueing Task ----

# Create numeric participant IDs
cueing_id_matches <- cueing_clean |> 
  distinct(id) |> 
  mutate(id_num = 1:n())

cueing_clean <- cueing_clean |> 
  mutate(condition = ifelse(condition == 'neutral', 1, 2)) |>
  left_join(cueing_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
cueing_y          <- round(ifelse(cueing_clean$correct == 0, (cueing_clean$rt*-1), cueing_clean$rt),3)
yInit              <- rep(NA, length(cueing_y))
cueing_condition  <- as.numeric(cueing_clean$condition)

#Create numbers for JAGS
cueing_nTrials    <- nrow(cueing_clean)
cueing_nSubjects  <- length(unique(cueing_clean$id))
cueing_nCondition <- max(cueing_condition)

#Create a list of the data; this gets sent to JAGS
cueing_datalist <- list(y = cueing_y, subject = cueing_clean$id_num, con = cueing_condition,
                         nTrials = cueing_nTrials, nCon = cueing_nCondition,
                         nSubjects = cueing_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
hddm_cueing_mod1 <- run.jags(method = "parallel",
                              model = mod_base_2con,
                              monitor = parameters,
                              data = cueing_datalist,
                              inits = initfunction_2con,
                              n.chains = nChains,
                              adapt = 1000, #how long the samplers "tune"
                              burnin = 2000, #how long of a burn in
                              sample = 10000,
                              thin = 10, #thin if high autocorrelation to avoid huge files
                              modules = c("wiener", "lecuyer"),
                              summarise = F,
                              plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_cueing_mod1 <- as.matrix(as.mcmc.list(hddm_cueing_mod1), chains = F) |> 
  as_tibble()

hddm_cueing_data <- mcmc_cueing_mod1 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id_num'), sep = "\\[") |> 
  mutate(
    id_num = str_remove(id_num, pattern = "\\]$"),
    id_num = ifelse(parameter %in% c('delta', 'tau'),
                str_replace_all(id_num, "([0-9]*),([0-9]*)", "\\2,\\1"),
                id_num
    )
  ) |> 
  separate(id_num, into = c('id_num', 'condition')) |> 
  mutate(
    id_num = as.numeric(id_num),
    parameter = case_when(
      parameter == 'alpha' ~ 'cueing_fixed_hddm_a',
      parameter == 'tau' & condition == 1 ~ 'cueing_neutral_hddm_t',
      parameter == 'tau' & condition == 2 ~ 'cueing_cued_hddm_t',
      parameter == 'delta' & condition == 1 ~ 'cueing_neutral_hddm_v',
      parameter == 'delta' & condition == 2 ~ 'cueing_cued_hddm_v',
    )) |> 
  left_join(
    cueing_clean |> 
      select(id, id_num) |> 
      distinct()
  ) |> 
  select(-id_num, -condition) |> 
  pivot_wider(names_from = parameter, values_from = estimated)



save(
  mcmc_flanker_mod1, hddm_flanker_data,
  mcmc_cueing_mod1, hddm_cueing_data,
  mcmc_change_mod1, hddm_change_data,
  file = 'preregistrations/1_pilot/analysis_objects/hddm_model_objects.RData')
