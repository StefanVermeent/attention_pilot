
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(here)
library(ggeffects)
library(lmerTest)

load("data/2_study1/1_task_data_clean.Rdata")


# Load data files ---------------------------------------------------------
hddm_flanker_data <- flanker_data_clean_average |> 
  select(flanker_data_long) |> 
  unnest(flanker_data_long) |> 
  select(id, rt, condition, congruency, correct) |> 
  drop_na(rt) |> 
  mutate(
    rt           = rt / 1000,
    version_cong = case_when(
      condition == "standard" & congruency == "congruent" ~ 1,
      condition == "standard" & congruency == "incongruent" ~ 2,
      condition == "enhanced" & congruency == "congruent" ~ 3,
      condition == "enhanced" & congruency == "incongruent" ~ 4,
      condition == "degraded" & congruency == "congruent" ~ 5,
      condition == "degraded" & congruency == "incongruent" ~ 6,
    ),
    version = case_when(
      condition == "standard" ~ 1,
      condition == "enhanced" ~ 2,
      condition == "degraded" ~ 3
    )
  )
  

# congruency: congruent (1) vs incongruent (2)
# condition: standard (1) vs enhanced (2) vs degraded (3)

  
mod <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[version[t], subject[t]], 
                   tau[version_cong[t], subject[t]], 
                   0.5, 
                   delta[version_cong[t], subject[t]])
  }
  
  for (s in 1:nSubjects) {
    for (version_cong in 1:nVersionCong) {

      tau[version_cong, s] ~ dnorm(muTau[version_cong], precTau) T(.0001, 1)
      delta[version_cong, s] ~ dnorm(muDelta[version_cong] , precDelta) T(-10, 10)
      
    }
    
    for (version in 1:nVersion) {
      
      alpha[version, s] ~ dnorm(muAlpha[version], precAlpha) T(.1, 5)
      
    }
  }
    
  
  #priors
  for (version_cong in 1:nVersionCong){ 
    
      muTau[version_cong] ~ dunif(.0001, 1)
      muDelta[version_cong] ~ dunif(-10, 10)
  
  } 
    
  for (version in 1:nVersion) {
  
    muAlpha[version] ~ dunif(.1, 5)
  
  }
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
  
}"

initfunction <- function(chain){
  return(list(
    muAlpha = runif(3, .2, 4.9),
    muTau = runif(6, .01, .05),
    muDelta = runif(6, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}


# 2. HDDM estimation -------------------------------------------------------

## 2.1 Flanker Task ----

# Create numeric participant IDs
flanker_id_matches <- hddm_flanker_data |> 
  distinct(id) |> 
  mutate(id_num = 1:n())

hddm_flanker_data <- hddm_flanker_data |> 
  left_join(flanker_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
flanker_y              <- round(ifelse(hddm_flanker_data$correct == 0, (hddm_flanker_data$rt*-1), hddm_flanker_data$rt),3)
yInit                  <- rep(NA, length(flanker_y))
flanker_version_cong  <- as.numeric(hddm_flanker_data$version_cong)
flanker_version        <- as.numeric(hddm_flanker_data$version)

#Create numbers for JAGS
flanker_nTrials    <- nrow(hddm_flanker_data)
flanker_nSubjects  <- length(unique(hddm_flanker_data$id))
flanker_nVersionCong <- max(flanker_version_cong)
flanker_nVersion    <- max(flanker_version)

#Create a list of the data; this gets sent to JAGS
flanker_datalist <- list(y = flanker_y, subject = hddm_flanker_data$id_num, 
                         version_cong = flanker_version_cong, version = flanker_version,
                         nTrials = flanker_nTrials, nVersionCong = flanker_nVersionCong, nVersion = flanker_nVersion,
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
                              model = mod,
                              monitor = parameters,
                              data = flanker_datalist,
                              inits = initfunction,
                              n.chains = nChains,
                              adapt = 1000, #how long the samplers "tune"
                              burnin = 50, #how long of a burn in
                              sample = 1000,
                              thin = 10, #thin if high autocorrelation to avoid huge files
                              modules = c("wiener", "lecuyer"),
                              summarise = F,
                              plots = F)


# Extract results

#Convert the runjags object to a coda format
mcmc_flanker_mod1 <- as.matrix(as.mcmc.list(hddm_flanker_mod1), chains = F) |> 
  as_tibble()

hddm_flanker_results <- mcmc_flanker_mod1 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id_num'), sep = "\\[") |> 
  mutate(id_num = str_remove(id_num, pattern = "\\]$"),) |> 
  separate(id_num, into = c('condition', 'id_num')) |> 
  mutate(
    id_num = as.numeric(id_num),
    
    condition = case_when(
      parameter == 'alpha' & condition == 1 ~ 'std_con',
      parameter == 'alpha' & condition == 2 ~ 'enh_con',
      parameter == 'alpha' & condition == 3 ~ 'deg_con',
      parameter %in% c('delta', 'tau') & condition == 1 ~ 'std_con',
      parameter %in% c('delta', 'tau') & condition == 2 ~ 'std_incon',
      parameter %in% c('delta', 'tau') & condition == 3 ~ 'enh_con',
      parameter %in% c('delta', 'tau') & condition == 4 ~ 'enh_incon',
      parameter %in% c('delta', 'tau') & condition == 5 ~ 'deg_con',
      parameter %in% c('delta', 'tau') & condition == 6 ~ 'deg_incon',
    ),
    parameter = case_when(
      parameter == 'alpha' ~ 'hddm_a',
      parameter == 'delta' ~ 'hddm_v',
      parameter == 'tau' ~ 'hddm_t0'
    )
  ) |> 
  separate(condition, into = c('condition', 'congruency'), sep = "_") |> 
  left_join(
    hddm_flanker_data |> 
      select(id, id_num) |> 
      distinct()
  ) |> 
  ungroup() |> 
  pivot_wider(names_from = 'parameter', values_from = 'estimated') |> 
  arrange(id) |> 
  mutate(
    hddm_a = ifelse(is.na(hddm_a), lag(hddm_a, 3), hddm_a)
  )
  
  select(-id_num, -condition) |> 
  pivot_wider(names_from = parameter, values_from = estimated)



save(
  mcmc_flanker_mod1, hddm_flanker_results,
  file = 'preregistrations/2_study1/analysis_objects/hddm_model_objects.RData')


hddm_flanker_results |> 
  unite("condition", c(condition, congruency)) |> pivot_wider(names_from = 'condition', values_from = c(hddm_a, hddm_v, hddm_t0)) |> 
  left_join(cleaned_data |> select(id, unp_comp, vio_comp)) |> 
  select(-c(id_num, id)) |> 
  mutate(across(everything(), ~scale(.) |> as.numeric())) |> 
  cor(use = 'complete.obs') |> 
  corrplot::corrplot(method = 'number')

mcmc_flanker_mod1 |> 
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


mcmc_flanker_mod1 |> 
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
