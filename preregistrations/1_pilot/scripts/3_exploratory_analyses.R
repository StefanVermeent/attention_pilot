library(tidyverse)
library(magrittr)
library(broom)
library(here)
library(stats)
library(GPArotation)
library(nFactors)
library(multitool) #remotes::install_github('https://github.com/ethan-young/multitool', force = T)
library(lmerTest)
library(ggeffects)
library(interactions)
library(parameters)
library(specr)
library(flextable)


load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))
load("preregistrations/1_pilot/analysis_objects/hddm_model_objects.RData")

load('data/1_pilot/2_cleaned_data.RData')
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "DBDA2E-utilities.R"))

source("preregistrations/1_pilot/scripts/custom_functions/ddm_functions.R")

# set up flextable for tables
set_flextable_defaults(
  font.family = "Times", 
  font.size = 10,
  font.color = "black",
  line_spacing = 1,
  padding.bottom = 1, 
  padding.top = 1,
  padding.left = 1,
  padding.right = 1
)


# 1. EFA: Unpredictability ------------------------------------------------



# We conducted an EFA including all items measuring unpredictability: 1) The QUIC items, 2) the perceived unpredictability items, 
# 3) the CHAOS items, 4) the items measuring environmental change, 5) number of partners of the father and mother, 
# 6) Number of residential changes, 7) household size. 

# The measures of residential changes and number of different partners of both parents were heavily positively skewed.


efa_data <- cleaned_data %>%
  select(matches("(quic\\d\\d)|(unp\\d\\d)|(chaos\\d\\d)|(change_env\\d\\d)"), 
         unp_partners_father_binned, unp_partners_mother_binned, unp_moving_binned) %>%
  drop_na() %>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .))


# Determine optimal number of factors
ev <- eigen(cor(efa_data)) # get eigenvalues
ap <- parallel(subject=nrow(efa_data),var=ncol(efa_data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

efa_model <- factanal(efa_data, factors = 5, rotation = "oblimin")

efa_model

efa_tidy <- tidy(efa_model) %>% 
    mutate(across(starts_with("fl"), ~ifelse(. < .32, NA, .))) %>% 
  left_join(codebook %>% rename(variable = Variable)) %>% 
  arrange(fl1, fl2, fl3, fl4, fl5) %>% 
  mutate(
    Label = case_when(
      variable == "unp_moving_binned" ~ "Residential changes", 
      variable == "unp_partners_father_binned" ~ "Romantic partners - father", 
      variable == "unp_partners_mother_binned" ~ "Romantic partners - mother", 
      variable == "unp_household_size" ~ "Household size",
      variable == "unp03" ~ "My parents had a difficult divorce or separation during this time.",
      TRUE ~ Label
    ),
    var = variable,
    variable = case_when(
      str_detect(variable, "quic") ~ "QUIC - ",
      str_detect(variable, "unp\\d\\d") ~ "Perceived - ",
      str_detect(variable, "chaos") ~ "CHAOS - ",
      str_detect(variable, "change_env") ~ "Changes - ",
      TRUE ~ ""
    )
  ) %>% 
  unite(col = "Item", c(variable, Label), sep = "") %>%
  select(var, Item, fl1, fl2, fl3, fl4, fl5) %>% 
  mutate(across(starts_with("fl"), ~round(., 2))) %>%
  rename(
    `1` = fl1,
    `2` = fl2,
    `3` = fl3,
    `4` = fl4,
    `5` = fl5
  ) 



# Extract new variables from EFA model
pilot_efa_table <- efa_tidy %>%
  rename(
    "Household\nstability/conflict" = `1`,
    "Monitoring/neglect" = `2`,
    "Macro unp." = `3`,
    "Disorganization" = `4`,
    "People in household" = `5`
  ) |> 
  select(-var) |> 
  flextable() |> 
  autofit() |> 
  bold(i = 1:24, j = 2) |> 
  bold(i = 25:37, j = 3) |>
  bold(i = 38:44, j = 4) |>
  bold(i = 45:52, j = 5) |>
  bold(i = 53:62, j = 6) |> 
  border(i = 67, border.bottom = fp_border_default(), part = "body") |> # Add APA-style bottom border
  border(i = 1, border.bottom = fp_border_default(), part = "header") |>
  border(i = 1, border.top = fp_border_default(style = "none", width = 0), part = "header") |>
  add_header_row(
    values = " ",
    colwidths = 6
  ) |> # Add a new header row on top. We can use this new row to add the title
  flextable::compose(
    i = 1, j = 1,
    as_paragraph(as_b("Table SX. "), "Exploratory Factor Analysis on unpredictability items in the Pilot."),
    part = "header"
  )
  
save(pilot_efa_table, file = "preregistrations/1_pilot/analysis_objects/supp_section2.Rdata")


# 2. HDDM model fit -------------------------------------------------------
cueing_hddm_traces <- extract_traces_2con(mcmc_cueing_mod1, chains = 3, iterations = 10000)
cueing_hddm_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_cueing_mod1, task_prefix = "cueing_", id_matches = cueing_id_matches) 

set.seed(284)

cueing_hddm_fit <- cueing_hddm_param_est %>%
  mutate(
    responses = pmap(., function(id, cueing_a, cueing_v1, cueing_v2, cueing_t1, cueing_t2) {
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=100, alpha=cueing_a, tau=cueing_t1, beta=0.5, delta=cueing_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "neutral"
          ),
        # Condition 2
        RWiener::rwiener(n=100, alpha=cueing_a, tau=cueing_t2, beta=0.5, delta=cueing_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'cued'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(id, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(id, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    cueing_clean |> 
      drop_na(rt) |> 
      mutate(condition = ifelse(condition == 1, "neutral", "cued")) |> 
      group_by(id, condition) |> 
      summarise(
        RT_25 = quantile(rt, 0.25),
        RT_50 = quantile(rt, 0.50),
        RT_75 = quantile(rt, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) 


change_hddm_traces <- extract_traces(mcmc_change_mod1, chains = 3, iterations = 10000)
change_hddm_param_est <- extract_ddm_estimates(mcmc = mcmc_change_mod1, task_prefix = "change_", id_matches = change_id_matches) 

set.seed(284)

change_mod1_fit <- change_hddm_param_est %>%
  mutate(
    responses = pmap(., function(id, change_a, change_v, change_t) {
      RWiener::rwiener(n=100, alpha=change_a, tau=change_t, beta=0.5, delta=change_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(id, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(id) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    change_clean |> 
      drop_na(rt) |> 
      group_by(id) |> 
      summarise(
        RT_25 = quantile(rt, 0.25),
        RT_50 = quantile(rt, 0.50),
        RT_75 = quantile(rt, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) 

change_mod1_fit |> group_by(percentile) |> summarise(cor = cor(RT_sim, RT))
