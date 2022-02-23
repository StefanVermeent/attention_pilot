library(tidyverse)
library(here)
library(flankr)
library(furrr)
library(parallel)

load(here("data", "1_pilot", "2_cleaned_data.Rdata"))

cores <- detectCores()- 2

plan('multisession')


predicted_quantiles <- cleaned_data %>%
  select(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker, id) %>%
  drop_na() %>%
  filter(rd_flanker > 0) %>%
  mutate(
    sim_data = future_pmap(., function(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker, id) {
      
      simulateSSP(parms = c(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker), nTrials = 50000) %>%
        mutate(id=id) %>%
        group_by(id, congruency) %>%
        summarise(
          prop_acc_pred = sum(accuracy == 1) / n(),
          quan_rt_pred  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
        ) %>%
        unnest(quan_rt_pred) %>%
        group_by(congruency)  %>%
        mutate(quantile = c("25th Percentile", "50th Percentile", "75th Percentile"))
    })
  ) %>%
  select(sim_data) %>%
  unnest(sim_data)


observed_quantiles <- cleaned_data %>%
  select(flanker_data_long) %>%
  unnest(flanker_data_long) %>%
  group_by(id, congruency) %>%
  summarise(
    prop_acc_obs = sum(correct == 1) / n(),
    quan_rt_obs  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
  ) %>%
  unnest(quan_rt_obs) %>%
  group_by(id, congruency)  %>%
  mutate(
    quantile = c("25th Percentile", "50th Percentile", "75th Percentile"),
    quan_rt_obs  = quan_rt_obs / 1000) 
  

qq_data <- left_join(predicted_quantiles, observed_quantiles)

ggplot(qq_data) +
  geom_point(aes(quan_rt_obs, quan_rt_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(congruency~quantile) +
  labs(
    x = "\nObserved",
    y = "Predicted\n"
  )

