library(tidyverse)
library(here)
library(flankr)
library(furrr)
library(parallel)
library(RWiener)

load("data/1_pilot/2_cleaned_data.Rdata")

cores <- detectCores()- 4

plan('multisession', workers = cores)



# Flanker Task - SSP Model ------------------------------------------------

predicted_quantiles_flanker <- cleaned_data %>%
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


observed_quantiles_flanker <- cleaned_data %>%
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
  



ggsave(qq_flanker_rt, file = "qq_plot_flanker_rt.png", height = 4, width = 6)
ggsave(qq_flanker_acc, file = "qq_plot_flanker_acc.png", height = 4, width = 6)


save(predicted_quantiles_flanker, observed_quantiles_flanker, qq_data_flanker, file = "data/1_pilot/mod_fit_flanker.RData")


# 2. Flanker Task -- DDM Model with KS estimation -------------------------


predicted_quantiles_flanker <- cleaned_data %>%
  select(flanker_con_ks_a, flanker_con_ks_v, flanker_con_ks_t0,
         flanker_incon_ks_a, flanker_incon_ks_v, flanker_incon_ks_t0,id) %>%
  pivot_longer(
    -matches('id'),
    names_to = c("condition", ".value"),
    names_pattern = "(flanker_.*_ks_)(.*)"
  ) %>%
  mutate(
    condition = ifelse(str_detect(condition, "incon"), "incongruent", "congruent")
  ) %>%
  drop_na() %>%
  distinct() %>%
  mutate(
    sim_data = pmap(., function(a, v, t0, condition, id) {
      
      RWiener::rwiener(n = 5000, alpha = a, tau = t0, beta = 0.5, delta = v) %>%
        as_tibble() %>%
        mutate(id=id, condition=condition) %>%
        group_by(id, condition) %>%
        summarise(
          prop_acc_pred = sum(resp == 'upper') / n(),
          quan_rt_pred  = list(quantile(q, probs = seq(0.25, 0.75, 0.25)))
        ) %>%
        unnest(quan_rt_pred) %>%
        group_by(condition)  %>%
        mutate(quantile = c("25th Percentile", "50th Percentile", "75th Percentile"))
    })
  ) %>%
  select(sim_data) %>%
  unnest(sim_data)



observed_quantiles_flanker <- cleaned_data %>%
  select(flanker_data_long) %>%
  unnest(flanker_data_long) %>%
  group_by(id) %>%
  summarise(
    prop_acc_obs = sum(correct == 1) / n(),
    quan_rt_obs  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
  ) %>%
  unnest(quan_rt_obs) %>%
  group_by(id)  %>%
  mutate(
    quantile = c("25th Percentile", "50th Percentile", "75th Percentile"),
    quan_rt_obs  = quan_rt_obs / 1000) 


qq_data_flanker <- left_join(predicted_quantiles_flanker, observed_quantiles_flanker)

qq_flanker_rt <- ggplot(qq_data_flanker) +
  geom_point(aes(quan_rt_obs, quan_rt_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(condition~quantile) +
  labs(
    x = "\nObserved RT",
    y = "Predicted RT\n",
    title = "Flanker task"
  ) +
  theme_classic()

qq_flanker_acc <- ggplot(qq_data_flanker) +
  geom_point(aes(prop_acc_obs, prop_acc_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(condition~quantile) +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "\nObserved Acc",
    y = "Predicted Acc\n"
  ) +
  theme_classic()

ggsave(qq_cueing_rt, file = "qq_plot_cueing_rt.png", height = 4, width = 6)
ggsave(qq_cueing_acc, file = "qq_plot_cueing_acc.png", height = 4, width = 6)



# 3. Change Detection Task -- DDM Model with ML Estimation ----------------

predicted_quantiles_change <- cleaned_data %>%
  select(change_ml_a, change_ml_v, change_ml_t0, id) %>%
  drop_na() %>%
  filter(id %in% 1) %>%
  mutate(
    sim_data = future_pmap(., function(change_ml_a, change_ml_v, change_ml_t0, id) {
      
      print(id)
      RWiener::rwiener(n = 5000, alpha = change_ml_a, tau = change_ml_t0, beta = 0.5, delta = change_ml_v) %>%
        as_tibble() %>%
        mutate(id=id) %>%
        group_by(id) %>%
        summarise(
          prop_acc_pred = sum(resp == 'upper') / n(),
          quan_rt_pred  = list(quantile(q, probs = seq(0.25, 0.75, 0.25)))
        ) %>%
        unnest(quan_rt_pred) %>%
        mutate(quantile = c("25th Percentile", "50th Percentile", "75th Percentile"))
    }, .options = furrr_options(seed = TRUE))
  ) %>%
  select(sim_data) %>%
  unnest(sim_data)


observed_quantiles_change <- cleaned_data %>%
  select(change_data_long) %>%
  unnest(change_data_long) %>%
  group_by(id) %>%
  summarise(
    prop_acc_obs = sum(correct == 1) / n(),
    quan_rt_obs  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
  ) %>%
  unnest(quan_rt_obs) %>%
  group_by(id)  %>%
  mutate(
    quantile = c("25th Percentile", "50th Percentile", "75th Percentile"),
    quan_rt_obs  = quan_rt_obs / 1000) 



qq_data_change <- left_join(predicted_quantiles_change, observed_quantiles_change)


qq_change_rt <- ggplot(qq_data_change) +
  geom_point(aes(quan_rt_obs, quan_rt_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~quantile) +
  labs(
    x = "\nObserved RT",
    y = "Predicted RT\n",
    title = "Change Detection"
  ) +
  theme_classic()

qq_change_acc <- ggplot(qq_data_change) +
  geom_point(aes(prop_acc_obs, prop_acc_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~quantile) +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "\nObserved Acc",
    y = "Predicted Acc\n"
  ) +
  theme_classic()

ggsave(qq_change_rt, file = "qq_plot_change_rt.png", height = 4, width = 6)
ggsave(qq_change_acc, file = "qq_plot_change_acc.png", height = 4, width = 6)


save(qq_data_change, observed_quantiles_change, predicted_quantiles_change, file = "data/1_pilot/mod_fit_change.RData")


# 3. Change Detection Task -- DDM Model with KS Estimation ----------------

###



# 5. Cued Attention Task -- DDM with ML Estimation ------------------------

predicted_quantiles_cueing <- cleaned_data %>%
  select(cueing_cued_ml_a, cueing_cued_ml_v, cueing_cued_ml_t0,
         cueing_neutral_ml_a, cueing_neutral_ml_v, cueing_neutral_ml_t0,id) %>%
  pivot_longer(
    -matches('id'),
    names_to = c("condition", ".value"),
    names_pattern = "(cueing_.*_ml_)(.*)"
  ) %>%
  mutate(
    condition = ifelse(str_detect(condition, "cued"), "cued", "neutral")
  ) %>%
  drop_na() %>%
  mutate(
    sim_data = pmap(., function(a, v, t0, condition, id) {

      RWiener::rwiener(n = 5000, alpha = a, tau = t0, beta = 0.5, delta = v) %>%
        as_tibble() %>%
        mutate(id=id, condition=condition) %>%
        group_by(id, condition) %>%
        summarise(
          prop_acc_pred = sum(resp == 'upper') / n(),
          quan_rt_pred  = list(quantile(q, probs = seq(0.25, 0.75, 0.25)))
        ) %>%
        unnest(quan_rt_pred) %>%
        group_by(condition)  %>%
        mutate(quantile = c("25th Percentile", "50th Percentile", "75th Percentile"))
    })
  ) %>%
  select(sim_data) %>%
  unnest(sim_data)



observed_quantiles_cueing <- cleaned_data %>%
  select(cueing_data_long) %>%
  unnest(cueing_data_long) %>%
  group_by(id) %>%
  summarise(
    prop_acc_obs = sum(correct == 1) / n(),
    quan_rt_obs  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
  ) %>%
  unnest(quan_rt_obs) %>%
  group_by(id)  %>%
  mutate(
    quantile = c("25th Percentile", "50th Percentile", "75th Percentile"),
    quan_rt_obs  = quan_rt_obs / 1000) 


qq_data_cueing <- left_join(predicted_quantiles_cueing, observed_quantiles_cueing)

qq_cueing_rt <- ggplot(qq_data_cueing) +
  geom_point(aes(quan_rt_obs, quan_rt_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~quantile) +
  labs(
    x = "\nObserved RT",
    y = "Predicted RT\n",
    title = "Attention Cueing"
  ) +
  theme_classic()

qq_cueing_acc <- ggplot(qq_data_cueing) +
  geom_point(aes(prop_acc_obs, prop_acc_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~quantile) +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "\nObserved Acc",
    y = "Predicted Acc\n"
  ) +
  theme_classic()

ggsave(qq_cueing_rt, file = "qq_plot_cueing_rt.png", height = 4, width = 6)
ggsave(qq_cueing_acc, file = "qq_plot_cueing_acc.png", height = 4, width = 6)


## 6. Cued Attention Task -- DDM with KS estimation ---------------------

predicted_quantiles_cueing <- cleaned_data %>%
  select(cueing_cued_ks_a, cueing_cued_ks_v, cueing_cued_ks_t0,
         cueing_neutral_ks_a, cueing_neutral_ks_v, cueing_neutral_ks_t0,id) %>%
  pivot_longer(
    -matches('id'),
    names_to = c("condition", ".value"),
    names_pattern = "(cueing_.*_ks_)(.*)"
  ) %>%
  mutate(
    condition = ifelse(str_detect(condition, "cued"), "cued", "neutral")
  ) %>%
  drop_na() %>%
  mutate(
    sim_data = pmap(., function(a, v, t0, condition, id) {
      
      RWiener::rwiener(n = 5000, alpha = a, tau = t0, beta = 0.5, delta = v) %>%
        as_tibble() %>%
        mutate(id=id, condition=condition) %>%
        group_by(id, condition) %>%
        summarise(
          prop_acc_pred = sum(resp == 'upper') / n(),
          quan_rt_pred  = list(quantile(q, probs = seq(0.25, 0.75, 0.25)))
        ) %>%
        unnest(quan_rt_pred) %>%
        group_by(condition)  %>%
        mutate(quantile = c("25th Percentile", "50th Percentile", "75th Percentile"))
    })
  ) %>%
  select(sim_data) %>%
  unnest(sim_data)



observed_quantiles_cueing <- cleaned_data %>%
  select(cueing_data_long) %>%
  unnest(cueing_data_long) %>%
  group_by(id) %>%
  summarise(
    prop_acc_obs = sum(correct == 1) / n(),
    quan_rt_obs  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
  ) %>%
  unnest(quan_rt_obs) %>%
  group_by(id)  %>%
  mutate(
    quantile = c("25th Percentile", "50th Percentile", "75th Percentile"),
    quan_rt_obs  = quan_rt_obs / 1000) 


qq_data_cueing <- left_join(predicted_quantiles_cueing, observed_quantiles_cueing)

qq_cueing_rt <- ggplot(qq_data_cueing) +
  geom_point(aes(quan_rt_obs, quan_rt_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~quantile) +
  labs(
    x = "\nObserved RT",
    y = "Predicted RT\n",
    title = "Attention Cueing"
  ) +
  theme_classic()

qq_cueing_acc <- ggplot(qq_data_cueing) +
  geom_point(aes(prop_acc_obs, prop_acc_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~quantile) +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "\nObserved Acc",
    y = "Predicted Acc\n"
  ) +
  theme_classic()

ggsave(qq_cueing_rt, file = "qq_plot_cueing_rt.png", height = 4, width = 6)
ggsave(qq_cueing_acc, file = "qq_plot_cueing_acc.png", height = 4, width = 6)


save(predicted_quantiles_cueing, observed_quantiles_cueing, qq_data_cueing, file = "data/1_pilot/mod_fit_cueing.RData")
