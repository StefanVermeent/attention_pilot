
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(RWiener)
library(mvtnorm)
library(flankr)
library(corrplot)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "1_DDM_objects.Rdata"))



# Explore model differences -----------------------------------------------

## T-test on boundary separation ----
t.test(cueing_DDM_results_mod1$a_cued, cueing_DDM_results_mod1$a_neutral, paired = T)

## Parameter and model fit comparisons across models ----
bind_rows(
  cueing_DDM_results_mod1 %>% mutate(iteration = "a_dependent"), 
  cueing_DDM_results_mod2 %>% mutate(a_neutral = a, a_cued = a, iteration = "a_independent") %>% select(-a)) %>%
  pivot_longer(ends_with(c("neutral", "cued")), names_to = "parameter", values_to = "value") %>%
  select(id, iteration, parameter, value) %>%
  pivot_wider(names_from = "iteration", values_from = "value") %>%
  ggplot(aes(a_dependent, a_independent)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free") +
  labs(
    title = "Parameter comparison across model versions"
  )

bind_rows(
  cueing_DDM_results_mod1 %>% mutate(iteration = "a_dependent"), 
  cueing_DDM_results_mod2 %>% mutate(iteration = "a_independent")) %>%
  select(id, iteration, fit) %>%
  pivot_wider(names_from = "iteration", values_from = "fit") %>%
  ggplot(aes(a_dependent, a_independent)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "Model fit comparison across model versions"
  )

## Parameter correlations across and within models ----
cueing_DDM_results_mod1 %>% select(v_cued, t0_cued, a_cued, v_neutral, t0_neutral, a_neutral) %>% cor %>% corrplot(method = "number")
cueing_DDM_results_mod2 %>% select(ends_with(c("cued", "neutral")), a) %>% cor %>% corrplot(method = "number")

## Comparisons between parameter values across conditions
cueing_DDM_results_mod1 %>%
  pivot_longer(ends_with(c("neutral", "cued")), names_to = "parameter", values_to = "value") %>%
  separate(parameter, c("parameter", "condition"), sep = "_") %>%
  pivot_wider(names_from = "condition", values_from = "value") %>%
  mutate(
    parameter = case_when(
      parameter == "v" ~ "Drift rate",
      parameter == "t0" ~ "Non-decision time",
      parameter == "a" ~ "Boundary separation"
    )
  ) %>%
  ggplot(aes(neutral, cued)) +
  geom_point() +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~parameter, scales = "free") +
  labs(
    title = "Model 1: Boundary separation varies across conditions"
  )

## Comparisons between parameter values across conditions
cueing_DDM_results_mod2 %>%
  mutate(a_neutral = a, a_cued = a) %>%
  select(-a) %>%
  pivot_longer(ends_with(c("neutral", "cued")), names_to = "parameter", values_to = "value") %>%
  separate(parameter, c("parameter", "condition"), sep = "_") %>%
  pivot_wider(names_from = "condition", values_from = "value") %>%
  mutate(
    parameter = case_when(
      parameter == "v" ~ "Drift rate",
      parameter == "t0" ~ "Non-decision time",
      parameter == "a" ~ "Boundary separation"
    )
  ) %>%
  ggplot(aes(neutral, cued)) +
  geom_point() +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~parameter, scales = "free") +
  labs(
    title = "Model 2: Boundary separation is fixed across conditions"
  )
