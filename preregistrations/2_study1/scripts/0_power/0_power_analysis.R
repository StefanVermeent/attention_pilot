library(tidyverse)
library(faux)
library(broom)
library(broom.mixed)
library(ggeffects)
library(interactions)
library(effectsize)
library(magrittr)
    

nsim = 1:100
nsubject = c(300, 400, 500, 600)
sigma_sd = 1

power_results <- tibble(
  N = NA,
  sim = NA,
  p_enh = NA,
  p_deg = NA
)


for (n in nsubject) {
  for (i in nsim) {
    
    data <- add_random(id = n) %>%
      add_within("id", condition = c("std", "enh", "deg")) %>%
      # add by-subject random intercept
      add_ranef("id", intercept_std = 1) %>%
      # add error term
      add_ranef(sigma = sigma_sd) %>%
      add_contrast("condition", "sum", colnames = c("condition_sum_enh", "condition_sum_deg"), 
                   levels = c("enh", "deg", "std")) %>%
      add_ranef("id", vio_comp = 1) %>%
      mutate(
        intercept_enh = intercept_std + 0.5 + rnorm(1, 0, 0.5),
        intercept_deg = intercept_std - 1.2 + rnorm(1, 0, 0.5),
        
        # Perceptual input
        p = case_when(
          condition == 'std' ~ intercept_std + -0.1 * vio_comp + sigma,
          condition == 'enh' ~ intercept_enh + -0.2 * vio_comp + sigma,
          condition == 'deg' ~ intercept_deg + 0.1 * vio_comp + sigma
        )
      )
    
  #  ggplot(data, aes(vio_comp, p, color = condition)) + geom_point() + geom_smooth(method = "lm")
    
    mod <- lmerTest::lmer(data = data, p ~ vio_comp * condition_sum_enh + vio_comp*condition_sum_deg + (1|id)) %>%
      broom.mixed::tidy()
    
    p_vio_enh <- mod %>% filter(term == "vio_comp:condition_sum_enh") %>% pull(p.value)
    p_vio_deg <- mod %>% filter(term == "vio_comp:condition_sum_deg") %>% pull(p.value)
    
    
    power_results %<>%
      add_row(N = n, sim = i, p_enh = p_vio_enh, p_deg = p_vio_deg)
  }
}


power_plot <- power_results %>%
  group_by(N) %>%
  summarise(
    Enhanced = sum(p_enh < .05) / n(),
    Degraded = sum(p_deg < .05) / n()
  ) %>%
  pivot_longer(c(Enhanced, Degraded), names_to = 'Condition', values_to = 'Power') %>%
  ggplot(aes(N, Power, group = Condition, color = Condition)) +
  geom_point() +
  geom_line()



