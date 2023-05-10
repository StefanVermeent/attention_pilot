
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(future)
library(furrr)

set.seed(43876)

n    <- c(500, 550, 600, 700, 800)
sigma <- c(0.6, 0.7, 0.8)


plan(multisession, workers = 5)

power <- 
  furrr::future_map_dfr(1:500, function(x){
    furrr::future_map_dfr(n, function(y){
      furrr::future_map_dfr(sigma, function(z){
        
        # Simulate data
        sim_data <- 
          tibble(
            id = 1:y,
            adversity = rnorm(n=y, mean = 0, sd = 1) |> scale() |> as.numeric(),
            con1    = (adversity * 0) + rnorm(y, 0, z),
            con2    = (adversity * (0.06*2)) + rnorm(y, 0, z) 
          ) |> 
          pivot_longer(c(con1, con2), names_to = "condition", values_to = "v") |> 
          mutate(
            condition = ifelse(condition == "con1", -1, 1)
          )
        
        fit <- lmerTest::lmer(data = sim_data, v ~ adversity*condition + (1|id))
        
        fit <- fit |> 
          broom.mixed::tidy() |> 
          filter(term == "adversity:condition") |> 
          mutate(
            sim  = x,
            n    = y,
            sigma = z
          )
      },.options = furrr_options(seed = TRUE))
    },.options = furrr_options(seed = TRUE))
  },.options = furrr_options(seed = TRUE))


power |> 
  group_by(n, sigma) |> 
  summarise(
    p = sum(p.value < .05)/n()*100,
    eff = mean(estimate),
    sd = sd(estimate)
  ) |> 
  ggplot(aes(n, p, color = factor(sigma), group = factor(sigma))) +
  geom_point() + 
  geom_line() +
  facet_wrap(~sigma)

