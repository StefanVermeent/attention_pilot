
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(future)
library(furrr)

set.seed(43876)

# Simulation settings -----------------------------------------------------

p_mean            <- 0.519
p_sd              <- 0.157
interference_mean <- 111.387
interference_sd   <-  58.165


# 1. Pooled Main Effects --------------------------------------------------

n    <- c(300,350,400,450,500, 600, 1500, 1600)
beta <- c(0.05, 0.1)

plan(multisession, workers = 5)

power <- furrr::future_map_dfr(1:500, function(x){
  furrr::future_map_dfr(n, function(y){
    furrr::future_map_dfr(beta, function(z){
      
      # Simulate data
      sim_data <- 
        tibble(
          id = 1:y,
          adversity = rnorm(n=y, mean = 0, sd = 1) |> scale() |> as.numeric(),
          p_flanker = p_mean + (adversity * z) + rnorm(y, 0, 2*p_sd) 
        )
      
      # Fit model
      lm(data = sim_data, p_flanker ~ adversity) |> 
        broom::tidy() |> 
        filter(term == "adversity") |> 
        mutate(
          sim  = x,
          n    = y,
          beta = z
        )
    },.options = furrr_options(seed = TRUE))
  },.options = furrr_options(seed = TRUE))
},.options = furrr_options(seed = TRUE))


power |> 
  group_by(n,beta) |> 
  summarise(
    p = sum(p.value < .05)/n()*100,
    eff = mean(estimate),
    sd = sd(estimate)
  )

