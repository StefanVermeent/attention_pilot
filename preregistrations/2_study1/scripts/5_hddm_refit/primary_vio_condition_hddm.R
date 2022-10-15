library(tidyverse)
library(forecast)

load("data/2_study1/hddm_free_std_results.RData")
load("data/2_study1/hddm_fixed_std_results.RData")
load("data/2_study1/2_cleaned_data.RData")



# Convergence checks ------------------------------------------------------

hddm_free_std_results$traces %>%
  map_df(function(x){
    x %>% mutate(n=1:n())
  }) %>%
  ggplot(aes(n,value)) +
  geom_line() +
  facet_wrap(~parm, scales = "free") +
  theme_classic()

hddm_free_std_results$traces %>%
  map_df(function(x){
    autocor <- forecast::Acf(x, 100)
    
    autocor$lag %>% 
      as_tibble() %>%
      select(Lag = V1) %>%
      bind_cols(
        autocor$acf %>%
          as_tibble() %>%
          select(ACF = V1)) %>%
      mutate(parm = unique(x$parm))
  }) %>%
  ggplot(aes(Lag, ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0)) +
  facet_wrap(~parm) +
  theme_classic()




hddm_free_std_results$parms %>%
  select(id,parameter,mean) %>%
  mutate(parameter = str_replace_all(parameter, "\\(incongruent\\)", "_incon")) %>%
  mutate(parameter = str_replace_all(parameter, "\\(congruent\\)", "_con")) %>%
  mutate(id = as.numeric(id)) %>%
  pivot_wider(names_from = 'parameter', values_from = 'mean') %>%
  select(id, v_incon, v_con, t_incon, t_con, a) %>%
  left_join(cleaned_data %>% select(id, matches('(p|t0|a|interference)_flanker_std'), acc_flanker_incongruent_std, acc_flanker_congruent_std)) %>%
  drop_na(a_flanker_std) %>%
  select(-id) %>%
  cor(method = "spearman") %>%
  corrplot::corrplot(method='number')




hddm_fixed_std_results$parms %>%
  select(id,parameter,mean) %>%
#  mutate(parameter = str_replace_all(parameter, "\\(incongruent\\)", "_incon")) %>%
#  mutate(parameter = str_replace_all(parameter, "\\(congruent\\)", "_con")) %>%
  mutate(id = as.numeric(id)) %>%
  pivot_wider(names_from = 'parameter', values_from = 'mean') %>%
 # select(id, v_incon, v_con, t_incon, t_con, a) %>%
  left_join(cleaned_data %>% select(id, acc_flanker_incongruent_std)) %>%
  drop_na(acc_flanker_incongruent_std) %>%
  select(-id) %>%
  cor(method = "spearman") %>%
  corrplot::corrplot(method='number')



