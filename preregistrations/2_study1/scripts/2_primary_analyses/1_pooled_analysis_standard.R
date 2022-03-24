# This script contains code used to address Aim 1 of the preregistration:
# Investigate the robustness of the primary DDM findings in the pilot study 
# by pooling the pilot data and the data of the standard condition in the current study. 



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)



# Load data ---------------------------------------------------------------

# Pilot study
load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
pilot_data <- cleaned_data %>%
  select(id, vio_comp = violence_composite, rt_flanker_congruent, rt_flanker_incongruent, a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker) %>%
  mutate(
    id = str_c("pilot_", id),
    study = -1,
    vio_comp_c = scale(vio_comp) %>% as.numeric()
    )


# Study 1
load(here("data", "2_study1", "2_cleaned_data.Rdata"))
study1_data <- cleaned_data %>%
  select(id, vio_comp, rt_flanker_congruent_standard, rt_flanker_incongruent_standard, 
         a_flanker_std, t0_flanker_std, p_flanker_std, rd_flanker_std, sda_flanker_std) %>%
  rename(rt_flanker_congruent = rt_flanker_congruent_standard,
         rt_flanker_incongruent = rt_flanker_incongruent_standard,
         a_flanker = a_flanker_std,
         t0_flanker = t0_flanker_std,
         p_flanker = p_flanker_std,
         rd_flanker = rd_flanker_std,
         sda_flanker = sda_flanker_std) %>%
  mutate(
    id = str_c("study1_", id),
    study = 1,
    vio_comp_c = scale(vio_comp) %>% as.numeric
    )

# Combine data of pilot study and study 1
pooled_data <- bind_rows(pilot_data, study1_data)

t.test(data = pooled_data, a_flanker~study)
t.test(data = pooled_data, t0_flanker~study)
t.test(data = pooled_data, p_flanker~study)
t.test(data = pooled_data, rd_flanker~study)
t.test(data = pooled_data, sda_flanker~study)


# Multiverse -- SSP parameters --------------------------------------------

