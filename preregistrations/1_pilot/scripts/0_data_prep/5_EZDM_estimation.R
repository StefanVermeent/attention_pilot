
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(here)
library(EZ2)
library(data.table)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "1_task_data_clean.Rdata"))

# DDM helper functions
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_DDM.R"))


# Change Detection Task ---------------------------------------------------

change_EZDM_setup <- change_data_clean_average %>%
  select(acc_change, rt_var_change, rt_change, id) %>%
  mutate(acc_change = ifelse(acc_change == 1, 1-(1/(2*50)), acc_change))

change_EZDM_results <- 
  pmap(change_EZDM_setup[c("acc_change", "rt_var_change", "rt_change", "id")], function(acc_change,rt_var_change,rt_change, id) {
    
    EZ_fit <- Data2EZ(Pc = acc_change, VRT = rt_var_change, MRT = rt_change, s = 1) %>%
      as_tibble() %>%
      mutate(id = id)
  }) %>%
  bind_rows %>%
  select(
    id,
    change_EZ_v = v,
    change_EZ_a = a,
    change_EZ_t0 = Ter)


# Attention Cueing Task ---------------------------------------------------

## Procedure 2: EZ-DM

cueing_EZDM_setup <- cueing_data_clean_average %>%
  select(acc_cueing_neutral, acc_cueing_cued, rt_var_cueing_neutral, rt_var_cueing_cued, rt_cueing_neutral, rt_cueing_cued, id) %>%
  mutate(acc_cueing_neutral = ifelse(acc_cueing_neutral == 1, 1-(1/(2*32)), acc_cueing_neutral),
         acc_cueing_cued    = ifelse(acc_cueing_cued    == 1, 1-(1/(2*32)), acc_cueing_cued))

cueing_neutral_EZDM_results <- 
  pmap(cueing_EZDM_setup[c("acc_cueing_neutral", "rt_var_cueing_neutral", "rt_cueing_neutral", "id")], function(acc_cueing_neutral,rt_var_cueing_neutral,rt_cueing_neutral, id) {
    EZ_fit <- Data2EZ(Pc = acc_cueing_neutral, VRT = rt_var_cueing_neutral, MRT = rt_cueing_neutral, s = 1) %>%
      as_tibble() %>%
      mutate(id = id)
  }) %>%
  bind_rows %>%
  rename(
    cueing_neutral_EZ_v = v,
    cueing_neutral_EZ_a = a,
    cueing_neutral_EZ_T0 = Ter
  )


cueing_cued_EZDM_results <- 
  pmap(cueing_EZDM_setup[c("acc_cueing_cued", "rt_var_cueing_cued", "rt_cueing_cued", "id")], function(acc_cueing_cued,rt_var_cueing_cued,rt_cueing_cued, id) {
    EZ_fit <- Data2EZ(Pc = acc_cueing_cued, VRT = rt_var_cueing_cued, MRT = rt_cueing_cued, s = 1) %>%
      as_tibble() %>%
      mutate(id = id)
  }) %>%
  bind_rows %>%
  rename(
    cueing_cued_EZ_v = v,
    cueing_cued_EZ_a = a,
    cueing_cued_EZ_T0 = Ter
  )

cueing_EZDM_results <- full_join(cueing_neutral_EZ_results, cueing_cued_EZ_results) 

save(
  change_EZDM_results,
  cueing_EZDM_results,
  file = here("data", "1_pilot", "1_EZDM_objects.Rdata"))

