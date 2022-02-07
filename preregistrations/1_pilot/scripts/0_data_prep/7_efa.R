# Factor structure of unpredictability measures


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(broom)
library(here)
library(stats)
library(GPArotation)
library(nFactors)

load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))

# We conducted an EFA including all items measuring unpredictability: 1) The QUIC items, 2) the perceived unpredictability items, 
# 3) the CHAOS items, 4) the items measuring environmental change, 5) number of partners of the father and mother, 
# 6) Number of residential changes, 7) household size. 

# The measures of residential changes and number of different partners of both parents were heavily positively skewed.


efa_data <- cleaned_data %>%
  select(matches("(quic\\d\\d)|(unp\\d\\d)|(chaos\\d\\d)|(change_env\\d\\d)"), 
         unp_partners_father_binned, unp_partners_mother_binned, unp_household_size, unp_moving_binned) %>%
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
efa_vars <- efa_tidy %>%
  mutate(label = case_when(
    !is.na(`1`)                                                     ~ "efa_daily_unp",
    !is.na(`2`) & is.na(`1`)                                        ~ "efa_routine",
    !is.na(`3`) & is.na(`1`) & is.na(`2`)                           ~ "efa_spatial_unp",
    !is.na(`4`) & is.na(`1`) & is.na(`2`) & is.na(`3`)              ~ "efa_chaos_clutter",
    !is.na(`5`) & is.na(`1`) & is.na(`2`) & is.na(`3`) & is.na(`4`) ~ "efa_social_unp",
    TRUE ~ "other"
  )) %>%
  filter(label != "other") %>%
  select(var, label)

# Add variables to cleaned dataset

efa_variables <- cleaned_data %<>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .)) %>%
  mutate(
    efa_daily_unp     = across(efa_vars %>% filter(label == "efa_daily_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_routine       = across(efa_vars %>% filter(label == "efa_routine") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_spatial_unp   = across(efa_vars %>% filter(label == "efa_spatial_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_chaos_clutter = across(efa_vars %>% filter(label == "efa_chaos_clutter") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_social_unp    = across(efa_vars %>% filter(label == "efa_social_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
  ) %>%
  select(id, starts_with("efa"))

save(efa_vars, efa_variables, file = here("data", "1_pilot", "efa_data.Rdata"))
