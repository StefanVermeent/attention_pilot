library(tidyverse)
library(magrittr)
library(broom)
library(here)
library(lmerTest)
library(stats)
library(GPArotation)
library(nFactors)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(ggeffects)
library(interactions)
library(parameters)



# 1. EFA analysis of unpredictability items ----------------------------------

# Load pilot data
load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))

cleaned_data_pilot <- cleaned_data %>%
  rename(
    unp_male_fig_rom_binned = unp_partners_mother_binned,
    unp_female_fig_rom_binned = unp_partners_father_binned 
  )

source("preregistrations/1_pilot/scripts/3_exploratory_analyses.R")

efa_tidy_pilot <- efa_tidy %>%
  rename(
    pilot_f1 = `1`,
    pilot_f2 = `2`,
    pilot_f3 = `3`,
    pilot_f4 = `4`,
    pilot_f5 = `5`
  ) %>%
  mutate(
    var = ifelse(var == "unp_partners_mother_binned", "unp_male_fig_rom_binned", var),
    var = ifelse(var == "unp_partners_father_binned", "unp_female_fig_rom_binned", var)
  ) %>%
  select(-Item)


# We conducted an EFA including all items measuring unpredictability: 1) The QUIC items, 2) the perceived unpredictability items, 
# 3) the CHAOS items, 4) the items measuring environmental change, 5) number of partners of the father and mother, 
# 6) Number of residential changes, 7) household size. 

# The measures of residential changes and number of different partners of both parents were heavily positively skewed.
cleaned_data <- read_csv("data/2_study1/2_cleaned_data.csv")
load(here("data", "2_study1", "0_self_report_raw.Rdata"))

cleaned_data_study1 <- cleaned_data

efa_data <- cleaned_data %>%
  select(matches("(quic\\d\\d)|(unp\\d\\d)|(chaos\\d\\d)|(change_env\\d\\d)"), 
         unp_male_fig_rom_binned, unp_female_fig_rom_binned, unp_moving_binned) %>%
  drop_na() 


# Determine optimal number of factors
ev <- eigen(cor(efa_data)) # get eigenvalues
ap <- parallel(subject=nrow(efa_data),var=ncol(efa_data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

efa_model <- factanal(efa_data, factors = 5, rotation = "oblimin")

efa_model

efa_tidy_study1 <- tidy(efa_model) %>% 
  #  mutate(across(starts_with("fl"), ~ifelse(. < .32, NA, .))) %>% 
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
    study1_f1 = fl1,
    study1_f2 = fl2,
    study1_f3 = fl4,
    study1_f4 = fl3,
    study1_f5 = fl5
  ) 


# Plot factor loadings
efa_data_combined <- full_join(
  efa_tidy_pilot %>%
    pivot_longer(starts_with("pilot"), names_to = "Factor", values_to = "Loading_pilot") %>%
    mutate(Factor = str_replace_all(Factor, "pilot_", "")),
  
  efa_tidy_study1 %>%
    pivot_longer(starts_with("study1"), names_to = "Factor", values_to = "Loading_study1") %>%
    mutate(Factor = str_replace_all(Factor, "study1_", ""))
) %>%
  mutate(cutoff = case_when(
    Loading_pilot > .32 & Loading_study1 <= .32 ~ "Pilot only",
    Loading_pilot <= .32 & Loading_study1 > .32 ~ "Study 1 only",
    Loading_pilot > .32 & Loading_study1 > .32 ~ "Both studies",
    Loading_pilot <= .32 & Loading_study1 <= .32 ~ "Neither study"
  )) %>%
  filter(cutoff != "Neither study") %>%
  mutate(cutoff = factor(cutoff, levels = c("Both studies", "Pilot only", "Study 1 only"))) 


study1_efa_fig <- efa_data_combined %>%
  mutate(
    Factor = case_when(
      Factor == "f1" ~ "Household stability/conflict",
      Factor == "f2" ~ "Monitoring/neglect",
      Factor == "f3" ~ "Macro unp.",
      Factor == "f4" ~ "Clutter/disorganization",
      Factor == "f5" ~ "People in household",
    )
  ) %>%
  ggplot(aes(x = Loading_pilot, y = Loading_study1, color = cutoff)) +
  geom_point() +
  geom_text_repel(aes(label = var)) +
  geom_smooth(method = "lm") +
  theme_classic() +
  scale_color_manual(values = c("#198B27", "black", "darkgrey")) +
  facet_wrap(~Factor) +
  labs(
    x = "\nFactor Loading - Pilot study",
    y = "Factor Loading - Study 1\n",
    color = "Loading > .32"
  )

save(study1_efa_fig, file = "preregistrations/2_study1/analysis_objects/supp_section2.Rdata")
