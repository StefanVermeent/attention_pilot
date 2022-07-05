# Factor structure of unpredictability measures


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(broom)
library(here)
library(stats)
library(GPArotation)
library(nFactors)

# Load pilot data
load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "0_self_report_raw.Rdata"))

cleaned_data_pilot <- cleaned_data %>%
  rename(
  unp_male_fig_rom_binned = unp_partners_mother_binned,
  unp_female_fig_rom_binned = unp_partners_father_binned 
)

source(here("preregistrations", "1_pilot", "scripts", "3_exploratory_analyses", "1_efa.R"))

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
load(here("data", "2_study1", "2_cleaned_data.Rdata"))
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
  

efa_data_combined %>%
  mutate(
    Factor = case_when(
      Factor == "f1" ~ "Factor 1 - Household stability/conflict",
      Factor == "f2" ~ "Factor 2 - Monitoring/neglect",
      Factor == "f3" ~ "Factor 3 - Macro unpredictability",
      Factor == "f4" ~ "Factor 4 - Clutter/disorganization",
      Factor == "f5" ~ "Factor 5 - People in household",
    )
  ) %>%
  ggplot(aes(Loading_pilot, Loading_study1, color = cutoff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = var), nudge_y = 0.05, nudge_x = 0.05) +
  theme_classic() +
  scale_color_manual(values = c("#198B27", "black", "darkgrey")) +
  facet_wrap(~Factor) +
  labs(
    x = "\nFactor Loading - Pilot study",
    y = "Factor Loading - Study 1\n",
    color = "Loading > .32"
    )

# Compute variables based on EFA for pilot data and join with study data
pilot_efa_vars <- efa_data_combined %>%
  select(var, Factor, Loading_pilot) %>%
  mutate(Loading_pilot = ifelse(Loading_pilot <= .32, NA, Loading_pilot)) %>%
  drop_na(Loading_pilot) %>%
  pivot_wider(names_from = "Factor", values_from = "Loading_pilot") %>%
  mutate(label = case_when(
    !is.na(f1)                                                 ~ "efa_stability_conflict",
    !is.na(f2) & is.na(f1)                                     ~ "efa_monitoring_neglect",
    !is.na(f3) & is.na(f1) & is.na(f2)                         ~ "efa_spatial_unp",
    !is.na(f4) & is.na(f1) & is.na(f2) & is.na(f3)             ~ "efa_clutter_disorganization",
    !is.na(f5) & is.na(f1) & is.na(f2) & is.na(f3) & is.na(f4) ~ "efa_social_unpredictability",
    TRUE ~ "other"
  ))

cleaned_data_pilot %<>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .)) %>%
  mutate(
    efa_daily_unp     = across(pilot_efa_vars %>% filter(label == "efa_stability_conflict") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_routine       = across(pilot_efa_vars %>% filter(label == "efa_monitoring_neglect") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_spatial_unp   = across(pilot_efa_vars %>% filter(label == "efa_spatial_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_chaos_clutter = across(pilot_efa_vars %>% filter(label == "efa_clutter_disorganization") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_social_unp    = across(pilot_efa_vars %>% filter(label == "efa_social_unpredictability") %>% pull(var)) %>% rowMeans(., na.rm = T),
  )


# Compute variables based on EFA for pilot data and join with study data
study1_efa_vars <- efa_data_combined %>%
  select(var, Factor, Loading_study1) %>%
  mutate(Loading_study1 = ifelse(Loading_study1 <= .32, NA, Loading_study1)) %>%
  drop_na(Loading_study1) %>%
  pivot_wider(names_from = "Factor", values_from = "Loading_study1") %>%
  mutate(label = case_when(
    !is.na(f1)                                                 ~ "efa_stability_conflict",
    !is.na(f2) & is.na(f1)                                     ~ "efa_monitoring_neglect",
    !is.na(f3) & is.na(f1) & is.na(f2)                         ~ "efa_spatial_unp",
    !is.na(f4) & is.na(f1) & is.na(f2) & is.na(f3)             ~ "efa_clutter_disorganization",
    !is.na(f5) & is.na(f1) & is.na(f2) & is.na(f3) & is.na(f4) ~ "efa_social_unpredictability",
    TRUE ~ "other"
  ))

cleaned_data_study1 %<>%
  mutate(
    efa_daily_unp     = across(study1_efa_vars %>% filter(label == "efa_stability_conflict") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_routine       = across(study1_efa_vars %>% filter(label == "efa_monitoring_neglect") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_spatial_unp   = across(study1_efa_vars %>% filter(label == "efa_spatial_unp") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_chaos_clutter = across(study1_efa_vars %>% filter(label == "efa_clutter_disorganization") %>% pull(var)) %>% rowMeans(., na.rm = T),
    efa_social_unp    = across(study1_efa_vars %>% filter(label == "efa_social_unpredictability") %>% pull(var)) %>% rowMeans(., na.rm = T),
  )

cleaned_data_pilot %>%
  select(starts_with("efa"), vio_comp = violence_composite, unp_comp = unpredictability_composite) %>%
  cor(., use = "complete.obs") %>%
  corrplot::corrplot(method = "number")

cleaned_data_study1 %>%
  select(starts_with("efa"), vio_comp, unp_comp) %>%
  cor(., use = "complete.obs") %>%
  corrplot::corrplot(method = "number")

