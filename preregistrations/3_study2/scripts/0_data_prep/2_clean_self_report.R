
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "3_study2", "0_self_report_raw.Rdata"))

source(here("preregistrations", "3_study2", "scripts", "custom_functions", "functions_exclusions.R"))


# Create exclusion variables ----------------------------------------------

self_report_clean <- self_report %>%
  mutate(
    # Participants with all questionnaires missing - Non-arbitrary exclusion choice
    ex_narb_NA_selfreport_pass     = if_any(ends_with(c("total", "mean")), ~!is.na(.)),
    # Participants who missed both attention checks
    ex_narb_attention_checks_pass  = ifelse(attention_check_sum == 2, FALSE, TRUE),
    # Participants with standard deviations of 0 on 2 or more survey scales
    ex_narb_suspect_responses_pass = ifelse(across(starts_with("sd"), ~. == 0) %>% rowSums() > 1, FALSE, TRUE),
    # Screen resolution width is smaller than height -- possibly not performed on desktop/laptop
    ex_narb_resolution_ratio       = ifelse(meta_resolution_ratio < 1, FALSE, TRUE),
    # Screen resolution height is smaller than 700px
    ex_narb_resolution_height      = ifelse(meta_resolution_height < 700, FALSE, TRUE)
  )

self_report_clean %>%
  filter(ex_narb_NA_selfreport_pass) %>%
  select(id, ends_with("missing")) %>%
  filter(if_any(-id, ~ . > 0)) %>%
  pivot_longer(-id, names_to = "Scale", values_to = "missing") %>%
  ggplot(aes(Scale, missing, color = factor(id), group = factor(id))) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(breaks=seq(0, 5,1)) +
  labs(
    y = "# missing items"
  )

# Only two subjects have 1 missing response on a questionnaire.


self_report_clean %>%
  summarise(across(starts_with("ex_narb"), ~sum(. == FALSE, na.rm = T))) %>%
  glue_data("{ex_narb_NA_selfreport_pass} participants were excluded because they did not complete the questionnaires;\n
             {ex_narb_attention_checks_pass} participants were excluded because they failed both attention checks;\n
             {ex_narb_suspect_responses_pass} participants were excluded because they showed suspect response patterns on more than one questionnaire;\n
             {ex_narb_resolution_ratio} participants were excluded due to an incorrect screen resolution ratio;\n
             {ex_narb_resolution_height} participants were excluded because their screen height was less than 700px.")


# Apply exclusions --------------------------------------------------------

self_report_clean %<>%
  filter(if_all(starts_with("ex_narb"), ~ . == TRUE)) %>%
  select(-starts_with("ex_arb"))



# Data fixes --------------------------------------------------------------

# Based on participant feedback

  
# Save data ---------------------------------------------------------------

save(self_report_clean, file = here("data", "3_study2", "1_self_report_clean.Rdata"))

