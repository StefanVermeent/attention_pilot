
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(magrittr)
library(glue)

# Load data ---------------------------------------------------------------

load(here("data", "1_pilot", "0_self_report_raw.Rdata"))

source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_exclusions.R"))



# Create exclusion variables ----------------------------------------------

self_report_clean <- self_report %>%
  mutate(
    # Participants with all questionnaires missing - Non-arbitrary exclusion choice
    ex_narb_NA_selfreport_pass     = if_any(ends_with(c("total", "mean")), ~!is.na(.)),
    # Participants who missed both attention checks
    ex_narb_attention_checks_pass     = ifelse(attention_check_sum == 2, FALSE, TRUE),
    # Participants with standard deviations of 0 on 2 or more survey scales
    ex_narb_suspect_responses_pass = ifelse(across(starts_with("sd"), ~. == 0) %>% rowSums() > 1, FALSE, TRUE) 
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
  scale_y_continuous(breaks=seq(0, 40, 2)) +
  labs(
    y = "# missing items"
  )

#Subject 369 has missing responses on the majority of items across all questionnaires. Therefore, we exclude this participant from the analyses.

self_report_clean %<>%
  mutate(ex_narb_NA_selfreport_pass = ifelse(id == "369", FALSE, ex_narb_NA_selfreport_pass))


self_report_clean %>%
  summarise(across(starts_with("ex_narb"), ~sum(. == FALSE, na.rm = T))) %>%
  glue_data("{ex_narb_NA_selfreport_pass} participants were excluded because they did not complete the questionnaires;\n
             {ex_narb_attention_checks_pass} participants were excluded because they failed both attention checks;\n
             {ex_narb_suspect_responses_pass} participants were excluded because they showed suspect response patterns on more than one questionnaire.")


# Apply exclusions --------------------------------------------------------

self_report_clean %<>%
  filter(if_all(starts_with("ex_narb"), ~ . == TRUE)) %>%
  select(-starts_with("ex_arb"))


# Save data ---------------------------------------------------------------

save(self_report_clean, file = here("data", "1_pilot", "1_self_report_clean.Rdata"))

