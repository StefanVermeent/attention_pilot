
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(flextable)
library(here)

cleaned_data <- read_csv(here("data", "1_pilot", "2_cleaned_data.csv"))

ethnicity <- cleaned_data %>%
  select(id, starts_with("dems")) %>%
  transmute(var = ifelse(dems_ethnicity_mixed == 1, "Mixed", dems_ethnicity)) %>%
  group_by(var) %>%
  summarise(
    percentage = round((n() / nrow(cleaned_data)) * 100, 1),
  ) %>%
  arrange(desc(percentage))

education <- cleaned_data %>%
  select(id, dems_edu) %>%
  transmute(
    var = case_when(
      dems_edu == 1 ~ "Some high school",
      dems_edu == 2 ~ "GED",
      dems_edu == 3 ~ "High school diploma",
      dems_edu == 4 ~ "Some college but no college degree",
      dems_edu == 5 ~ "Associate's degree",
      dems_edu == 6 ~ "Bachelor's or RN degree",
      dems_edu == 7 ~ "Master's degree",
      dems_edu == 8 ~ "Doctoral or law degree"
    )
  ) %>%
  mutate(var = factor(var, levels = c("Some high school", "GED", "High school diploma", "Some college but no college degree", "Associate's degree", "Bachelor's or RN degree", "Master's degree", "Doctoral or law degree"))) %>%
  group_by(var) %>%
  summarise(percentage = round((n() / nrow(cleaned_data)) * 100, 1)) 


gender <- cleaned_data %>%
  select(id, dems_gender) %>% 
  transmute(
    var = case_when(
      dems_gender == 1 ~ "Man",
      dems_gender == 2 ~ "Woman",
      dems_gender == 3 ~ "Non-binary",
      dems_gender == 4 ~ "Other",
      dems_gender == 5 ~ "Prefer not to say"
    )
  ) %>%
  group_by(var) %>%
  summarise(percentage = round((n() / nrow(cleaned_data)) * 100, 1)) %>%
  arrange(desc(percentage))


sex_at_birth <- cleaned_data %>%
  select(id, dems_sex) %>% 
  transmute(
    var = case_when(
      dems_sex == 0 ~ "Male",
      dems_sex == 1 ~ "Female",
      dems_sex == 2 ~ "Intersex",
      dems_sex == 3 ~ "Prefer not to say"
    )
  ) %>%
  group_by(var) %>%
  summarise(percentage = round((n() / nrow(cleaned_data)) * 100, 1)) %>%
  arrange(desc(percentage))

social_class <- cleaned_data %>%
  select(id, dems_class_current) %>% 
  transmute(
    var = case_when(
      dems_class_current == 1 ~ "Poor",
      dems_class_current == 2 ~ "Working class",
      dems_class_current == 3 ~ "Middle class",
      dems_class_current == 4 ~ "Upper-middle class",
      dems_class_current == 5 ~ "Upper class"
    ),
    var = factor(var, levels = c("Poor", "Working class", "Middle class", "Upper-middle class", "Upper class"))
  ) %>%
  group_by(var) %>%
  summarise(percentage = round((n() / nrow(cleaned_data)) * 100, 1))


demo_table_data <- bind_rows(sex_at_birth, gender, ethnicity, education, social_class)
