# Packages ----------------------------------------------------------------
library(tidyverse)
library(qualtRics)
library(jsonlite)

# Functions ---------------------------------------------------------------
source("scripts/functions-create_codebook.R")

# Data --------------------------------------------------------------------
pilot_data <- 
  fetch_survey(
    surveyID = "SV_2tsChV0wnzp0LH0", 
    verbose  = T,
    force_request = T,
    label = F,
    convert = F
  ) %>% 
  rename_with(tolower) %>% 
  mutate(id = 1:n()) %>% 
  sjlabelled::var_labels(
    id = "Blinded participant ID"
  ) 

# meta data ---------------------------------------------------------------
vars01_meta <- 
  pilot_data %>% 
  rename(
    meta_duration = `duration (in seconds)`,
    meta_start    = startdate,
    meta_end      = enddate,
    meta_recorded = recordeddate,
    meta_finished = finished,
    meta_captcha  = q_recaptchascore,
  ) %>% 
  select(id, starts_with("meta_"))


# Current state -----------------------------------------------------------

vars02_state <- 
  pilot_data %>%
  select(id, starts_with('stai_s'), sick, meal, hungry, sleep, rested) %>%
  mutate(
    stai_s_mean = across(matches("stai_s")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1), items = ., mini = 1, maxi = 4) %>% rowSums(., na.rm = T)
  )

# Unpredictability --------------------------------------------------------
vars03_unp <- 
  pilot_data %>% 
  select(id,starts_with(c("chaos", "unp", "quic"))) %>% 
  mutate(
    unp_mean                 = across(matches("unp\\d\\d")) %>% rowMeans(., na.rm = T),
    unp_missing              = across(matches("unp\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    chaos_mean               = across(matches("chaos")) %>% psych::reverse.code(keys = c(-1,-1,1,-1,1,1,-1,1,1,1,1,-1,1,1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    chaos_missing            = across(matches("chaos")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_total_sum           = across(matches("quic")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,1,1,1,1,1,1,-1,1,1,1,1,1,1,-1,1,1,1,1,1), items = ., mini = 0, maxi = 1) %>% rowMeans(., na.rm = T),
    quic_total_missing       = across(matches("quic")) %>% is.na() %>% rowSums(., na.rm = T),    
    
    quic_monitoring_sum      = across(matches("quic_monitoring")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1), items = ., mini = 0, maxi = 1) %>% rowMeans(., na.rm = T),
    quic_monitoring_missing  = across(matches("quic_monitoring")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_predict_sum     = across(matches("quic_par_predict")) %>% psych::reverse.code(keys = c(1,-1,1,1,-1,1,-1,1,1,1,1,1), items = ., mini = 0, maxi = 1) %>% rowMean(., na.rm = T),
    quic_par_predict_missing = across(matches("quic_par_predict")) %>% is.na() %>% rowSums(., na.rm = T),
     
    quic_par_env_sum         = across(matches("quic_par_env")) %>% psych::reverse.code(keys = c(1,1,1,1,-1,1,1), items = ., mini = 0, maxi = 1) %>% rowMeans(., na.rm = T),
    quic_par_env_missing     = across(matches("quic_par_env")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_phys_env_sum        = across(matches("quic_phys_env")) %>% psych::reverse.code(keys = c(1,1,1,1-1,1,1), items = ., mini = 0, maxi = 1) %>% rowMeans(., na.rm = T),
    quic_phys_env_missing    = across(matches("quic_phys_env")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_safety_sum          = across(matches("quic_safety")) %>% rowMeans(., na.rm = T),
    quic_safety_missing      = across(matches("quic_safety")) %>% is.na() %>% rowSums(., na.rm = T)
  ) 

# Violence ----------------------------------------------------------------
vars04_vio <- 
  aut_data %>% 
  select(id,matches("violence\\d\\d")) %>% 
  mutate(
    violence_mean    = across(matches("violence\\d\\d$")) %>% psych::reverse.code(keys = c(-1,1,-1,1,1,1,1)) %>% rowMeans(., na.rm = T),
    violence_missing = across(matches("violence\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )

# SES ---------------------------------------------------------------------
vars05_ses <- 
  pilot_data %>% 
  select(id,matches("ses\\d\\d")) %>% 
  mutate(
    ses_mean    = across(matches("ses\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    ses_missing = across(matches("ses\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )


# Temporal Orientation ----------------------------------------------------

vars06_temp_orientation <- 
  pilot_data %>% 
  select(id,starts_with(c('impuls', 'fut_orient'))) %>% 
  mutate(
    impuls_mean    = across(matches("impuls")) %>% rowMeans(., na.rm = T),
    impuls_missing = across(matches("impuls")) %>% is.na() %>% rowSums(., na.rm = T),
    
    #'Planning Ahead' Subscale
    fos_pa_mean         = across(matches("fos(01|06|07|12|13)")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    fos_pa_missing      = across(matches("fos(01|06|07|12|13)")) %>% is.na() %>% rowSums(., na.rm = T),
    # 'Time Perspective' subscale
    fos_tp_mean         = across(matches("fos(02|05|08|11|14)")) %>% psych::reverse.code(keys = c(1,1,-1,-1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    fos_tp_missing      = across(matches("fos(02|05|08|11|14)")) %>% is.na() %>% rowSums(., na.rm = T),
    # 'Anticipation of Future Consequences' subscale
    fos_fc_mean         = across(matches("fos(03|04|09|10|15)")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    fos_fc_missing      = across(matches("fos(03|04|09|10|15)")) %>% is.na() %>% rowSums(., na.rm = T),
    # 'Future Orientation Scale' (total of all items)
    fos_fo_mean         = across(matches("fos(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15)")) %>% psych::reverse.code(keys = c(-1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    fos_fo_missing      = across(matches("fos(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15)")) %>% is.na() %>% rowSums(., na.rm = T),  
  )


# Depressive Symptoms -----------------------------------------------------

vars08_dep <- 
  pilot_data %>% 
  select(id, matches("depression\\d\\d$")) %>% 
  mutate(
    depression_mean    = across(matches("depression\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,1), items = ., mini = 1, maxi = 4) %>% rowMeans(., na.rm = T),
    depression_missing = across(matches("depression\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )

# Demographics ------------------------------------------------------------
vars09_dems <- 
  pilot_data %>% 
  select(id,starts_with("dems_ethnicity")) %>%
  pivot_longer(-id, names_to = "option", values_to = "value") %>%
  drop_na(value) %>%
  mutate(
    dems_ethnicity = case_when(
      option == "dems_ethnicity_1" ~ "Asian or Asian American (e.g., Chinese, Japanese, and others)",
      option == "dems_ethnicity_2" ~ "Black or African American",
      option == "dems_ethnicity_3" ~ "Hispanic of Latino (e.g., Mexican American, Central American, and others)",
      option == "dems_ethnicity_4" ~ "White, Caucasian, Anglo, European American",
      option == "dems_ethnicity_5" ~ "Native American/Alaskan Native/indigenous",
      option == "dems_ethnicity_6" ~ "Native Hawaiian or other Pacific Islander",
      option == "dems_ethnicity_7" ~ "Filipino",
      option == "dems_ethnicity_8" ~ "Middle Eastern",
      option == "dems_ethnicity_9" ~ "Other",
      option == "dems_ethnicity_9_text" ~ as.character(value),
      option == "dems_ethnicity_10" ~ "Prefer not to say",
    )
  ) %>%
  group_by(id) %>%
  summarise(dems_ethnicity = str_c(dems_ethnicity, collapse = " & ")) %>%
  mutate(dems_ethnicity_mixed = ifelse(str_detect(dems_ethnicity, "\\s&\\s"), 1, 0)) %>%
  left_join(pilot_data %>% select(id, matches("dems_(age|sex|gender|born|english|class|edu|occupation|income|colorblind)"), weight, height, activity))

# Attention ---------------------------------------------------------------
vars10_att <- 
  pilot_data %>% 
  select(id,starts_with("att")) %>% 
  mutate(
    attention_sum = (ifelse(att_check01 == 5, 0, 1) + ifelse(att_check02 == 2, 0, 1))
  )

# Admin -------------------------------------------------------------------
vars11_admin <- 
  pilot_data %>% 
  select(ends_with("id"))

# Creativity --------------------------------------------------------------
## Raw AUT ----
vars11_tasks_raw <- 
  pilot_data %>% 
  select(id, tasks_data) %>% 
  #filter(str_detect(tasks_data, "^\\[\\{\"view_history")) %>% 
  mutate(
    #aut = str_replace(aut_data,"\\.\\.\\.$","ETHAN") %>% stringi::stri_replace_last(regex = ",", "ETHAN") %>% str_replace("ETHAN.*ETHAN$",'\\}\\]'),
    #aut = str_replace(aut, "\\}\\}\\]$", "\\}\\]"),
    tasks_data = ifelse(!is.na(pilot_data), map(tasks_data, function(x) jsonlite::fromJSON(x)), NA)
  ) %>% 
  select(id, aut) %>% 
  unnest(aut) %>% 
  filter(str_detect(variable, "(towel|paperclip|fork)_(responses|rank|rerank)")) %>% 
  select(id, variable, trial_index, use, rt, time_elapsed, timed_out, choice, selected, browser)

## Clean up AUT ----
vars12_aut <- 
  vars11_aut_raw %>% 
  filter(str_detect(variable,"responses")) %>% 
  mutate(
    n_commas = str_count(use,"\\,"),
    use_list = ifelse(n_commas > 1 & timed_out, map(use, function(x) x %>% str_split(",",simplify = T) %>% str_squish()), list("none"))
  ) %>% 
  unnest(use_list) %>% 
  group_by(id, variable) %>% 
  mutate(
    response_order = 1:n(),
    rt_rolling     = ifelse(timed_out, rt, cumsum(rt)),
    finished       = sum(timed_out, na.rm = T) == 1,
    rt_max         = max(rt_rolling),
    response_raw   = use,
    response       = ifelse(use_list == "none", use, use_list),
    rt             = ifelse(use_list == "none", rt, NA),
    item           = str_remove_all(variable, "^aut_|_responses$")
  ) %>%
  ungroup() %>% 
  select(id, finished, item, rt, rt_rolling, rt_max, response_order, response_raw, response) 




# Cleaned Data ------------------------------------------------------------
## Self-report scales ----
aut_self_report <- 
  reduce(
    ls() %>% str_subset("^vars0\\d|^vars10") %>% map(function(x) eval(as.symbol(x))),
    left_join,
    by = "id"
  )

## Creativity Data ----
aut_task <- vars14_prepped

# Save Data ---------------------------------------------------------------
save(aut_self_report, aut_task, file = "data/clean/aut-online-data.Rdata")

# Write Data --------------------------------------------------------------
write_csv(create_codebook(aut_self_report), "aut-self-report-codebook.csv")
write_csv(aut_self_report, "data/clean/aut-self-report.csv")
write_csv(aut_task, "data/clean/aut-task.csv")
write_csv(aut_task %>% filter(item == "fork"), "data/clean/aut-fork.csv")
write_csv(aut_task %>% filter(item == "paperclip"), "data/clean/aut-paperclip.csv")
write_csv(aut_task %>% filter(item == "towel"), "data/clean/aut-towel.csv")
