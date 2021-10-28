# Packages ----------------------------------------------------------------
library(tidyverse)
library(qualtRics)
library(jsonlite)
library(here)

# Functions ---------------------------------------------------------------
source("scripts/custom_functions/create_codebook.R")

# Data --------------------------------------------------------------------
pilot_data <- 
  fetch_survey(
    surveyID = "SV_2tsChV0wnzp0LH0", 
    verbose  = T,
    force_request = T,
    label = F,
    convert = F,
    add_var_labels = F
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
    stai_s_sum = across(matches("stai_s")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1), items = ., mini = 0, maxi = 3) %>% rowSums(., na.rm = T)
  )

# Unpredictability --------------------------------------------------------
vars03_unp <- 
  pilot_data %>% 
  select(id,starts_with(c("chaos", "unp", "quic"))) %>% 
  mutate(
    unp_mean                 = across(matches("unp\\d\\d")) %>% rowMeans(., na.rm = T),
    unp_missing              = across(matches("unp\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    chaos_mean               = across(matches("chaos")) %>% psych::reverse.code(keys = c(-1,-1,1,-1,1,1,-1,1,1,1,1,-1,1,1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    chaos_missing            = across(matches("chaos")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_monitoring_mean     = across(matches("quic(01|02|03|04|05|06|07|08|09)")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    quic_monitoring_missing  = across(matches("quic(01|02|03|04|05|06|07|08|09)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_predict_mean    = across(matches("quic(10|11|12|13|14|15|16|17|18|19|20|21)")) %>% psych::reverse.code(keys = c(1,-1,1,1,-1,1,-1,1,1,1,1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    quic_par_predict_missing = across(matches("quic(10|11|12|13|14|15|16|17|18|19|20|21)")) %>% is.na() %>% rowSums(., na.rm = T),
     
    quic_par_env_mean        = across(matches("quic(22|23|24|25|26|27|28)")) %>% psych::reverse.code(keys = c(1,1,1,1,-1,1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    quic_par_env_missing     = across(matches("quic(22|23|24|25|26|27|28)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_phys_env_mean       = across(matches("quic(29|30|31|32|33|34|35)")) %>% psych::reverse.code(keys = c(1,1,1,1-1,1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    quic_phys_env_missing    = across(matches("quic(29|30|31|32|33|34|35)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_safety_mean         = across(matches("quic(36|37|38)")) %>% rowMeans(., na.rm = T),
    quic_safety_missing      = across(matches("quic(36|37|38)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_total_mean          = across(matches("quic(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33|34|35|36|37|38)")) %>% 
                               psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,1,1,1,1,1,1,-1,1,1,1,1,1,1,-1,1,1,1,1,1), items = ., mini = 1, maxi = 5) %>% 
                               rowMeans(., na.rm = T) %>% round(1),
    quic_total_missing       = across(matches("quic(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33|34|35|36|37|38)")) %>% 
                               is.na() %>% rowSums(., na.rm = T),    
  ) 

# Violence ----------------------------------------------------------------
vars04_vio <- 
  pilot_data %>% 
  select(id,matches("violence\\d\\d"), aces_fighting1, aces_fighting2) %>% 
  mutate(
    violence_mean    = across(matches("violence\\d\\d$")) %>% psych::reverse.code(keys = c(-1,1,-1,1,1,1,1)) %>% rowMeans(., na.rm = T) %>% round(1),
    violence_missing = across(matches("violence\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T),
    violence_z = scale(violence_mean) %>% as.numeric(),
    fighting1_z = scale(aces_fighting1) %>% as.numeric(),
    fighting2_z = scale(aces_fighting2) %>% as.numeric(),
    violence_total = mean(c(violence_z, fighting1_z, fighting2_z), na.rm = T)
  )

# SES ---------------------------------------------------------------------
vars05_ses <- 
  pilot_data %>% 
  select(id,matches("ses\\d\\d")) %>% 
  mutate(
    ses_mean    = across(matches("ses\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    ses_missing = across(matches("ses\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )


# Temporal Orientation ----------------------------------------------------

vars06_temp_orientation <- 
  pilot_data %>% 
  select(id,starts_with(c('impuls', 'fos'))) %>% 
  mutate(
    impuls_mean    = across(matches("impuls")) %>% rowMeans(., na.rm = T) %>% round(1),
    impuls_missing = across(matches("impuls")) %>% is.na() %>% rowSums(., na.rm = T),
    
    #'Planning Ahead' Subscale
    fos_pa_mean         = across(matches("fos(01|06|07|12|13)")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    fos_pa_missing      = across(matches("fos(01|06|07|12|13)")) %>% is.na() %>% rowSums(., na.rm = T),
    # 'Time Perspective' subscale
    fos_tp_mean         = across(matches("fos(02|05|08|11|14)")) %>% psych::reverse.code(keys = c(1,1,-1,-1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    fos_tp_missing      = across(matches("fos(02|05|08|11|14)")) %>% is.na() %>% rowSums(., na.rm = T),
    # 'Anticipation of Future Consequences' subscale
    fos_fc_mean         = across(matches("fos(03|04|09|10|15)")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    fos_fc_missing      = across(matches("fos(03|04|09|10|15)")) %>% is.na() %>% rowSums(., na.rm = T),
    # 'Future Orientation Scale' (total of all items)
    fos_fo_mean         = across(matches("fos(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15)")) %>% psych::reverse.code(keys = c(-1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T) %>% round(1),
    fos_fo_missing      = across(matches("fos(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15)")) %>% is.na() %>% rowSums(., na.rm = T),  
  )


# Depressive Symptoms -----------------------------------------------------

vars08_dep <- 
  pilot_data %>% 
  select(id, matches("depression\\d\\d$")) %>% 
  mutate(
    depression_mean    = across(matches("depression\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,1), items = ., mini = 1, maxi = 4) %>% rowSums(., na.rm = T),
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


# Flanker Task ------------------------------------------------------------

flanker_data <- 
  pilot_data %>% 
  select(id, data_flanker) %>% 
  drop_na(starts_with("data")) %>%
  mutate(data_flanker = map(data_flanker, jsonlite::fromJSON) ) %>%
  unnest(data_flanker) %>%
  select(-c(view_history, internal_node_id, stimulus)) %>%
  filter(!variable %in% c('welcome', 'instructions', 'practice_start', 'practice', 'practice_finish', 'end')) %>%
  mutate(correct = ifelse(correct, 1, 0)) 

change_data <- 
  pilot_data %>% 
  select(id, data_change) %>% 
  drop_na(starts_with("data")) %>%
  mutate(data_change = map(data_change, jsonlite::fromJSON) ) %>%
  unnest(data_change) %>%
  select(-c(view_history, trial_type, internal_node_id, stimulus, response_type, avg_frame_time, center_x, center_y, starts_with(c('test', 'stim', 'mem')))) %>%
  filter(!variable %in% c(c("welcome", "instructions", "practice_start", "practice", "practice_finish", "break", "end"))) %>%
  mutate(correct = ifelse(correct, 1, 0)) 

cueing_data <- 
  pilot_data %>% 
  select(id, data_cueing) %>% 
  drop_na(starts_with("data")) %>%
  mutate(data_cueing = map(data_cueing, jsonlite::fromJSON)) %>%
  unnest(data_cueing) %>%
  select(-c(view_history, trial_type, internal_node_id, stimulus, response_type, center_x, center_y, target, starts_with(c('cue', 'target')))) %>%
  filter(!variable %in% c("welcome", "instructions", "practice_start", "practice", "practice_finish", "break", "end")) %>%
  mutate(correct = ifelse(correct, 1, 0)) 

# DDM datasets
ddm_change <- change_data %>%
  select(id, correct, rt) %>%
  mutate(rt = rt/1000)
  write_csv(here("data", "1_pilot_data", "clean", "DDM_change.csv")) 

ddm_cueing <- cueing_data %>%
  select(id, correct, rt) %>%
  mutate(rt = rt/1000)

ddm_flanker <- flanker_data %>%
  select(
    subject = id,
    congruency = stimtype,
    accuracy = correct,
    rt = rt
  ) %>%
  mutate(
    rt = rt/1000,
    congruency = ifelse(str_detect(congruency, "^congruent"), "congruent", "incongruent")
    )



# Cleaned Data ------------------------------------------------------------
## Self-report scales ----
pilot_self_report <- 
  reduce(
    ls() %>% str_subset("^vars0\\d|^vars10|^vars11") %>% map(function(x) eval(as.symbol(x))),
    left_join,
    by = "id"
  )

## Creativity Data ----
#aut_task <- vars14_prepped

# Save Data ---------------------------------------------------------------
save(pilot_self_report, file = here("data", "1_pilot_data", "clean", "pilot_online_data.Rdata"))

# Write Data --------------------------------------------------------------
codebook <- create_codebook(pilot_self_report) %>%
  mutate(Label =  ifelse(str_detect(Variable, "^(meta|stai|chaos|unp|quic|violence|ses|impuls|fos|depression)"), 
                        str_replace_all(Label, pattern = "^(.*|.*\\n.*)\\s-\\s", replacement = ""),
                        'no'))

write_csv(codebook, here("data", "1_pilot_data", "clean", "pilot_self_report_codebook.csv"))
write_csv(pilot_self_report, here("data", "1_pilot_data", "clean", "pilot_self_report.csv"))

write_csv(aut_task, "data/clean/aut-task.csv")
write_csv(aut_task %>% filter(item == "fork"), "data/clean/aut-fork.csv")
write_csv(aut_task %>% filter(item == "paperclip"), "data/clean/aut-paperclip.csv")
write_csv(aut_task %>% filter(item == "towel"), "data/clean/aut-towel.csv")
