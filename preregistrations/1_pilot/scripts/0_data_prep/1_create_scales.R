# Packages ----------------------------------------------------------------
library(tidyverse)
library(qualtRics)
library(jsonlite)
library(here)
library(sjlabelled)

# Functions ---------------------------------------------------------------
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "create_codebook.R"))
source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_exclusions.R"))

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
  ) %>%
  filter(finished==1)


# Self-report -------------------------------------------------------------

## Non-recoded standard deviations in item responses ----

pilot_data <- response_sd(pilot_data, "stai")
pilot_data <- response_sd(pilot_data, "chaos")
pilot_data <- response_sd(pilot_data, "quic")
pilot_data <- response_sd(pilot_data, "violence")
pilot_data <- response_sd(pilot_data, "ses")
pilot_data <- response_sd(pilot_data, "fos")
pilot_data <- response_sd(pilot_data, "depression")


## meta data ----

vars01_meta <- 
  pilot_data %>% 
  rename(
    meta_duration       = `duration (in seconds)`,
    meta_start          = startdate,
    meta_end            = enddate,
    meta_recorded       = recordeddate,
    meta_finished       = finished,
    meta_captcha        = q_recaptchascore,
    meta_feedback       = feedback
  ) %>%
  separate(meta_resolution, into = c("meta_resolution_width", "meta_resolution_height"), sep = "x") %>%
  mutate(
    meta_resolution_height = as.numeric(meta_resolution_height),
    meta_resolution_width  = as.numeric(meta_resolution_width),
    meta_resolution_ratio  = meta_resolution_width / meta_resolution_height) %>%
  mutate(
    meta_task_duration        = timestamp_tasks - timestamp_consent,
    meta_state_duration       = timestamp_state - timestamp_tasks,
    meta_ace_duration         = timestamp_ace - timestamp_state,
    meta_temp_orient_duration = timestamp_temp_orient - timestamp_ace,
    meta_psychopath_duration  = timestamp_psychopath - timestamp_temp_orient,
    meta_dems_duration        = timestamp_dems - timestamp_psychopath,
    
    meta_task_duration_z        = scale(meta_task_duration),
    meta_state_duration_z       = scale(meta_state_duration),
    meta_ace_duration_z         = scale(meta_ace_duration),
    meta_temp_orient_duration_z = scale(meta_temp_orient_duration),
    meta_psychopath_duration_z  = scale(meta_psychopath_duration),
    meta_dems_duration_z        = scale(meta_dems_duration)
  ) %>% 
  select(id, starts_with("meta_"))


## Current state ----

vars02_state <- 
  pilot_data %>%
  select(id, starts_with('stai_s'), sd_stai, sick, meal, hungry, sleep, rested) %>%
  mutate(
    stai_s_mean = across(matches("stai_s")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1), items = ., mini = 1, maxi = 4) %>% rowMeans(., na.rm = T),
    stai_s_missing = across(matches("stai_s")) %>% is.na() %>% rowSums(., na.rm = T)
  ) %>%
  sjlabelled::var_labels(
    stai_s_mean = "STAI - State"
  )

## Unpredictability ----

vars03_unp <- 
  pilot_data %>% 
  select(id,starts_with(c("chaos", "unp", "quic", "change_env")), sd_quic, sd_chaos) %>% 
  # Fix data based on subject feedback
  mutate(
    unp03 = ifelse(id == "145", NA, unp03)
  ) %>%
  mutate(
    unp_mean                 = across(matches("unp\\d\\d")) %>% rowMeans(., na.rm = T),
    unp_missing              = across(matches("unp\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    change_env_mean          = across(matches("change_env\\d\\d")) %>% rowMeans(., na.rm = T),
    change_env_missing       = across(matches("change_env\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    chaos_mean               = across(matches("chaos\\d\\d")) %>% psych::reverse.code(keys = c(-1,-1,1,-1,1,1,-1,1,1,1,1,-1,1,-1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    chaos_missing            = across(matches("chaos\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_monitoring_mean     = across(matches("quic(01|02|03|04|05|06|07|08|09)")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    quic_monitoring_missing  = across(matches("quic(01|02|03|04|05|06|07|08|09)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_predict_mean    = across(matches("quic(10|11|12|13|14|15|16|17|18|19|20|21)")) %>% psych::reverse.code(keys = c(1,-1,1,1,-1,1,-1,1,1,1,1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    quic_par_predict_missing = across(matches("quic(10|11|12|13|14|15|16|17|18|19|20|21)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_env_mean        = across(matches("quic(22|23|24|25|26|27)")) %>% psych::reverse.code(keys = c(-1,1,1,1,1,1), items = ., mini = 0, maxi = 5) %>% rowMeans(., na.rm = T),
    quic_par_env_missing     = across(matches("quic(22|23|24|25|26|27)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_phys_env_mean       = across(matches("quic(28|29|30|31|32|33|34)")) %>% psych::reverse.code(keys = c(1,1,1,1,-1,1,1), items = ., mini = 0, maxi = 5) %>% rowMeans(., na.rm = T),
    quic_phys_env_missing    = across(matches("quic(28|29|30|31|32|33|34)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_safety_mean         = across(matches("quic(35|36|37)")) %>% rowMeans(., na.rm = T),
    quic_safety_missing      = across(matches("quic(35|36|37)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_total_mean          = across(matches("quic(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33|34|35|36|37)")) %>% 
      psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,1,1,-1,1,1,1,1,1,1,1,1,1,-1,1,1,1,1,1), items = ., mini = 1, maxi = 5) %>% 
      rowMeans(., na.rm = T),
    quic_total_missing       = across(matches("quic(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33|34|35|36|37)")) %>% 
      is.na() %>% rowSums(., na.rm = T),   
    unp_moving_binned = case_when(
      unp_moving == 0 ~ 0,
      unp_moving %in% c(1,2) ~ 1,
      unp_moving %in% c(3,4) ~ 2,
      unp_moving %in% c(5,6) ~ 3,
      unp_moving %in% c(7,8) ~ 4,
      unp_moving %in% c(9,10) ~ 5,
      unp_moving > 10 ~ 6,
    ),
    unp_partners_mother_binned = case_when(
      unp_partners_mother == 0 ~ 0,
      unp_partners_mother == 1 ~ 1,
      unp_partners_mother == 2 ~ 2,
      unp_partners_mother == 3 ~ 3,
      unp_partners_mother == 4 ~ 4,
      unp_partners_mother == 5 ~ 5,
      unp_partners_mother >= 6 ~ 6,
    ),
    unp_partners_father_binned = case_when(
      unp_partners_father == 0 ~ 0,
      unp_partners_father == 1 ~ 1,
      unp_partners_father == 2 ~ 2,
      unp_partners_father == 3 ~ 3,
      unp_partners_father == 4 ~ 4,
      unp_partners_father == 5 ~ 5,
      unp_partners_father >= 6 ~ 6,
    ),
    
    unpredictability_subj      = across(c(unp_mean, chaos_mean, quic_total_mean)) %>% rowMeans(., na.rm = T) %>% scale,
    unpredictability_obj       = across(c(unp_moving_binned, unp_partners_mother_binned, unp_partners_father_binned, change_env_mean)) %>% scale %>% rowMeans(., na.rm = T),
    unpredictability_composite = across(c(unpredictability_subj, unpredictability_obj)) %>% rowMeans(., na.rm = T)
  ) %>%
  sjlabelled::var_labels(
    unp_mean              = "Perceived unpredictability",
    change_env_mean       = "Environmental changes",
    chaos_mean            = "CHAOS",
    quic_monitoring_mean  = "QUIC - Parental monitoring",
    quic_par_predict_mean = "QUIC - Parental predictability",
    quic_par_env_mean     = "QUIC - Parental environment",
    quic_phys_env_mean    = "QUIC - Physical environment",
    quic_safety_mean      = "QUIC - Safety and security",
    quic_total_mean       = "QUIC - Total"
  )

## Violence ----

vars04_vio <- 
  pilot_data %>% 
  select(id,matches("violence\\d\\d"), aces_fighting1, aces_fighting2, sd_violence) %>% 
  mutate(
    violence_mean      = across(matches("violence\\d\\d$")) %>% psych::reverse.code(keys = c(-1,1,-1,1,1,1,1)) %>% rowMeans(., na.rm = T),
    violence_missing   = across(matches("violence\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T),
    fighting_mean      = across(matches("aces_fighting\\d")) %>% rowMeans(., na.rm = T),
    violence_composite = across(c(violence_mean, fighting_mean)) %>% scale %>% rowMeans(., na.rm = TRUE)
  ) %>%
  sjlabelled::var_labels(
    violence_mean = "NVS",
    fighting_mean = "Fighting",
    violence_composite = "Violence - Composite"
  )


## SES ----

vars05_ses <- 
  pilot_data %>% 
  select(id,matches("ses\\d\\d"), dems_edu_mother, dems_edu_father, dems_income_past, dems_class_past, sd_ses) %>% 
  mutate(
    dems_income_past      = ifelse(dems_income_past == 7, NA, dems_income_past),
    ses_subj_mean         = across(matches("ses\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    ses_subj_missing      = across(matches("ses\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T),
    ses_subj_mean_recoded = across(matches("ses\\d\\d$")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    
    edu_parents_recoded   = across(c(dems_edu_mother, dems_edu_father)) %>% psych::reverse.code(keys = c(-1,-1), items = ., mini = 1, maxi = 8) %>% rowMeans(., na.rm = TRUE),
    income_past_recoded   = across(dems_income_past) %>% psych::reverse.code(keys = c(-1), items = ., mini = 1, maxi = 6) %>% mean(., na.rm = TRUE),
    class_past_recoded    = across(dems_class_past) %>% psych::reverse.code(keys = c(-1), items = ., mini = 1, maxi = 5) %>% mean(., na.rm = TRUE),
    
    ses_obj_mean          = across(c(edu_parents_recoded, income_past_recoded, class_past_recoded)) %>% scale %>% rowMeans(., na.rm = TRUE),
    ses_obj_missing       = across(c(edu_parents_recoded, income_past_recoded, class_past_recoded)) %>% is.na %>% rowSums(., na.rm = TRUE),
    
    poverty_composite     = across(c(ses_subj_mean_recoded, ses_obj_mean)) %>% scale %>% rowMeans(., na.rm = TRUE)
  ) %>%
  sjlabelled::var_labels(
    ses_subj_mean = "Perceived SES",
    edu_parents_recoded = "Parental education - Recoded",
    income_past_recoded = "Childhood income - Recoded",
    class_past_recoded = "Childhood social class - Recoded",
    ses_obj_mean  = "Objective SES - Composite",
    poverty_composite = "Poverty - Composite"
  )

## Temporal Orientation ----


vars06_temp_orientation <- 
  pilot_data %>% 
  select(id,starts_with(c('impuls', 'fos')), sd_fos) %>% 
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
  ) %>%
  sjlabelled::var_labels(
    impuls_mean = "Impulsivity",
    fos_pa_mean = "FOS - Planning ahead",
    fos_tp_mean = "FOS - Time perspective",
    fos_fc_mean = "FOS - Future consequences",
    fos_fo_mean = "FOS - Total"
  )


## Depressive Symptoms ----

vars08_dep <- 
  pilot_data %>% 
  select(id, matches("depression\\d\\d$"), sd_depression) %>% 
  mutate(
    depression_mean    = across(matches("depression\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,1), items = ., mini = 1, maxi = 4) %>% rowMeans(., na.rm = T),
    depression_missing = across(matches("depression\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  ) %>%
  sjlabelled::var_labels(
    depression_mean = "CES-D"
  )

## Demographics ----

vars09_dems <- 
  pilot_data %>% 
  select(id,starts_with("dems_ethnicity")) %>%
  mutate(across(contains("ethnicity"), as.character)) %>%
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
  left_join(pilot_data %>% select(id, matches("dems_(age|sex|gender|born|english|class_current|edu$|occupation|income$|colorblind)"), weight, height, activity)) %>%
  # Fix data based on subject feedback
  mutate(
    dems_colorblind = ifelse(id == "413", 0, dems_colorblind)
  )

## Attention ----

vars10_att <- 
  pilot_data %>% 
  select(id,starts_with("att")) %>% 
  mutate(
    attention_check_sum     = (ifelse(att_check01 == 5, 0, 1) + ifelse(att_check02 == 2, 0, 1)),
    attention_interrupt_sum = att_getup + att_interrupted
  )

## Admin ----

vars11_admin <- 
  pilot_data %>% 
  select(ends_with("id"))



# Attention Tasks ---------------------------------------------------------


## Flanker data ----

flanker_data <- 
  pilot_data %>% 
  select(id, data_flanker) %>% 
  mutate(data_flanker = map_if(.x = data_flanker, .p = ~!is.na(.x), .f = jsonlite::fromJSON)) %>%
  mutate(data_flanker = ifelse(is.na(data_flanker), tibble(x=NA), data_flanker)) %>%
  unnest(data_flanker) %>%
  filter(!variable %in% c('welcome', 'instructions', 'practice_start', 'practice', 'practice_finish', 'end')) %>%
  mutate(
    correct = ifelse(correct, 1, 0),
    congruency = ifelse(str_detect(stimtype, "^congruent"), "congruent", "incongruent")) %>%
  select(-c(internal_node_id, stimulus, stimtype))

NA_placeholder <- tribble(
  ~rt,              ~response_type,   ~key_press,        ~response,         ~avg_frame_time,   ~center_x,       ~center_y,        ~variable,        
  ~task,            ~condition,       ~correct_response, ~mem_stim1_x,      ~mem_stim2_x,      ~mem_stim3_x,    ~mem_stim4_x,     ~mem_stim5_x,     
  ~mem_stim1_y,     ~mem_stim2_y,     ~mem_stim3_y,      ~mem_stim4_y,      ~mem_stim5_y,      ~test_stim1_x,   ~test_stim2_x,    ~test_stim3_x,    
  ~test_stim4_x,    ~test_stim5_x,    ~test_stim1_y,     ~test_stim2_y,     ~test_stim3_y,     ~test_stim4_y,   ~test_stim5_y,    ~stim1_color,     
  ~stim2_color,     ~stim3_color,     ~stim4_color,      ~stim5_color,      ~trial_type,       ~trial_index,    ~time_elapsed,    ~internal_node_id,
  ~counterbalance,  ~correct,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
  NA, NA, NA, NA, NA
)

## Change data ----

change_data <- 
  pilot_data %>% 
  select(id, data_change_block1, data_change_block2) %>% 
  #drop_na(data_change_block1, data_change_block2) %>%
  mutate(across(c(data_change_block1, data_change_block2), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) %>%
  mutate(across(c(data_change_block1, data_change_block2), ~ifelse(is.na(.), list(NA_placeholder), .))) %>%
  mutate(data_change = map2(data_change_block1, data_change_block2, ~bind_rows(.x, .y))) %>%
  unnest(data_change) %>% 
  select(-data_change_block1, -data_change_block2) %>%
  select(-c(trial_type, internal_node_id, response_type, avg_frame_time, center_x, center_y, starts_with(c('test', 'stim', 'mem')))) %>%
  mutate(
    correct = ifelse(correct, 1, 0)) 

## Cueing data ----

cueing_data <- 
  pilot_data %>% 
  select(id, data_cueing) %>% 
  mutate(data_cueing = map_if(.x = data_cueing, .p = ~!is.na(.x), .f = jsonlite::fromJSON)) %>%
  mutate(data_cueing = ifelse(is.na(data_cueing), tibble(x=NA), data_cueing)) %>%
  unnest(data_cueing) %>%
  select(-c(trial_type, internal_node_id, stimulus, response_type, center_x, center_y, target, starts_with(c('cue', 'target')))) %>%
  filter(!variable %in% c("welcome", "instructions", "practice_start", "practice", "practice_finish", "break", "end")) %>%
  mutate(correct = ifelse(correct, 1, 0)) 

## Browser interactions ----

browser_interactions <- 
  bind_rows(
    pilot_data %>%
      select(id, tasks_browser) %>%
      drop_na(tasks_browser) %>%
      mutate(tasks_browser = map(tasks_browser, jsonlite::fromJSON)) %>%
      unnest(tasks_browser),
    reduce(
      ls() %>% str_subset("^change_data$|^cueing_data$|^flanker_data$") %>% map(function(x) {
        eval(as.symbol(x)) %>%
          select(id, task, time_elapsed) %>% 
          group_by(id, task) %>% 
          summarise(
            "time_start_{x}" := min(time_elapsed),
            "time_end_{x}"   := max(time_elapsed)
          )}
      ),
      left_join,
      by = "id", "task") %>%
      pivot_wider(names_from = task.x, names_sep = ".", values_from = c(time_start_change_data, time_end_change_data)) %>%
      rename(
        time_start_change_block1 = time_start_change_data.change_block1,
        time_end_change_block1   = time_end_change_data.change_block1,
        time_start_change_block2 = time_start_change_data.change_block2,
        time_end_change_block2   = time_end_change_data.change_block2
      ) %>%
      select(-c(time_start_change_data.NA, time_end_change_data.NA), -starts_with("task"))
  ) %>% 
  group_by(id) %>%
  fill(starts_with("time_"), .direction = "up") %>%
  ungroup() %>%
  select(-trial) %>%
  drop_na(event) %>%
  # Did a blur event occur during a task?
  mutate(
    event_during_cueing  = ifelse((event == "blur") & (time > time_start_cueing_data & time < time_end_cueing_data), TRUE, FALSE),
    event_during_flanker = ifelse((event == "blur") & (time > time_start_flanker_data & time < time_end_flanker_data), TRUE, FALSE),
    event_during_change  = ifelse((event == "blur") & ((time > time_start_change_block1 & time < time_end_change_block1) |
                                                       (time > time_start_change_block2 & time < time_end_change_block2)), TRUE, FALSE)
  )
                                                              

browser_interactions_summary <- browser_interactions %>%
  group_by(id) %>%
  summarise(
    fullscreenenter      = sum(event == "fullscreenenter", na.rm=T),
    fullscreenexit       = sum(event == "fullscreenexit", na.rm=T),
    blur_event           = sum(event == "blur", na.rm=T),
    focus_event          = sum(event == "focus", na.rm = T),
    event_during_cueing  = ifelse(any(event_during_cueing == TRUE), TRUE, FALSE),
    event_during_change  = ifelse(any(event_during_change == TRUE), TRUE, FALSE),
    event_during_flanker = ifelse(any(event_during_flanker == TRUE), TRUE, FALSE)
  )

## Screen resizing ----

resize_screen <- 
  pilot_data %>%
  select(id, data_resize) %>%
  drop_na(data_resize) %>%
  mutate(data_resize = map(data_resize, jsonlite::fromJSON)) %>%
  unnest(data_resize) %>%
  select(id, final_width_px, scale_factor)


# Cleaned Data ------------------------------------------------------------

## Self-report scales ----
self_report <- 
  reduce(
    ls() %>% str_subset("^vars0\\d|^vars10") %>% map(function(x) eval(as.symbol(x))),
    left_join,
    by = "id"
   ) #%>%
  # left_join(browser_interactions_summary, by = "id") %>%
  # left_join(resize_screen, by = "id") %>%
  # left_join(
  #   browser_interactions %>% 
  #     select(id, starts_with("event")) %>% 
  #     distinct() %>%
  #     group_by(id) %>%
  #     summarise(
  #       event_during_cueing  = ifelse(isTRUE(any(event_during_cueing)), FALSE, TRUE),
  #       event_during_flanker = ifelse(isTRUE(any(event_during_flanker)), FALSE, TRUE),
  #       event_during_change  = ifelse(isTRUE(any(event_during_change)), FALSE, TRUE), 
  #     )
  # )

codebook <- create_codebook(self_report) %>%
  mutate(Label =  ifelse(str_detect(Variable, "^(meta|stai|chaos|unp|quic|change_env|violence|ses|impuls|fos|depression)"), 
                        str_replace_all(Label, pattern = "^(.*|.*\\n.*)\\s-\\s", replacement = ""),
                        'no'))

# Save Data ---------------------------------------------------------------

save(self_report, codebook, file = here("data", "1_pilot", "0_self_report_raw.Rdata"))

save(flanker_data, 
     change_data, 
     cueing_data, 
     browser_interactions, 
     browser_interactions_summary,
     resize_screen, 
     codebook, 
     file = here("data", "1_pilot", "0_task_data_raw.Rdata"))
