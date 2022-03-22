# Packages ----------------------------------------------------------------
library(tidyverse)
library(qualtRics)
library(jsonlite)
library(here)
library(sjlabelled)

# Functions ---------------------------------------------------------------
source(here("preregistrations", "2_study1", "scripts", "custom_functions", "create_codebook.R"))
source(here("preregistrations", "2_study1", "scripts", "custom_functions", "functions_exclusions.R"))

# Data --------------------------------------------------------------------
study_data <- 
  fetch_survey(
    surveyID = "SV_dparQvRc5ViFyYe", 
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
  filter(finished==1) %>%
  select(-session_id)


# Self-report -------------------------------------------------------------

## Non-recoded standard deviations in item responses ----

study_data <- response_sd(study_data, "stai")
study_data <- response_sd(study_data, "chaos")
study_data <- response_sd(study_data, "quic")
study_data <- response_sd(study_data, "violence")
study_data <- response_sd(study_data, "ses")
study_data <- response_sd(study_data, "fos")
study_data <- response_sd(study_data, "depression")


## meta data ----

vars01_meta <- 
  study_data %>% 
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
    
    meta_task_duration_z        = scale(meta_task_duration) %>% as.numeric(),
    meta_state_duration_z       = scale(meta_state_duration) %>% as.numeric(),
    meta_ace_duration_z         = scale(meta_ace_duration) %>% as.numeric(),
    meta_temp_orient_duration_z = scale(meta_temp_orient_duration) %>% as.numeric(),
    meta_psychopath_duration_z  = scale(meta_psychopath_duration) %>% as.numeric(),
    meta_dems_duration_z        = scale(meta_dems_duration) %>% as.numeric()
  ) %>% 
  select(id, starts_with("meta_"))


## Current state ----

vars02_state <- 
  study_data %>%
  select(id, starts_with('stai_s'), sd_stai, sick, meal, hungry, sleep, rested) %>%
  # Recode variables
  mutate(across(matches("stai_s(01|02|05|08|10|11|15|16|19|20)"), ~ 5 - .)) %>%
  # Tidy labels
  mutate(across(matches("stai_s"), ~set_label(x = ., label = str_replace_all(get_label(.), "^.*-\\s", "")))) %>%
  mutate(across(matches("stai_s(01|02|05|08|10|11|15|16|19|20)"), ~set_label(x = ., label = str_c(get_label(.), " (recoded)")))) %>%
  # Create composites
  mutate(
    stai_s_mean = across(matches("stai_s"))  %>% rowMeans(., na.rm = T),
    stai_s_missing = across(matches("stai_s")) %>% is.na() %>% rowSums(., na.rm = T)
  ) %>%
  var_labels(
    stai_s_mean    = "Mean score of the State-Trait Anxiety Scale (state subscale). Higher scores mean more state anxiety.",
  )
  
  

## Unpredictability ----

vars03_unp <- 
  study_data %>% 
  select(id,starts_with(c("chaos", "unp", "quic", "change_env")), sd_quic, sd_chaos) %>% 
  # Fix mistake in Qualtrics data - contained a six answer with a default answer label
  mutate(across(matches("unp\\d\\d"), ~case_when(. == 6 ~ 5, TRUE ~ .))) %>%
  # Recode variables
  mutate(across(matches("chaos(01|02|04|07|12|14|15)"), ~ 6 - .)) %>%
  mutate(across(matches("quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32)"), ~ 6 - .)) %>%
  # Tidy labels
  mutate(across(matches("(quic\\d\\d)|(chaos\\d\\d)|change_env(\\d\\d)|(unp\\d\\d)"), ~set_label(x = ., label = str_replace_all(get_label(.), "^.*-\\s", "")))) %>%
  mutate(across(matches("(chaos(01|02|04|12|14|15))|(quic(01|02|03|04|05|06|07|08|09|11|14|16|22|32))"), ~set_label(x = ., label = str_c(get_label(.), " (recoded)")))) %>%
  # Create composites
  mutate(
    pcunp_mean               = across(matches("unp\\d\\d")) %>% rowMeans(., na.rm = T),
    pcunp_missing            = across(matches("unp\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    change_env_mean          = across(matches("change_env\\d\\d")) %>% rowMeans(., na.rm = T),
    change_env_missing       = across(matches("change_env\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    chaos_mean               = across(matches("chaos\\d\\d")) %>% rowMeans(., na.rm = T),
    chaos_missing            = across(matches("chaos\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_monitoring_mean     = across(matches("quic(01|02|03|04|05|06|07|08|09)")) %>%  rowMeans(., na.rm = T),
    quic_monitoring_missing  = across(matches("quic(01|02|03|04|05|06|07|08|09)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_predict_mean    = across(matches("quic(10|11|12|13|14|15|16|17|18|19|20|21)")) %>% rowMeans(., na.rm = T),
    quic_par_predict_missing = across(matches("quic(10|11|12|13|14|15|16|17|18|19|20|21)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_env_mean        = across(matches("quic(22|23|24|25|26|27)")) %>% rowMeans(., na.rm = T),
    quic_par_env_missing     = across(matches("quic(22|23|24|25|26|27)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_phys_env_mean       = across(matches("quic(28|29|30|31|32|33|34)")) %>% rowMeans(., na.rm = T),
    quic_phys_env_missing    = across(matches("quic(28|29|30|31|32|33|34)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_safety_mean         = across(matches("quic(35|36|37)")) %>% rowMeans(., na.rm = T),
    quic_safety_missing      = across(matches("quic(35|36|37)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_total_mean          = across(matches("quic\\d\\d")) %>% rowMeans(., na.rm = T),
    quic_total_missing       = across(matches("quic\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T),   
    
    unp_moving_binned = case_when(
      unp_moving == 0 ~ 0,
      unp_moving %in% c(1,2) ~ 1,
      unp_moving %in% c(3,4) ~ 2,
      unp_moving %in% c(5,6) ~ 3,
      unp_moving %in% c(7,8) ~ 4,
      unp_moving %in% c(9,10) ~ 5,
      unp_moving > 10 ~ 6,
    ),
    
    # If no other adults besides caregivers lived in the household, set nr of male/female romantic partners to 0.
    unp_female_fig_num       = ifelse(unp_other_adults == 2, 0, unp_female_fig_num),
    unp_male_fig_num         = ifelse(unp_other_adults == 2, 0, unp_male_fig_num),
    
    unp_female_fig_rom       = ifelse(unp_female_fig_num == 0, 0, unp_female_fig_rom),
    unp_male_fig_rom         = ifelse(unp_male_fig_num == 0, 0, unp_male_fig_rom),
                                      
    unp_male_fig_rom_binned = case_when(
      unp_male_fig_rom == 0 ~ 0,
      unp_male_fig_rom == 1 ~ 1,
      unp_male_fig_rom == 2 ~ 2,
      unp_male_fig_rom == 3 ~ 3,
      unp_male_fig_rom == 4 ~ 4,
      unp_male_fig_rom == 5 ~ 5,
      unp_male_fig_rom >= 6 ~ 6,
    ),
    unp_female_fig_rom_binned = case_when(
      unp_female_fig_rom == 0 ~ 0,
      unp_female_fig_rom == 1 ~ 1,
      unp_female_fig_rom == 2 ~ 2,
      unp_female_fig_rom == 3 ~ 3,
      unp_female_fig_rom == 4 ~ 4,
      unp_female_fig_rom == 5 ~ 5,
      unp_female_fig_rom >= 6 ~ 6,
    ),
                                      
    unp_subj_comp             = across(c(pcunp_mean, chaos_mean, quic_total_mean)) %>% rowMeans(., na.rm = T) %>% scale %>% as.numeric(),
    unp_obj_comp              = across(c(unp_moving_binned, unp_male_fig_rom_binned, unp_female_fig_rom_binned, change_env_mean)) %>% scale %>% rowMeans(., na.rm = T),
    unp_comp                  = across(c(unp_subj_comp, unp_obj_comp)) %>% rowMeans(., na.rm = T)
    
    ) %>%
  var_labels(
    pcunp_mean                = "Mean score of the Perceived Unpredictability Scale. Higher scores mean more perceived unpredictability prior to age 13.",
    chaos_mean                = "Mean score of the Confusion, Hubbub, and Order Scale (CHAOS; adapted). Higher scores mean more household chaos prior to age 13.",
    quic_monitoring_mean      = "Mean scores of the 'Parental Monitoring and Involvement' subscale of the Questionnaire of Unpredictability in Childhood (QUIC; adapted). 
                                 Higher scores mean more unpredictability prior to age 13.",
    quic_par_predict_mean     = "Mean scores of the 'Parental Predictability' subscale of the Questionnaire of Unpredictability in Childhood (QUIC; adapted). 
                                 Higher scores mean more unpredictability prior to age 13.",
    quic_par_env_mean         = "Mean scores of the 'Parental Environment' subscale of the Questionnaire of Unpredictability in Childhood (QUIC; adapted). 
                                 Higher scores mean more unpredictability prior to age 13.",
    quic_phys_env_mean        = "Mean scores of the 'Physical Environment' subscale of the Questionnaire of Unpredictability in Childhood (QUIC; adapted). 
                                 Higher scores mean more unpredictability prior to age 13.",
    quic_safety_mean          = "Mean scores of the 'Safety and Security' subscale of the Questionnaire of Unpredictability in Childhood (QUIC; adapted). 
                                 Higher scores mean more unpredictability prior to age 13.",
    quic_total_mean           = "Mean score of all items of the Questionnaire of Unpredictability in Childhood (QUIC; adapted). 
                                 Higher scores mean more unpredictability prior to age 13.",
    unp_moving_binned         = "Binned score of the number of residential changes of participants prior to age 13.",
    unp_male_fig_rom_binned   = "Binned score of the number of male romantic partners within the household prior to age 13.",
    unp_female_fig_rom_binned = "Binned score of the number of female romantic partners within the household prior to age 13",
    
    unp_subj_comp             = "Subjective unpredictability: Composite measure consisting of the unweighted average of Perceived unpredictability scale (pcunp_mean), 
                                 CHAOS scale (chaos-mean) and QUIC (quic_total_mean). After averaging, the composite was standardized. Higher scores mean more subjective unpredictability",
    unp_obj_comp              = "Objective unpredictability: Composite measure consisting of the number of residential changes (binned; unp_moving_binned), 
                                 number of male romantic figures in the household (binned; unp_male_fig_rom_binned), and number of female romantic partners in the household
                                 (binned; unp_female_fig_rom_binned). All variables were standardized before calculating the unweighted average. Higher scores mean more
                                 objective unpredictability",
    unp_comp                  = "Unpredictability: Composite measure consisting of the unweighted average of objective unpredictability (unp_obj_comp) and subjective unpredictability
                                 (unp_subj_comp)"
  ) %>%
  val_labels(
    unp_moving_binned         = c("0 times" = 0,
                                  "1-2 times" = 1,
                                  "3-4 times" = 2,
                                  "5-6 times" = 3,
                                  "7-8 times" = 4,
                                  "9-10 times" = 5,
                                  "> 10 times" = 6),
    unp_male_fig_rom_binned   = c("0" = 0,
                                  "1" = 1, 
                                  "2" = 2,
                                  "3" = 3,
                                  "4" = 4,
                                  "5" = 5,
                                  ">6" = 6),
    unp_female_fig_rom_binned = c("0" = 0,
                                  "1" = 1, 
                                  "2" = 2,
                                  "3" = 3,
                                  "4" = 4,
                                  "5" = 5,
                                  ">6" = 6)
  )




## Violence ----

vars04_vio <- 
  study_data %>% 
  select(id,matches("violence\\d\\d"), aces_fighting1, aces_fighting2, sd_violence) %>% 
  # Recode variables
  mutate(across(matches("violence(01|03)"), ~ 6 - .)) %>%
  # Tidy labels
  mutate(across(matches("violence\\d\\d|aces_fighting\\d"), ~set_label(x = ., label = str_replace_all(get_label(.), "^.*-\\s", "")))) %>%
  mutate(across(matches("violence(01|03)"), ~set_label(x = ., label = str_c(get_label(.), " (recoded)")))) %>%
  # Create composites
  mutate(
    nvs_mean           = across(matches("violence\\d\\d$")) %>% rowMeans(., na.rm = T),
    nvs_missing        = across(matches("violence\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T),
    fighting_mean      = across(matches("aces_fighting\\d")) %>% rowMeans(., na.rm = T),
    vio_comp           = across(c(nvs_mean, fighting_mean)) %>% scale %>% rowMeans(., na.rm = TRUE)
  ) %>%
  var_labels(
    nvs_mean           = "Mean score of the Neighborhood Violence Scale (NVS). Higher scores mean more neighborhood violence prior to age 13.",
    fighting_mean      = "Mean score of the two fighting exposure items. Higher scores mean more fighting exposure prior to age 13.",
    vio_comp           = "Violence Exposure: Composite measure consisting of the unweighted average of the NVS (nvs_mean) and fighting average (fighting_mean).
                          Both measures were standardized before averaging. Higher scores mean more violence exposure prior to age 13."
  ) 


## SES ----

vars05_ses <- 
  study_data %>% 
  select(id,matches("ses\\d\\d"), dems_edu_first, dems_edu_second, dems_income_past, dems_class_past, sd_ses) %>% 
  # Recode Variables
  mutate(across(matches("ses07"), ~ 6 - .)) %>%
  # Tidy labels
  mutate(across(matches("(ses\\d\\d)"), ~set_label(x = ., label = str_replace_all(get_label(.), "^.*-\\s", "")))) %>%
  mutate(across(matches("ses07"), ~set_label(x = ., label = str_c(get_label(.), " (recoded)")))) %>% 
  mutate(
    dems_income_past        = ifelse(dems_income_past == 7, NA, dems_income_past),
    edu_caregivers          = across(c(dems_edu_first, dems_edu_second)) %>% rowMeans(., na.rm = T),
    ) %>%
  # Create Composites
  mutate(
    ses_subj_comp           = across(matches("ses\\d\\d$")) %>% rowMeans(., na.rm = T),
    ses_subj_missing        = across(matches("ses\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T),
    
    ses_obj_comp            = across(c(edu_caregivers, dems_income_past, dems_class_past)) %>% scale %>% rowMeans(., na.rm = T),
    ses_obj_missing         = across(c(edu_caregivers, dems_income_past, dems_class_past)) %>% is.na %>% rowSums(., na.rm = TRUE),
    
    ses_comp                = across(c(ses_subj_comp, ses_obj_comp)) %>% scale %>% rowMeans(., na.rm = TRUE)
  ) %>%
  var_labels(
    ses_subj_comp             = "Subjective SES: Consisting of the average of the perceived SES items. Higher scores mean higher perceived SES, 
                                 CHAOS scale (chaos-mean) and QUIC (quic_total_mean). After averaging, the composite was standardized. Higher scores mean more subjective unpredictability",
    ses_obj_comp              = "Objective SES: Composite measure consisting of the average of the caregivers' education level (edu_caregivers), family income during childhood (dems_income_past),
                                 and social class during childhood (dems_class_past). All variables were standardized before calculating the unweighted average. Higher scores mean higher objective SES.",
    ses_comp                  = "SES composite: Composite measure consisting of the unweighted average of objective SES (ses_obj_comp) and subjective SES (ses_subj_comp)"
  )

## Temporal Orientation ----


vars06_temp_orientation <- 
  study_data %>% 
  select(id,starts_with(c('impuls', 'fos')), sd_fos) %>% 
  # Recode variables
  mutate(across(matches("fos(01|03|04|06|08|11|13|15)"), ~ 6 - .)) %>%
  # Tidy labels
  mutate(across(matches("(fos\\d\\d)|impuls\\d\\d"), ~set_label(x = ., label = str_replace_all(get_label(.), "^.*-\\s", "")))) %>%
  mutate(across(matches("fos(01|03|04|06|08|11|13|15)"), ~set_label(x = ., label = str_c(get_label(.), " (recoded)")))) %>%
  # Create composites
  mutate(
    impuls_mean         = across(matches("impuls")) %>% rowMeans(., na.rm = T),
    impuls_missing      = across(matches("impuls")) %>% is.na() %>% rowSums(., na.rm = T),
    
    fos_pa_mean         = across(matches("fos(01|06|07|12|13)")) %>% rowMeans(., na.rm = T),
    fos_pa_missing      = across(matches("fos(01|06|07|12|13)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    fos_tp_mean         = across(matches("fos(02|05|08|11|14)")) %>% rowMeans(., na.rm = T),
    fos_tp_missing      = across(matches("fos(02|05|08|11|14)")) %>% is.na() %>% rowSums(., na.rm = T),

    fos_fc_mean         = across(matches("fos(03|04|09|10|15)")) %>% rowMeans(., na.rm = T),
    fos_fc_missing      = across(matches("fos(03|04|09|10|15)")) %>% is.na() %>% rowSums(., na.rm = T),
    
    fos_fo_mean         = across(matches("fos\\d\\d")) %>% rowMeans(., na.rm = T),
    fos_fo_missing      = across(matches("fos\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T)
    ) %>%
  var_labels(
    impuls_mean = "Mean score of the 'Motor Impulsivity' subscale of the Barrett Impulsivity Scale (BIS). Higher scores mean more impulsivity.",
    fos_pa_mean = "Mean score of the 'Planning Ahead' subscale of the Future Orientation Scale (FOS). Higher scores mean more planning ahead.",
    fos_tp_mean = "Mean score of the 'Time Perspective' subscale of the Future Orientation Scale (FOS). Higher scores mean longer time perspective.",
    fos_fc_mean = "Mean score of the 'Anticipation of Future Consequences' subscale of the Future Orientation Scale (FOS). Higher scores mean more anticipation.",
    fos_fo_mean = "Mean score of the total Future Orientation Scale (FOS). Higher scores mean more future orientation."
  )

## Depressive Symptoms ----

vars07_dep <- 
  study_data %>% 
  select(id, matches("depression\\d\\d$"), sd_depression) %>% 
    # Recode variables
    mutate(across(matches("depression(04|08|12|16)"), ~ 5 - .)) %>%
    # Tidy labels
    mutate(across(matches("(depression\\d\\d)"), ~set_label(x = ., label = str_replace_all(get_label(.), "^.*-\\s", "")))) %>%
    mutate(across(matches("depression(04|08|12|16)"), ~set_label(x = ., label = str_c(get_label(.), " (recoded)")))) %>%
  # Create composites
  mutate(
    depression_mean = across(matches("depression\\d\\d")) %>% rowMeans(., na.rm = T),
    impuls_missing  = across(matches("depression\\d\\d")) %>% is.na() %>% rowSums(., na.rm = T)
  ) %>%
  var_labels(
    depression_mean = "Mean score of the Center for Epidemiologic Studies Depression Scale (CESD). Higher scores mean more depressive symptoms."
  )

## Family Composition ----

vars08_fam_comp <- 
  study_data %>%
  select(id, starts_with("fam_composition")) %>%
  mutate(across(contains("fam_composition"), as.character)) %>%
  pivot_longer(-id, names_to = "option", values_to = "value") %>%
  drop_na(value) %>%
  mutate(
    fam_composition = case_when(
      option == "fam_composition_1" ~ "I lived with my mother, but not father, most of the time.",
      option == "fam_composition_2" ~ "I lived with my father, but not mother, most of the time.",
      option == "fam_composition_3" ~ "I lived with my mother and father an equal amount of time (joint custody)",
      option == "fam_composition_4" ~ "I lived with my extended family on mother's side (grandparents, aunts ,uncles, cousins)",
      option == "fam_composition_5" ~ "I lived with my extended family on father's side (grandparents, aunts ,uncles, cousins)",
      option == "fam_composition_6" ~ "I was in foster care most of the time.",
      option == "fam_composition_9_text" ~ str_c("Other, namely:", value),
      option == "fam_composition_10" ~ "Prefer not to say.",
    )
  ) %>%
  group_by(id) %>%
  summarise(family_composition = str_c(fam_composition, collapse = " & ")) %>%
  mutate(fam_composition_multiple = ifelse(str_detect(family_composition, "\\s&\\s"), 1, 0)) 

## Demographics ----

vars09_dems <- 
  study_data %>% 
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
  left_join(study_data %>% select(id, matches("dems_(age|sex|gender|born|english|class_current|edu$|occupation|income$)"), weight, height, activity))
  

## Attention ----

vars10_att <- 
  study_data %>% 
  select(id,starts_with("att")) %>% 
  mutate(
    attention_check_sum     = (ifelse(att_check01 == 5, 0, 1) + ifelse(att_check02 == 2, 0, 1)),
    attention_interrupt_sum = att_getup + att_interrupted
  )

## Admin ----

vars11_admin <- 
  study_data %>% 
  select(ends_with("id"))



# Attention Tasks ---------------------------------------------------------

## Flanker data ----

NA_placeholder <- tribble(
  ~rt,              ~stimulus,        ~response,         ~variable,
  ~task,            ~location,        ~condition,        ~congruency,       ~trial_type,       ~trial_index,    ~time_elapsed,
  ~internal_node_id,~counterbalance,  ~correct,          ~correct_response,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
)

flanker_data <- 
  study_data %>% 
  select(id, data_flanker_std, data_flanker_enh, data_flanker_deg) %>% 
  mutate(across(c(starts_with("data_flanker")), ~map_if(., .p =  ~!is.na(.x), .f = jsonlite::fromJSON))) %>%
  mutate(across(c(starts_with("data_flanker")), ~ifelse(is.na(.), list(NA_placeholder), .))) %>%
  mutate(data_flanker = pmap(list(data_flanker_std, data_flanker_enh, data_flanker_deg), function(data_flanker_std, data_flanker_enh, data_flanker_deg) {
    bind_rows(data_flanker_std, data_flanker_enh, data_flanker_deg)})) %>%
  unnest(data_flanker) %>% 
  select(-data_flanker_std, -data_flanker_enh, -data_flanker_deg) %>%
  select(-c(internal_node_id, stimulus)) %>%
  filter(task == "flanker") %>%
  mutate(
    correct = ifelse(correct, 1, 0)) 

## Browser interactions ----

browser_interactions <- 
  left_join(
    study_data %>%
      select(id, tasks_browser) %>%
      drop_na(tasks_browser) %>%
      mutate(tasks_browser = map(tasks_browser, jsonlite::fromJSON)) %>%
      unnest(tasks_browser),
    flanker_data %>%
      select(id, condition, task, time_elapsed) %>% 
      group_by(id, task, condition) %>% 
      summarise(
        "time_start_flanker" := min(time_elapsed),
        "time_end_flanker"   := max(time_elapsed)
          )
      ) %>% 
  group_by(id) %>%
  fill(starts_with("time_"), .direction = "up") %>%
  ungroup() %>%
  select(-trial) %>%
  drop_na(event) %>%
  # Did a blur event occur during a task?
  mutate(
    event_during_flanker = ifelse((event == "blur") & (time > time_start_flanker & time < time_end_flanker), TRUE, FALSE),
  )


browser_interactions_summary <- browser_interactions %>%
  group_by(id) %>%
  summarise(
    fullscreenenter      = sum(event == "fullscreenenter", na.rm=T),
    fullscreenexit       = sum(event == "fullscreenexit", na.rm=T),
    blur_event           = sum(event == "blur", na.rm=T),
    focus_event          = sum(event == "focus", na.rm = T),
    event_during_flanker = ifelse(any(event_during_flanker == TRUE), TRUE, FALSE)
  ) %>%
  var_labels(
    fullscreenenter      = "Logs whether the participant entered fullscreen mode at the start of the experiment. value 1 if participant entered fullscreen mode;
                            Value 0 if participant did not enter fullscreen mode.",
    fullscreenexit       = "Logs whether the participant exited fullscreen mode at any point during the Flanker Task. value 1 if participant exited fullscreen mode;
                            Value 0 if participant did not exit fullscreen mode.",
    blur_event           = "Logs whether the participant clicked in another browser tab during the Flanker Task (i.e., a blur event), indicating they left the
                            main experiment window. Value 1 if participant had 1 or more blur events; Value 0 if participant had no blur events.",
    event_during_flanker = "Logs whether the participant had at least 1 blur event while the Flanker trials were ungoing. Value 1 if participant had at least 1
                            blur event during Flanker trials; Value 0 if participant had no blur events during main Flanker trials (excluding instructions, break,
                            and practice."
  )

## Screen resizing ----

resize_screen <- 
  study_data %>%
  select(id, data_resize) %>%
  drop_na(data_resize) %>%
  mutate(data_resize = map(data_resize, jsonlite::fromJSON)) %>%
  unnest(data_resize) %>%
  select(id, final_width_px, scale_factor) %>%
  mutate(no_resize = ifelse(round(scale_factor, 3) == 0.901, 1, 0)) %>%
  var_labels(
    final_width_px = "Final width of the resizing box.",
    scale_factor   = "Factor by which the experiment screen was resized to fit the participant's monitor size",
    no_resize      = "If 0, the resize box still had its initial size, indicating the participant did not resize the box."
  )


# Cleaned Data ------------------------------------------------------------

## Self-report scales ----
self_report <- 
  reduce(
    ls() %>% str_subset("^vars0\\d|^vars10") %>% map(function(x) eval(as.symbol(x))),
    left_join,
    by = "id"
  ) 

codebook <- create_codebook(self_report) %>%
  mutate(Label =  ifelse(str_detect(Variable, "^(meta|stai|chaos|unp|quic|change_env|violence|ses|impuls|fos|depression)"), 
                         str_replace_all(Label, pattern = "^(.*|.*\\n.*)\\s-\\s", replacement = ""),
                         'no'))

# Save Data ---------------------------------------------------------------

save(self_report, codebook, file = here("data", "2_study1", "0_self_report_raw.Rdata"))

save(flanker_data, 
     browser_interactions, 
     browser_interactions_summary,
     resize_screen, 
     codebook, 
     file = here("data", "2_study1", "0_task_data_raw.Rdata"))

