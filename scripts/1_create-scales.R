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
    stai_s_mean = across(matches("stai_s")) %>% psych::reverse.code(keys = c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1), items = ., mini = 1, maxi = 4) %>% rowMeans(., na.rm = T), 
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
    
    quic_total_sum           = across(matches("quic")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,1,1,1,1,1,1,-1,1,1,1,1,1,1,-1,1,1,1,1,1), items = ., mini = 0, maxi = 1) %>% rowSums(., na.rm = T),
    quic_total_missing       = across(matches("quic")) %>% is.na() %>% rowSums(., na.rm = T),    
    
    quic_monitoring_sum      = across(matches("quic_monitoring")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1,-1,-1), items = ., mini = 0, maxi = 1) %>% rowSums(., na.rm = T),
    quic_monitoring_missing  = across(matches("quic_monitoring")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_par_predict_sum     = across(matches("quic_par_predict")) %>% psych::reverse.code(keys = c(1,-1,1,1,-1,1,-1,1,1,1,1,1), items = ., mini = 0, maxi = 1) %>% rowSums(., na.rm = T),
    quic_par_predict_missing = across(matches("quic_par_predict")) %>% is.na() %>% rowSums(., na.rm = T),
     
    quic_par_env_sum         = across(matches("quic_par_env")) %>% psych::reverse.code(keys = c(1,1,1,1,-1,1,1), items = ., mini = 0, maxi = 1) %>% rowSums(., na.rm = T),
    quic_par_env_missing     = across(matches("quic_par_env")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_phys_env_sum        = across(matches("quic_phys_env")) %>% psych::reverse.code(keys = c(1,1,1,1-1,1,1), items = ., mini = 0, maxi = 1) %>% rowSums(., na.rm = T),
    quic_phys_env_missing    = across(matches("quic_phys_env")) %>% is.na() %>% rowSums(., na.rm = T),
    
    quic_safety_sum          = across(matches("quic_safety")) %>% rowMeans(., na.rm = T),
    quic_safety_missing      = across(matches("quic_safety")) %>% is.na() %>% rowSums(., na.rm = T)
  )


# SES ---------------------------------------------------------------------
vars04_ses <- 
  aut_data %>% 
  select(id,matches("ses\\d\\d")) %>% 
  mutate(
    ses_mean    = across(matches("ses\\d\\d$")) %>% psych::reverse.code(keys = c(1,1,1,1,1,1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    ses_missing = across(matches("ses\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )

# ACEs --------------------------------------------------------------------
vars04_ctq <- 
  aut_data %>% 
  select(id,matches("ctq\\d\\d")) %>% 
  mutate(
    ctq_ea    = across(matches("ctq(03|08|14|18|25)")) %>% rowMeans(., na.rm = T),
    ctq_pa    = across(matches("ctq(09|11|12|15|17)")) %>% rowMeans(., na.rm = T),
    ctq_sa    = across(matches("ctq(20|21|23|24|27)")) %>% rowMeans(., na.rm = T),
    ctq_en    = across(matches("ctq(05|07|13|19|28|02|26)")) %>% psych::reverse.code(keys = c(-1,-1,-1,-1,-1,-1,-1), items = ., mini = 1, maxi = 5) %>% rowMeans(., na.rm = T),
    ctq_md    = across(matches("ctq(04|10|16|22)")) %>% rowMeans(., na.rm = T),
    ctq_pn    = across(matches("ctq(01|04|06)")) %>% rowMeans(., na.rm = T),
    ctq_total = across(matches("ctq(01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28)$")) %>%
      psych::reverse.code(keys = c(1,-1,1,1,-1,1,-1,1,1,-1,1,1,-1,1,1,-1,1,1,-1,1,1,1,1,1,1,-1,1,-1), items = ., mini = 1, maxi = 5) %>% 
      rowMeans(., na.rm = T)
  )

# Violence ----------------------------------------------------------------
vars05_vio <- 
  aut_data %>% 
  select(id,matches("violence\\d\\d")) %>% 
  mutate(
    violence_mean    = across(matches("violence\\d\\d$")) %>% psych::reverse.code(keys = c(-1,1,-1,1,1,1,1)) %>% rowMeans(., na.rm = T),
    violence_missing = across(matches("violence\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )

# Resilience --------------------------------------------------------------
vars06_res <- 
  aut_data %>% 
  select(id,matches("resilience\\d\\d")) %>% 
  mutate(
    resilience_mean    = across(matches("resilience\\d\\d$")) %>% rowMeans(., na.rm = T),
    resilience_missing = across(matches("resilience\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )

# Family Psychopathology --------------------------------------------------
vars07_fam <- 
  aut_data %>% 
  select(id, matches("fam_psych\\d\\d$")) %>% 
  mutate(
    fam_pscyh_mean    = across(matches("fam_psych\\d\\d$")) %>% rowMeans(., na.rm = T),
    fam_pscyh_missing = across(matches("fam_psych\\d\\d$")) %>% is.na() %>% rowSums(., na.rm = T)
  )

# Demographics ------------------------------------------------------------
vars08_dems <- 
  aut_data %>% 
  select(id,starts_with("dems")) 

# Attention ---------------------------------------------------------------
vars09_att <- 
  aut_data %>% 
  select(id,starts_with("att")) %>% 
  rename(attention_getup = att_getup, attention_interrupted = att_interrupted) %>% 
  mutate(
    attention_sum = (ifelse(attention1 == 1, 0, 1) + ifelse(attention2 == 4, 0, 1))
  )

# Admin -------------------------------------------------------------------
vars10_admin <- 
  aut_data %>% 
  select(ends_with("id"))

# Creativity --------------------------------------------------------------
## Raw AUT ----
vars11_aut_raw <- 
  aut_data %>% 
  select(id, aut_data) %>% 
  filter(str_detect(aut_data, "^\\[\\{\"view_history")) %>% 
  mutate(
    aut = str_replace(aut_data,"\\.\\.\\.$","ETHAN") %>% stringi::stri_replace_last(regex = ",", "ETHAN") %>% str_replace("ETHAN.*ETHAN$",'\\}\\]'),
    aut = str_replace(aut, "\\}\\}\\]$", "\\}\\]"),
    aut = ifelse(!is.na(aut_data), map(aut, function(x) jsonlite::fromJSON(x)), NA)
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

## Deal with Ranks ----
vars13_aut_ranks <- 
  vars11_aut_raw %>%
  filter(str_detect(variable, "rank")) %>% 
  select(id, variable, choice, selected) %>% 
  mutate(rank = case_when(selected == "first" ~ 1, selected == "second" ~ 2, selected == "third" ~ 3)) %>% 
  filter(!is.na(rank)) %>% 
  group_by(id, variable) %>% 
  mutate(rank_count = 1:n()) %>% 
  filter(rank_count >= max(rank_count) - 2) %>% 
  mutate(item = str_remove_all(variable, "^aut_|_rank$")) %>% 
  ungroup() %>% 
  select(id, item, choice, rank)

## Merge ranks ----
vars14_prepped <- 
  left_join(
    vars12_aut,
    vars13_aut_ranks, by = c("id" = "id","item" = "item","response" = "choice")
  )

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
