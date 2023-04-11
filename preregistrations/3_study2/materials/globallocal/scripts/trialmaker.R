library(tidyverse)
library(glue)
set.seed(486)

n_block_trials <-  32

stim_vector <- c('Ge_Lf', 'Ge_Lp', 'Ge_Lt', 'Gf_Le', 'Gf_Lh', 
'Gh_Lf', 'Gh_Lp', 'Gh_Lt', 'Gp_Le', 'Gp_Lh', 'Gt_Le', 
'Gt_Lh')

# stimulus set 1
trials01 <- tibble(
  type     = c('first', sample(c(rep('repeat',n_block_trials/2), rep('switch',n_block_trials/2)), size = n_block_trials, replace = F)),
  rule     = c('global', rep(NA, n_block_trials)),
  variable = "globloc_01",
  task     = "globloc",
  key_answer = NA
) 

# Generate repeat or switch trials
n = 1
while(n<=n_block_trials){
  trials01 <- trials01 |> 
    mutate(
      rule = case_when(
        is.na(rule) & type == "repeat" & lag(rule, n=1) == 'global' ~ 'global',
        is.na(rule) & type == "repeat" & lag(rule, n=1) == 'local' ~ 'local',
        is.na(rule) & type == "switch" & lag(rule, n=1) == 'global' ~ 'local',
        is.na(rule) & type == "switch" & lag(rule, n=1) == 'local' ~ 'global',
        TRUE ~ rule
      )
    )
  n = n+1
}


trials01 <- trials01 |> 
  mutate(
    stimulus = case_when(
      rule == 'global' ~ sample(str_subset(stim_vector, "(Ge|Gh)"), size = 33, replace = T), 
      rule == 'local' ~ sample(str_subset(stim_vector, "(Le|Lh)"), size = 33, replace = T)),
    key_answer = case_when(
      rule == 'global' ~ 's',
      rule == 'local'  ~ 'l'
      
    )
  )

  


# stimulus set 2
trials02 <- tibble(
  type     = c('first', sample(c(rep('repeat',n_block_trials/2), rep('switch',n_block_trials/2)), size = n_block_trials, replace = F)),
  rule     = c('global', rep(NA, n_block_trials)),
  variable = "globloc_01",
  task     = "globloc",
  key_answer = NA
) 

# Generate repeat or switch trials
n = 1
while(n<=n_block_trials){
  trials02 <- trials02 |> 
    mutate(
      rule = case_when(
        is.na(rule) & type == "repeat" & lag(rule, n=1) == 'global' ~ 'global',
        is.na(rule) & type == "repeat" & lag(rule, n=1) == 'local' ~ 'local',
        is.na(rule) & type == "switch" & lag(rule, n=1) == 'global' ~ 'local',
        is.na(rule) & type == "switch" & lag(rule, n=1) == 'local' ~ 'global',
        TRUE ~ rule
      )
    )
  n = n+1
}


trials02 <- trials02 |> 
  mutate(
    stimulus = case_when(
      rule == 'global' ~ sample(str_subset(stim_vector, "(Ge|Gh)"), size = 33, replace = T), 
      rule == 'local' ~ sample(str_subset(stim_vector, "(Le|Lh)"), size = 33, replace = T)),
    key_answer = case_when(
      rule == 'global' ~ 's',
      rule == 'local'  ~ 'l'
      
    )
  )


# Trial stimulus set
trialsprac <- tibble(
  type     = c('first', sample(c(rep('repeat',5), rep('switch',5)), size = 10, replace = F)),
  rule     = c('global', rep(NA,10)),
  variable = "globloc_prac",
  task     = "globloc",
  key_answer = NA
) 

# Generate repeat or switch trials
n = 1
while(n<=10){
  trialsprac <- trialsprac |> 
    mutate(
      rule = case_when(
        is.na(rule) & type == "repeat" & lag(rule, n=1) == 'global' ~ 'global',
        is.na(rule) & type == "repeat" & lag(rule, n=1) == 'local' ~ 'local',
        is.na(rule) & type == "switch" & lag(rule, n=1) == 'global' ~ 'local',
        is.na(rule) & type == "switch" & lag(rule, n=1) == 'local' ~ 'global',
        TRUE ~ rule
      )
    )
  n = n+1
}


trialsprac <- trialsprac |> 
  mutate(
    stimulus = case_when(
      rule == 'global' ~ sample(str_subset(stim_vector, "(Ge|Gh)"), size = 11, replace = T), 
      rule == 'local' ~ sample(str_subset(stim_vector, "(Le|Lh)"), size = 11, replace = T)),
    key_answer = case_when(
      rule == 'global' ~ 's',
      rule == 'local'  ~ 'l'
      
    )
  )



glue_data(
  trials01,
  "{{stimulus: {stimulus}, key_answer: '{key_answer}', data: {{rule: '{rule}', type: '{type}', variable: '{variable}', task: '{task}'}}}},"
)

glue_data(
  trials02,
  "{{stimulus: {stimulus}, key_answer: '{key_answer}', data: {{rule: '{rule}', type: '{type}', variable: '{variable}', task: '{task}'}}}},"
)

glue_data(
  trialsprac,
  "{{stimulus: {stimulus}, key_answer: '{key_answer}', data: {{rule: '{rule}', type: '{type}', variable: '{variable}', task: '{task}'}}}},"
)
