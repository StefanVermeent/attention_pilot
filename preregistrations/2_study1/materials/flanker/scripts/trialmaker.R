library(glue)



# Standard Condition ------------------------------------------------------

standard_condition <- 
  expand_grid(
    condition  = "standard",
    size       = 40,
    padding    = 0,
    target     = c("left", "right", "left", "right"),
    congruency = c("congruent", "incongruent"),
    location   = c("up", "down")
  ) %>%
  mutate(
    outer_left = case_when(
      target == "left" & congruency == "congruent" ~ "left",
      target == "left" & congruency == "incongruent" ~ "right",
      target == "right" & congruency == "congruent" ~ "right",
      target == "right" & congruency == "incongruent" ~ "left"
    ),
    inner_left = outer_left,
    inner_right = outer_left,
    outer_right = outer_left,
    
    correct_response   = ifelse(target == "left", "Arrowleft", "Arrowright"),
    direction_target   = ifelse(target == 'left', "&larr;", "&rarr;"),
    direction_flankers = ifelse(outer_left == 'left', "&larr;", "&rarr;")) %>%
  select(congruency, condition, size, padding, location, outer_left, inner_left, target, inner_right, outer_right, correct_response, direction_target, direction_flankers)


# Degraded Condition ------------------------------------------------------

degraded_condition <- 
  expand_grid(
    condition  = "degraded",
    size       = 40,
    padding    = 0,
    target     = c("left_up", "left_down", "right_up", "right_down"),
    congruency = c("congruent", "incongruent"),
    location   = c("up", "down")
  ) %>%
  mutate(
    outer_left = case_when(
      target == "left_up"    & congruency == "congruent" ~ "left_up",
      target == "left_down"  & congruency == "congruent" ~ "left_down",
      target == "right_up"   & congruency == "congruent" ~ "right_up",
      target == "right_down" & congruency == "congruent" ~ "right_down",
      
      target == "left_up"    & congruency == "incongruent" ~ "right_down",
      target == "left_down"  & congruency == "incongruent" ~ "right_up",
      target == "right_up"   & congruency == "incongruent" ~ "left_down",
      target == "right_down" & congruency == "incongruent" ~ "left_up",
    ),
    inner_left = outer_left,
    inner_right = outer_left,
    outer_right = outer_left,
    
    correct_response   = ifelse(target %in% c("left_up", "left_down"), "Arrowleft", "Arrowright"),
    direction_target   = ifelse(target %in% c("left_up", "left_down"), "&larr;", "&rarr;"),
    direction_flankers = ifelse(outer_left %in% c("left_up", "left_down"), "&larr;", "&rarr;")
  ) %>%
  select(congruency, condition, size, padding, location, outer_left, inner_left, target, inner_right, outer_right, correct_response, direction_target, direction_flankers)


# Enhanced Condition -------------------------------------------------------

enhanced_condition <- 
  expand_grid(
    condition  = "enhanced",
    size       = 45,
    padding    = 5,
    target     = c("left", "right", "left", "right"),
    congruency = c("congruent", "incongruent"),
    location   = c("up", "down")
  ) %>%
  mutate(
    outer_left = case_when(
      target == "left" & congruency == "congruent" ~ "left",
      target == "left" & congruency == "incongruent" ~ "right",
      target == "right" & congruency == "congruent" ~ "right",
      target == "right" & congruency == "incongruent" ~ "left"
    ),
    inner_left = outer_left,
    inner_right = outer_left,
    outer_right = outer_left,
    correct_response   = ifelse(target == "left", "Arrowleft", "Arrowright"),
    direction_target   = ifelse(target == 'left', "&larr;", "&rarr;"),
    direction_flankers = ifelse(outer_left == 'left', "&larr;", "&rarr;")) %>%
  select(congruency, condition, size, padding, location, outer_left, inner_left, target, inner_right, outer_right, correct_response, direction_target, direction_flankers)



flanker_trials <- bind_rows(standard_condition, degraded_condition, enhanced_condition) %>%
  mutate(across(matches("^target$|(outer|inner)_(left|right)"),
                ~case_when(
                  . == "left" | . == "right" ~ "0",
                  . == "left_up" | . == "right_down" ~ "45",
                  . == "left_down" | . == "right_up" ~ "315",
                  TRUE ~ .
                )
  )) %>%
  mutate(
    angles_vtr         = str_c("[", str_c(outer_left, inner_left, target, inner_right, outer_right, sep = ", "), "]"),
    correct_response   = str_c("correct_response: ", "'", correct_response, "'"),
    stim               = str_c("stim: set_arrows(angles = ", angles_vtr, ", loc = '", location, "', flankers = '", direction_flankers, "', target = '", direction_target, "', size = ", size, ", padding = ", padding, ")"),
    location           = str_c("location: '", location, "'"),
    congruency         = str_c("congruency: '", congruency, "'"),
    condition          = str_c("condition: '", condition, "'") 
  ) %>%
  select(congruency, condition, location, correct_response, stim)


trial_spec <- 
  glue_data(
    flanker_trials,
    "{{{congruency}, {condition}, {location}, {correct_response}, {stim}}},"
  ) 


writeClipboard(trial_spec)
