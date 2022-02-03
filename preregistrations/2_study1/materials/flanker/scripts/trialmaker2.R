library(glue)



# Standard Condition ------------------------------------------------------

standard_condition <- 
  expand_grid(
    angle  = "standard",
    target     = c("left", "right"),
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
  outer_right = outer_left) %>%
  select(congruency, angle, location, outer_left, inner_left, target, inner_right, outer_right)


# Opposite Condition ------------------------------------------------------

opposite_condition <- 
  expand_grid(
    angle      = "opposite",
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
    outer_right = outer_left
  ) %>%
  select(congruency, angle, location, outer_left, inner_left, target, inner_right, outer_right)



# Chaotic Condition -------------------------------------------------------

chaotic_condition <- 
  expand_grid(
    angle      = "chaotic",
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
   inner_left = case_when(
     target == "left_up"    & congruency == "congruent" ~ "left_down",
     target == "left_down"  & congruency == "congruent" ~ "left_up",
     target == "right_up"   & congruency == "congruent" ~ "right_down",
     target == "right_down" & congruency == "congruent" ~ "right_up",
     
     target == "left_up"    & congruency == "incongruent" ~ "right_up",
     target == "left_down"  & congruency == "incongruent" ~ "right_down",
     target == "right_up"   & congruency == "incongruent" ~ "left_up",
     target == "right_down" & congruency == "incongruent" ~ "left_down",
   ),
   outer_right = outer_left,
   inner_right = inner_left
  ) %>%
  select(congruency, angle, location, outer_left, inner_left, target, inner_right, outer_right)


 flanker_trials <- bind_rows(standard_condition, opposite_condition, chaotic_condition) %>%
   mutate(
     correct_response   = target,
     direction_target   = ifelse(target %in% c('left', "left_up", "left_down"), "&larr;", "&rarr;"),
     direction_flankers = ifelse(outer_left %in% c('left', "left_up", "left_down"), "&larr;", "&rarr;")
   ) %>%
   mutate(across(contains(c("left", "right", "target")),
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
    stim               = str_c("stim: set_arrows(angles = ", angles_vtr, ", loc = '", location, "', flankers = '", direction_flankers, "', target = '", direction_target, "')"),
    location           = str_c("location: '", location, "'"),
    congruency         = str_c("congruency: '", congruency, "'"),
    angle              = str_c("angle: '", angle, "'") 
  ) %>%
  select(congruency, angle, location, correct_response, stim)


trial_spec <- 
  glue_data(
    flanker_trials,
    "{{{congruency}, {angle}, {location}, {correct_response}, {stim}}},"
  ) 


writeClipboard(trial_spec)
