library(glue)

flanker_trials <- expand_grid(
  target      = c('left_up', 'left_down', 'right_up', 'right_down'),
  outer_left  = c('left_up', 'left_down', 'right_up', 'right_down'),
  inner_left  = c('left_up', 'left_down', 'right_up', 'right_down'),
  inner_right = c('left_up', 'left_down', 'right_up', 'right_down'),
  outer_right = c('left_up', 'left_down', 'right_up', 'right_down'),
  angle       = c(45, 60, 65, 70, 80),
  location    = c('up', 'down'),
  congruency   = c("congruent", "incongruent")
) %>%
  # Filter 
  filter(
    congruency == "congruent" & str_detect(target, "left") & str_detect(outer_left, "left") & str_detect(inner_left, "left") & str_detect(inner_right, "left") & str_detect(outer_right, "left") |
    congruency == "congruent" & str_detect(target, "right") & str_detect(outer_left, "right") & str_detect(inner_left, "right") & str_detect(inner_right, "right") & str_detect(outer_right, "right") |
    congruency == "incongruent" & str_detect(target, "left") & str_detect(outer_left, "right") & str_detect(inner_left, "right") & str_detect(inner_right, "right") & str_detect(outer_right, "right") |
    congruency == "incongruent" & str_detect(target, "right") & str_detect(outer_left, "left") & str_detect(inner_left, "left") & str_detect(inner_right, "left") & str_detect(outer_right, "left")
  ) %>%
  mutate(trial = 1:n()) %>%
  select(trial, congruency, angle, outer_left, inner_left, target, inner_right, outer_right) %>%
  pivot_longer(c(outer_left, inner_left, target, inner_right, outer_right), names_to = "arrow", values_to = "direction") %>%
  separate(direction, c("horizontal", "vertical"), "_") %>%
  mutate(
    rotation   = case_when(
      horizontal == "left"  & vertical == "down" ~ 360 - angle,
      horizontal == "left"  & vertical == "up"   ~ angle,
      horizontal == "right" & vertical == "down" ~ angle,
      horizontal == "right" & vertical == "up"   ~ 360 - angle,
    ),
    horizontal = ifelse(horizontal == "left", "&larr;", "&rarr;"),
    horizontal = ifelse(arrow != "target", NA, horizontal)
  ) %>%
  select(-vertical) %>%
  group_by(trial) %>%
  fill(horizontal, .direction = "downup") %>%
  ungroup() %>%
  pivot_wider(names_from = "arrow", values_from = "rotation") %>%
  mutate(
    direction_flankers = case_when(
      congruency == "congruent"                            ~ horizontal,
      congruency == "incongruent" & horizontal == "&larr;" ~ "&rarr;",
      congruency == "incongruent" & horizontal == "&rarr;" ~ "&larr;",
    ),
    angles_vtr         = str_c("[", str_c(outer_left, inner_left, target, inner_right, outer_right, sep = ", "), "]"),
    correct_response   = str_c("correct_response: ", "'", ifelse(horizontal == "&larr;", "ArrowLeft", "ArrowRight"), "'"),
    stim               = str_c("stim: set_arrows(angles = ", angles_vtr, ", loc = '", location, "', flankers = '", direction_flankers, "', target = '", horizontal, "')"),
    location           = str_c("location: '", location, "'"),
    congruency         = str_c("congruency: '", congruency, "'"),
    angle              = str_c("angle: '", angle, "'") 
  ) %>%
  select(trial, congruency, angle, location, correct_response, stim)


trial_spec <- 
  glue_data(
    flanker_trials,
    "{{{congruency}, {angle}, {location}, {correct_response}, {stim}}},"
  ) 


writeClipboard(trial_spec)
