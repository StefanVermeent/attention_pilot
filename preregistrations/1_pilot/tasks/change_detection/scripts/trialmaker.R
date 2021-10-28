
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)


# Create trials -----------------------------------------------------------

n_trials = 50
set.seed(42)

color_set = c("#D40E0E", "#0457A0", "#FBF250", "#25D9E4", "#4D0063")

trials = tibble(
  type = rep(str_c("type: '", c('same', 'different'), "'"), (n_trials/2)),
  color = rep(c(color_set), (n_trials/5)),
  circle_to_change = rep(c('circle_1', 'circle_2', 'circle_3', 'circle_4', 'circle_5'), (n_trials/5)),
  correct_response = str_c("correct_response: '", ifelse(type == "type: 'same'", "ArrowLeft", "ArrowRight"), "'")) %>%
  rowwise() %>%
  # Assign colors to circles. the `color` variable determines the color of the changing circle (on change trials). 
  # After the color of the changing circle is set correctly (with the first part of the ifelse statement), the colors of the other circles is determined randomly
  # (using the colors that remain, randomization via sample())
  mutate(
    color1 = ifelse(circle_to_change == "circle_1", color, sample(color_set[str_detect(color_set, color, negate = TRUE)], 1)),
    color2 = ifelse(circle_to_change == "circle_2", color, sample(color_set[str_detect(color_set, str_c(color, color1, sep = "|"), negate = TRUE)], 1)),
    color3 = ifelse(circle_to_change == "circle_3", color, sample(color_set[str_detect(color_set, str_c(color, color1, color2, sep = "|"), negate = TRUE)], 1)),
    color4 = ifelse(circle_to_change == "circle_4", color, sample(color_set[str_detect(color_set, str_c(color, color1, color2, color3, sep = "|"), negate = TRUE)], 1)),
    color5 = ifelse(circle_to_change == "circle_5", color, sample(color_set[str_detect(color_set, str_c(color, color1, color2, color3, color4, sep = "|"), negate = TRUE)], 1)),
    
    color1 = str_c("stim1_color: '", color1, "'"),
    color2 = str_c("stim2_color: '", color2, "'"),
    color3 = str_c("stim3_color: '", color3, "'"),
    color4 = str_c("stim4_color: '", color4, "'"),
    color5 = str_c("stim5_color: '", color5, "'"),
  ) %>%
  ungroup() %>%
  mutate(  
    # X-coordinates of memory items
    mem_x1 = str_c("mem_stim1_x: ",
                   rep('mem_x', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 0, "]")
    ),
    mem_x2 = str_c("mem_stim2_x: ",
                   rep('mem_x', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 1, "]")
                   
    ),
    mem_x3 = str_c("mem_stim3_x: ",
                   rep('mem_x', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 2, "]")
                   
    ),
    mem_x4 = str_c("mem_stim4_x: ",
                   rep('mem_x', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 3, "]")
                   
    ),
    mem_x5 = str_c("mem_stim5_x: ",
                   rep('mem_x', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 4, "]")
                   
    ),
    # Y-coordinates of memory items
    mem_y1 = str_c("mem_stim1_y: ",
                   rep('mem_y', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 0, "]")
                   
    ),
    mem_y2 = str_c("mem_stim2_y: ",
                   rep('mem_y', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 1, "]")
                   
    ),
    mem_y3 = str_c("mem_stim3_y: ",
                   rep('mem_y', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 2, "]")
                   
    ),
    mem_y4 = str_c("mem_stim4_y: ",
                   rep('mem_y', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 3, "]")
                   
    ),
    mem_y5 = str_c("mem_stim5_y: ",
                   rep('mem_y', n_trials),
                   str_c("[", seq(0, n_trials-1, 1), "]"),
                   str_c("[", 4, "]")
                   
    ),
    
    # X-coordinates of test items
    # If current trial is a 'different'  trial, use vector containing new coordinates. Else, keep coordinates of memory items
    test_x1 = ifelse(type == "type: 'different'" & circle_to_change == "circle_1",
                     str_c("test_stim1_x: ",
                           rep('test_x', n_trials), 
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 0, "]")),
                     str_replace_all(mem_x1, "mem_stim1_x: ", "test_stim1_x: ")
    ),
    test_x2 = ifelse(type == "type: 'different'" & circle_to_change == "circle_2",
                     str_c("test_stim2_x: ",
                           rep('test_x', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 1, "]")),
                     str_replace_all(mem_x2, "mem_stim2_x: ", "test_stim2_x: ")
    ),
    test_x3 = ifelse(type == "type: 'different'" & circle_to_change == "circle_3",
                     str_c("test_stim3_x: ",
                           rep('test_x', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 2, "]")),
                     str_replace_all(mem_x3, "mem_stim3_x: ", "test_stim3_x: ")
    ),  
    test_x4 = ifelse(type == "type: 'different'" & circle_to_change == "circle_4",
                     str_c("test_stim4_x: ",
                           rep('test_x', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 3, "]")),
                     str_replace_all(mem_x4, "mem_stim4_x: ", "test_stim4_x: ")
    ),
    test_x5 = ifelse(type == "type: 'different'" & circle_to_change == "circle_5",
                     str_c("test_stim5_x: ",
                           rep('test_x', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 4, "]")),
                     str_replace_all(mem_x5, "mem_stim5_x: ", "test_stim5_x: ")
    ),
    # Y-Coordinates of test items
    test_y1 = ifelse(type == "type: 'different'" & circle_to_change == "circle_1",
                     str_c("test_stim1_y: ",
                           rep('test_y', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 0, "]")),
                     str_replace_all(mem_y1, "mem_stim1_y: ", "test_stim1_y: ")
    ),
    test_y2 = ifelse(type == "type: 'different'" & circle_to_change == "circle_2",
                     str_c("test_stim2_y: ",
                           rep('test_y', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 1, "]")),
                     str_replace_all(mem_y2, "mem_stim2_y: ", "test_stim2_y: ")
    ),
    test_y3 = ifelse(type == "type: 'different'" & circle_to_change == "circle_3",
                     str_c("test_stim3_y: ",
                           rep('test_y', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 2, "]")),
                     str_replace_all(mem_y3, "mem_stim3_y: ", "test_stim3_y: ")
    ),
    test_y4 = ifelse(type == "type: 'different'" & circle_to_change == "circle_4",
                     str_c("test_stim4_y: ",
                           rep('test_y', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 3, "]")),
                     str_replace_all(mem_y4, "mem_stim4_y: ", "test_stim4_y: ")
    ),
    test_y5 = ifelse(type == "type: 'different'" & circle_to_change == "circle_5",
                     str_c("test_stim5_y: ",
                           rep('test_y', n_trials),
                           str_c("[", seq(0, n_trials-1, 1), "]"),
                           str_c("[", 4, "]")),
                     str_replace_all(mem_y5, "mem_stim5_y: ", "test_stim5_y: ")
    )
  )


# Create Javascript trial blocks ------------------------------------------

trials_block1 <- glue_data(
  trials[1:(n_trials/2),],
  "{{{type}, {correct_response}, {mem_x1}, {mem_x2}, {mem_x3}, {mem_x4}, {mem_x5},\n{mem_y1}, {mem_y2}, {mem_y3}, {mem_y4}, {mem_y5},\n{test_x1}, {test_x2}, {test_x3}, {test_x4}, {test_x5},\n{test_y1}, {test_y2}, {test_y3}, {test_y4}, {test_y5},\n{color1}, {color2}, {color3}, {color4}, {color5}}},\n\n"
)

trials_block2 <- glue_data(
  trials[(n_trials/2 + 1):n_trials,],
  "{{{type}, {correct_response}, {mem_x1}, {mem_x2}, {mem_x3}, {mem_x4}, {mem_x5},\n{mem_y1}, {mem_y2}, {mem_y3}, {mem_y4}, {mem_y5},\n{test_x1}, {test_x2}, {test_x3}, {test_x4}, {test_x5},\n{test_y1}, {test_y2}, {test_y3}, {test_y4}, {test_y5},\n{color1}, {color2}, {color3}, {color4}, {color5}}},\n\n"
)
