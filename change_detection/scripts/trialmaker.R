
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)


# Create trials -----------------------------------------------------------

n_trials = 50


trials <- tibble(
  type = str_c("type: '", c(rep('same', n_trials/2), rep('different', n_trials/2)), "'"),
  correct_response = str_c("correct_response: '", c(rep("ArrowLeft", n_trials/2), rep("ArrowRight", n_trials/2)), "'"),
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
  test_x1 = ifelse(type == "type: 'different'",
                   str_c("test_stim1_x: ",
                         rep('test_x', n_trials), 
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 0, "]")),
                   str_replace_all(mem_x1, "mem_stim1_x: ", "test_stim1_x: ")
  ),
  test_x2 = ifelse(type == "type: 'different'",
                   str_c("test_stim2_x: ",
                         rep('test_x', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 1, "]")),
                   str_replace_all(mem_x2, "mem_stim2_x: ", "test_stim2_x: ")
  ),
  test_x3 = ifelse(type == "type: 'different'",
                   str_c("test_stim3_x: ",
                         rep('test_x', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 2, "]")),
                   str_replace_all(mem_x3, "mem_stim3_x: ", "test_stim3_x: ")
  ),  
  test_x4 = ifelse(type == "type: 'different'",
                   str_c("test_stim4_x: ",
                         rep('test_x', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 3, "]")),
                   str_replace_all(mem_x4, "mem_stim4_x: ", "test_stim4_x: ")
  ),
  test_x5 = ifelse(type == "type: 'different'",
                   str_c("test_stim5_x: ",
                         rep('test_x', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 4, "]")),
                   str_replace_all(mem_x5, "mem_stim5_x: ", "test_stim5_x: ")
  ),
  # Y-Coordinates of test items
  test_y1 = ifelse(type == "type: 'different'",
                   str_c("test_stim1_y: ",
                         rep('test_y', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 0, "]")),
                   str_replace_all(mem_y1, "mem_stim1_y: ", "test_stim1_y: ")
  ),
  test_y2 = ifelse(type == "type: 'different'",
                   str_c("test_stim2_y: ",
                         rep('test_y', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 1, "]")),
                   str_replace_all(mem_y2, "mem_stim2_y: ", "test_stim2_y: ")
  ),
  test_y3 = ifelse(type == "type: 'different'",
                   str_c("test_stim3_y: ",
                         rep('test_y', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 2, "]")),
                   str_replace_all(mem_y3, "mem_stim3_y: ", "test_stim3_y: ")
  ),
  test_y4 = ifelse(type == "type: 'different'",
                   str_c("test_stim4_y: ",
                         rep('test_y', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 3, "]")),
                   str_replace_all(mem_y4, "mem_stim4_y: ", "test_stim4_y: ")
  ),
  test_y5 = ifelse(type == "type: 'different'",
                   str_c("test_stim5_y: ",
                         rep('test_y', n_trials),
                         str_c("[", seq(0, n_trials-1, 1), "]"),
                         str_c("[", 4, "]")),
                   str_replace_all(mem_y5, "mem_stim5_y: ", "test_stim5_y: ")
  ),
  color1 = str_c("stim1_color: '", rep(c("#0070C0", "#00B050", "#BB0703", "#E836D3", "#FFC000"), times = ceiling(n_trials/5)), "'"),
  color2 = str_c("stim2_color: '", rep(c("#00B050", "#BB0703", "#E836D3", "#FFC000", "#0070C0"), times = ceiling(n_trials/5)), "'"),
  color3 = str_c("stim3_color: '", rep(c("#BB0703", "#E836D3", "#FFC000", "#0070C0", "#00B050"), times = ceiling(n_trials/5)), "'"),
  color4 = str_c("stim4_color: '", rep(c("#E836D3", "#FFC000", "#0070C0", "#00B050", "#BB0703"), times = ceiling(n_trials/5)), "'"),
  color5 = str_c("stim5_color: '", rep(c("#FFC000", "#0070C0", "#00B050", "#BB0703", "#E836D3"), times = ceiling(n_trials/5)), "'")
  ) %>%
  glue_data(
    "{{{type}, {correct_response}, {mem_x1}, {mem_x2}, {mem_x3}, {mem_x4}, {mem_x5},\n{mem_y1}, {mem_y2}, {mem_y3}, {mem_y4}, {mem_y5},\n{test_x1}, {test_x2}, {test_x3}, {test_x4}, {test_x5},\n{test_y1}, {test_y2}, {test_y3}, {test_y4}, {test_y5},\n{color1}, {color2}, {color3}, {color4}, {color5}}},\n\n",
    )
