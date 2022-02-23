
library(tidyverse)
library(psych)
library(here)
library(flextable)
library(gt)
library(Hmisc)


source(here("preregistrations", "1_pilot", "scripts", "custom_functions", "functions_corrplot.R"))
source(here("preregistrations", "1_pilot", "scripts", "3_exploratory_analyses", "1_efa.R"))
load(here("data", "1_pilot", "2_cleaned_data.Rdata"))

corr_tab_df <- cleaned_data %>%
  filter(round(scale_factor, 7) != 0.3081481) %>%
  mutate(flanker_interference = ifelse(rd_flanker > 0, sda_flanker / rd_flanker, NA)) %>%
  #filter(scale(flanker_interference) < 3.2) %>%
  select(violence_composite, unpredictability_composite,
         change_ml_v, change_ml_a, 
         cueing_neutral_EZ_v, cueing_neutral_EZ_a, cueing_cued_EZ_v, cueing_cued_EZ_a, p_flanker, a_flanker, t0_flanker, flanker_interference,
         fos_fo_mean, impuls_mean) %>%
  corr_table(
    c.names = c(
      "Violence",
      "Unpredictability",
      "Change - Drift rate",
      "Change - Bound. sep.",
      "Cueing - Drift (neut.)",
      "Cueing - Bound. sep. (neut.)",
      "Cueing - Drift (cued)",
      "Cueing - Bound. sep. (cued)",
      "Flanker - percep. input",
      "Flanker - Bound. sep.",
      "Flanker - Non-dec. time",
      "Flanker - Interference",
      "Fut. Orient.",
      "Impulsivity"
    ),
    stats = "",
    numbered = T) %>%
  select(-`14`)


my_header <- data.frame(
  col_keys = c("Variable", "1", "2", "blank1", "3", "4", "blank2", "5", "6", "7", "8", "blank3", "9", "10", "11", "12", "blank4", "13"),
  line2 =    c("Variable", rep("Adversity",2), "", rep("Change",2), "", rep("Cueing",4), "", rep("Flanker",4), "", rep("Orientation",1)),
  line3 =    c("Variable", "1", "2", "", "3", "4", "", "5", "6", "7", "8", "", "9", "10", "11", "12", "", "13")
)

corr_tab <- flextable(corr_tab_df, col_keys = my_header$col_keys, cwidth = c(1.72, rep(0.63, 13))) %>%
  font(fontname = "Times New Roman") %>%
  font(fontname = "Times New Roman", part = "header") %>%
  fontsize(size = 8, part = "all") %>%
  
  set_header_df(
    mapping = my_header,
    key = "col_keys"
  ) %>%
  fontsize(size = 8, part = "header") %>%
  theme_booktabs() %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", i = 1:11, j = 2:11) %>%
  align(align = "center", part = "header") %>%
  autofit() %>%
  empty_blanks() %>%
  fix_border_issues() %>%
  hline_top()





















