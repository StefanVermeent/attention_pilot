
library(tidyverse)
library(ggsci)
library(multitool) #devtools::install_github("ethan-young/multitool", force = T)
library(here)
library(ggeffects)
library(interactions)
library(lmerTest)
library(parameters)
library(specr)
library(psych)
library(flextable)

source("preregistrations/3_study2/scripts/custom_functions/functions_analyses.R")
source("preregistrations/3_study2/scripts/custom_functions/functions_corrplot.R")

load("data/3_study2/2_cleaned_data.rData") 

load("preregistrations/3_study2/analysis_objects/hddm_globloc_model2_objects.RData")


# 1. Bivariate correlations ------------------------------------------------------------


# Pilot study
pilot_data <- read_csv("data/1_pilot/2_cleaned_data.csv") |> 
  select(id, 
         depression_mean, impuls_mean, fos_fo_mean, ses_comp = poverty_composite,
         a_flanker = flanker_ssp_a, t0_flanker = flanker_ssp_t0, p_flanker = flanker_ssp_p, interference_flanker = flanker_ssp_interference) %>%
  filter(is.finite(interference_flanker)) |> 
  mutate(
    a_flanker           = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  )

# Study 1

study1_data <-  read_csv("data/2_study1/2_cleaned_data.csv") |> 
  select(id, 
         depression_mean, impuls_mean, fos_fo_mean, ses_comp,
         a_flanker_std, t0_flanker_std, p_flanker_std, interference_flanker_std) %>%
  rename(a_flanker = a_flanker_std,
         t0_flanker = t0_flanker_std,
         p_flanker = p_flanker_std,
         interference_flanker = interference_flanker_std) %>%
  mutate(
    a_flanker    = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) 

# Study 2
study2_data <-  read_csv("data/3_study2/2_cleaned_data.csv") |> 
  select(id,
         depression_mean, impuls_mean, fos_fo_mean, ses_comp, att_style_int, att_style_ext,
         a_flanker, t0_flanker, p_flanker, interference_flanker,
         hddm_v_local, hddm_v_global
  ) %>%
  mutate(
    v_globloc_diff   = hddm_v_global - hddm_v_local 
  ) |> 
  select(-hddm_v_global, -hddm_v_local ) |> 
  mutate(
    a_flanker    = log(a_flanker),
    interference_flanker = ifelse(scale(interference_flanker) > 3.2, NA, interference_flanker)
  ) 


# Combine data of pilot study and study 1
supp_cor_table <- bind_rows(pilot_data, study1_data, study2_data) |> 
  filter(is.finite(interference_flanker)) |> 
  select(
    depression_mean, impuls_mean, fos_fo_mean, ses_comp,
    att_style_int, att_style_ext, p_flanker, interference_flanker,
    a_flanker, t0_flanker, v_globloc_diff
  ) |>
  corr_table(
    c.names = c(
      "Depression",
      "Impulsivity",
      "Future orientation",
      "SES",
      "Internal attention style",
      "External attention style",
      "Flanker - Perceptual input",
      "Flanker - Interference",
      "Flanker - boundary separation",
      "Flanker - Non-decision time",
      "Global-Local - Drift rate difference"
    ),
    stats=F, 
    numbered = T
    ) |> 
  flextable() |> 
  border_remove() |> 
  border(i = 11, border.bottom = fp_border_default(), part = "body") |> # Add APA-style bottom border
  border(i = 1, border.top = fp_border_default(style = "none", width = 0), part = "header") |>
  border(i = 1, border.bottom = fp_border_default(), part = "header") |>
  # These next few rows are for including a Title
 # border(i = 11, border.bottom = fp_border_default(), part = "body") |> # Add APA-style bottom border
  border(i = 1, border.top = fp_border_default(style = "none", width = 0), part = "header") |>
  add_header_row(
    values = " ",
    colwidths = 12
  ) |> # Add a new header row on top. We can use this new row to add the title
  flextable::compose(
    i = 1, j = 1,
    as_paragraph(as_b("Table SX. "), "Bivariate correlations between exploratory measures and SSP parameters."),
    part = "header"
  ) |> 
  # These next few rows are for including a Footer (e.g., a note).
  add_footer_row(values = " ", colwidths = 12) |>
  add_footer_row(values = " ", colwidths = 12) |>
  flextable::compose(
    i = 1, j = 1,
    as_paragraph(as_i("Note: "), "* = ", as_i("p"), " < .05; ", "** = ", as_i("p"), " < .01; ", "*** = ", as_i("p"), " < .001. The upper diagonal presents sample sizes for each comparison."),
    part = "footer"
  ) |> 
  autofit()

save(supp_cor_table, file = "preregistrations/3_study2/analysis_objects/supp_section2.Rdata")  


# Model fit ---------------------------------------------------------------

load("preregistrations/3_study2/analysis_objects/hddm_globloc_model2_objects.RData")
  