library(tidyverse)
library(lmerTest)
library(interactions)
library(ggeffects)


# 1. Load data of all studies------------------------------------------------

suppl_mod_pooled_data <- c("data/1_pilot/2_cleaned_data.csv", "data/2_study1/2_cleaned_data.csv", "data/3_study2/2_cleaned_data.csv") |> 
  map_df(function(x){
    read_csv(x) |> 
      select(
        id, vio_comp, unp_comp, stai_s_mean, depression_mean, 
        matches("p_flanker|flanker_ssp_p"), matches("interference_flanker|flanker_ssp_interference"),
        scale_factor, fullscreenexit, attention_interrupt_sum) %>% 
      rename_with(.cols = matches("flanker_ssp_p|p_flanker_std"), .fn = ~sub(pattern = "flanker_ssp_p|p_flanker_std", "p_flanker", x = .)) |>
      rename_with(.cols = matches("flanker_ssp_interference|interference_flanker_std"), .fn = ~sub(pattern = "flanker_ssp_interference|interference_flanker_std", "interference_flanker", x = .)) |>
      mutate(
        study = deparse(x) |> str_extract_all(string = _, pattern = "pilot|study\\d")
      ) |> 
      unnest(study)
  }) |> 
  select(-c(p_flanker_deg, p_flanker_enh)) |> 
  filter(
    study == "pilot" & round(scale_factor, 3) != 0.3081 | study %in% c("study1", "study2") & round(scale_factor, 4) != 0.9007,
    fullscreenexit == 0, 
    attention_interrupt_sum < 1,
    !is.infinite(interference_flanker)
  ) |> 
  mutate(
    study    = faux::contr_code_sum(study),
    vio_comp = scale(vio_comp) |> as.numeric(),
    unp_comp = scale(unp_comp) |> as.numeric())



# 2. Main analyses --------------------------------------------------------

suppl_mod_results <- multitool::run_multiverse(
  .grid = 
    suppl_mod_pooled_data |> 
      multitool::add_variables("iv", vio_comp, unp_comp) |> 
      multitool::add_variables("dv", p_flanker, interference_flanker) |> 
      multitool::add_variables("mod", stai_s_mean, depression_mean) |> 
      multitool::add_model("lm", lm({dv} ~ {iv}*{mod} + study)) |> 
      multitool::expand_decisions()
) |> 
  multitool::reveal(.what = model_fitted, .which = lm_tidy, .unpack_specs = TRUE) |> 
  filter(str_detect(term, ":")) |> 
  select(iv, dv, mod, estimate, p.value) |> 
  pivot_wider(names_from = c(iv,dv,mod), values_from = c(estimate, p.value))


