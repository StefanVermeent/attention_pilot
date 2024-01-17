library(tidyverse)
library(here)
library(ggsci)
library(patchwork)
library(flextable)
library(specr)

source(here("preregistrations/1_pilot/scripts/custom_functions/functions_corrplot.R"))

source(here("manuscript/pilot_stage_objects.R"))
source(here("manuscript/study1_stage_objects.R"))
source(here("manuscript/study2_stage_objects.R"))

pilot_data = read_csv(here("data/1_pilot/2_cleaned_data.csv")) |> mutate(study = "pilot")
study1_data = read_csv(here("data/2_study1/2_cleaned_data.csv")) |> mutate(study = "study1")
study2_data = read_csv(here("data/3_study2/2_cleaned_data.csv")) |> mutate(study = "study2")
# IV correlations ---------------------------------------------------------

iv_cor_pooled_table <-  bind_rows(
    pilot_data |> 
      mutate(id = paste0("pilot", id)) |> 
      rename(
        pcunp_mean   = unp_mean,
        nvs_mean     = violence_mean
      ),
    study1_data |> 
      mutate(id = paste0("study1", id)),
    study2_data |> 
      mutate(id = paste0("study2", id)) |> select(-weight)
  ) |> 
select(nvs_mean, fighting_mean, vio_comp, 
       quic_total_mean, pcunp_mean, chaos_mean, change_env_mean, unp_obj_comp, unp_subj_comp, unp_comp) |> 
  corr_table(
    numbered = T,
    sample_size = F,
    c.names = c("Neigh. violence", "Fighting", "Violence comp.", 
                "QUIC", "Perc. unpredictability", "CHAOS", "Env. change", "Obj. unpredictability", "Subj. Unpredictability", "Unpredictability comp."),
    stats = c("mean", "sd", "median", "min", "max", "skew", "kurtosis")
  ) |> 
  add_column(empty1 = "", .after = 4) |> 
  mutate(across(
    everything(),
    ~ifelse(is.na(.), "", .)
  )) |> 
  mutate(across(
    everything(),
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f'))) |> 
  flextable() |> 
  width("empty1", width = .2) |> 
  width(j = 1, width = .75) |> 
  set_header_labels(Variable = "", empty1 = "") |> 
  add_header_row(
    values = c(" ", "Violence exposure", " ", "Environmental unpredictability"),
    colwidths = c(1, 3, 1, 7)
  ) |> 
  border_remove() |> 
  border(i = 1, j = c("1","2","3","4","5","6","7","8","9"), border.bottom = fp_border_default(), part = "header") |> 
  border(i = 1, border.top = fp_border_default(), part = "header") |> 
  border(i = 2, border.bottom = fp_border_default(), part = "header") |> 
  border(i = 17, border.bottom = fp_border_default(), part = "body") |> 
  align(i = 1:12, j = 1, align = "left", part = "body") |> 
  bold(i = 1:2, part = "header") |> 
  align(i = 1:2, align = c("center"), part = "header") |> 
  set_table_properties(width = 1, layout = "autofit")





demographics_table <- list(
      n = map_dfr(list(pilot_data, study1_data, study2_data), function(x){
        x |> 
          summarise(stat = n() |> as.character()) |> 
          mutate(categories = "N") |> 
          mutate(study = x |> pull(study) |> unique())
      }),
      age = map_dfr(list(pilot_data, study1_data, study2_data), function(x) { 
        x |> 
        summarise(stat = paste0(paste0(round(mean(dems_age, na.rm = T),2), " (", round(sd(dems_age, na.rm = T), 2), ")"))) |> 
        mutate(categories = "Mean age (SD)") |> 
        mutate(study = x |> pull(study) |> unique())
        }),
      sex = map_dfr(list(pilot_data, study1_data, study2_data), function(x) { 
        x |> 
        group_by(dems_sex) |> 
        tally() |> 
        mutate(stat = formatC(round(n/sum(n)*100,2), digits = 1, width = 3, flag = "0", format = 'f')) |> 
        mutate(
          categories = case_when(
            dems_sex == 0 ~ "sex - Male",
            dems_sex == 1 ~ "sex - Female",
            dems_sex == 2 ~ "sex - Intersex",
            dems_sex == 3 ~ "sex - Prefer not to say"
          ),
          study = x |> pull(study) |> unique()
        )}),
     #ethnicity = map_dfr(list(pilot_data, study1_data, study2_data), function(x) { 
     #  x |> 
     #    mutate(dems_ethnicity = ifelse(dems_ethnicity_mixed == 1, "Mixed", dems_ethnicity)) |> 
     #    group_by(dems_ethnicity) |> 
     #    tally() |> 
     #    mutate(stat = formatC(round(n/sum(n)*100,2), digits = 1, width = 3, flag = "0", format = 'f')) |> 
     #    mutate(categories = forcats::fct_relevel(dems_ethnicity, "White, Caucasian, Anglo, European American", 
     #                                             "Asian or Asian American (e.g., Chinese, Japanese, and others)", 
     #                                             "Hispanic of Latino (e.g., Mexican American, Central American, and others)", 
     #                                             "Black or African American", "Filipino", "Native American/Alaskan Native/indigenous", 
     #                                             "Middle Eastern", "Mixed", "Prefer not to say")) |> 
     #    arrange(categories) |> 
     #    mutate(
     #      categories = paste0("eth - ", categories),
     #      study = x |> pull(study) |> unique())
     #}),
      dems_edu = map_dfr(list(pilot_data, study1_data, study2_data), function(x) { 
        x |> 
        mutate(
          dems_edu = case_when(
            dems_edu == 1 ~ "Some high school",
            dems_edu == 2 ~ "GED",
            dems_edu == 3 ~ "High school diploma",
            dems_edu == 4 ~ "Some college but no college degree",
            dems_edu == 5 ~ "Associate's degree",
            dems_edu == 6 ~ "Bachelor's or RN degree",
            dems_edu == 7 ~ "Master's degree",
            dems_edu == 8 ~ "Doctoral or law degree",
            dems_edu == 9 ~ "Prefer not to say"
          )
        ) |> 
        group_by(dems_edu) |> 
        tally() |> 
        mutate(stat = formatC(round(n/sum(n)*100,2), digits = 1, width = 3, flag = "0", format = 'f')) |> 
        mutate(categories = forcats::fct_relevel(dems_edu, "Some high school", "GED", "High school diploma", "Some college but no college degree",
                                                 "Associate's degree","Bachelor's or RN degree","Master's degree","Doctoral or law degree")) |> 
        arrange(categories) |> 
          mutate(
            study = x |> pull(study) |> unique(),
            categories = paste0("edu - ", categories))
        }),
      dems_class = map_dfr(list(pilot_data, study1_data, study2_data), function(x) { 
        x |> 
          mutate(
            dems_class_current = case_when(
              dems_class_current == 1 ~ "Poor",
              dems_class_current == 2 ~ "Working class",
              dems_class_current == 3 ~ "Middle class",
              dems_class_current == 4 ~ "Upper-middle class",
              dems_class_current == 5 ~ "Upper class",
              dems_class_current == 6 ~ "Don't know/prefer not to say",
            )
          ) |> 
          group_by(dems_class_current) |> 
          tally() |> 
          mutate(stat = formatC(round(n/sum(n)*100,2), digits = 1, width = 3, flag = "0", format = 'f')) |> 
          mutate(categories = forcats::fct_relevel(dems_class_current, "Poor","Working class","Middle class","Upper-middle class",
                                                   "Upper class")) |> 
          arrange(categories) |> 
          mutate(
            study = x |> pull(study) |> unique(),
            categories = paste0("class - ", categories))
      })
    ) |> 
      bind_rows() |> 
      select(categories, stat, study) |> 
      pivot_wider(names_from = study, values_from = stat) |> 
      mutate(categories = str_remove(categories, ".*-\\s")) |> 
      mutate(across(c(pilot, study1, study2), ~ifelse(is.na(.), "0", .))) |> 
      add_row(.after = 2, categories = "Sex (%)") |> 
     # add_row(.after = 7, categories = "Ethnicity (%)") |> 
      add_row(.after = 7, categories = "Highest education (%)") |> 
      add_row(.after = 17, categories = "Social class (%)") |> 
      flextable() |> 
      set_header_labels(
        categories = "",
        pilot = "Pilot study",
        study1 = "Study 1",
        study2 = "Study 2"
      ) |> 
      border_remove() |> 
      border(i = 1, border.top = fp_border_default(), part = "header") |> 
      border(i = 1, border.top = fp_border_default(), part = "body") |> 
      border(i = 24, border.bottom = fp_border_default(), part = "body") |> 
      set_table_properties(width = 1, layout = "autofit") |> 
      bold(i = c(1, 2, 3, 8, 18), j = 1) |> 
      bold(i = 1, j = 2:4, part = "header") |> 
      padding(i=c(4:7, 9:17, 19:24), j=1, padding.left=20)
    


# Pooled Flanker results --------------------------------------------------
    
## General ggplot theme for plots
theme_set(
  theme_bw() +
    theme(
      axis.line.y       = element_line(),
      axis.line.x       = element_line(),
      axis.text.y       = element_text(size = 14),
      axis.text.x       = element_text(size = 14),
      axis.title.y      = element_text(size = rel(1), margin = margin(1,0,0,0,"lines")),
      axis.ticks.y      = element_line(),
      
      panel.border      = element_blank(), 
      panel.spacing.y   = unit(0.5, "lines"),
      plot.margin       = margin(.25,.25,.25,.25,"lines"),
      plot.background   = element_rect(color = NA),
      plot.title        = element_text(size = rel(.85), hjust = 0, margin = margin(0,0,.5,0, "lines")),
      plot.subtitle     = element_blank(),
      panel.grid        = element_line(color = NA),
      strip.background  = element_blank(), 
      strip.placement   = "outside",
      strip.text        = element_text(size = rel(.85), angle = 0),
      legend.background = element_rect(fill='transparent'), #
      legend.box.background = element_rect(color = 'transparent', fill='transparent'),
      legend.position = "bottom"
    )
)


mult_variance_vio_p_pooled <- 
  bind_rows(
    pilot_prim_lm_variance_sum$flanker_ssp_p |> 
      mutate(study = "Pilot"),
    study1_prim_ssp_variance_sum_study1$p_flanker |> 
      mutate(study = "Study 1"),
    study2_prim_aim1_variance_sum_study2$p_flanker.vio_comp |> 
      mutate(
        study = "Study 2",
        grp = as.character(grp),
        grp = ifelse(grp == "Scaling", "Scaled", grp),
        grp = ifelse(grp == "Interruptions", "Interrupted", grp)),
    study2_prim_aim1_variance_sum$p_flanker.vio_comp |> 
      mutate(
        study = "Pooled",
        grp = as.character(grp),
        grp = ifelse(grp == "Scaling", "Scaled", grp),
        grp = ifelse(grp == "Interruptions", "Interrupted", grp)
      )
  ) |>
  mutate(
    study = factor(study, levels = c("Pilot", "Study 1", "Study 2", "Pooled"))
  ) |> 
  ggplot(aes(grp, percent, fill = study, group = study)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_uchicago() +
  ylim(0, 100) +
  coord_flip() +
  labs(
    x = "",
    y = ""
  )

mult_variance_vio_interference_pooled <- 
  bind_rows(
    pilot_prim_lm_variance_sum$flanker_ssp_interference |> 
      mutate(study = "Pilot"),
    study1_prim_ssp_variance_sum_study1$interference_flanker |> 
      mutate(study = "Study 1"),
    study2_prim_aim1_variance_sum$interference_flanker.vio_comp |> 
      mutate(
        study = "Study 2",
        grp = as.character(grp),
        grp = ifelse(grp == "Scaling", "Scaled", grp),
        grp = ifelse(grp == "Interruptions", "Interrupted", grp)),
    study2_prim_aim1_variance_sum$interference_flanker.vio_comp |> 
      mutate(
        study = "Pooled",
        grp = as.character(grp),
        grp = ifelse(grp == "Scaling", "Scaled", grp),
        grp = ifelse(grp == "Interruptions", "Interrupted", grp)
      )
  ) |>
  mutate(
    study = factor(study, levels = c("Pilot", "Study 1", "Study 2", "Pooled"))
  ) |> 
  ggplot(aes(grp, percent, fill = study, group = study)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_uchicago() +
  ylim(0, 100) +
  coord_flip() +
  labs(
    x = "",
    y = ""
  )


study2_aim1_plot <- 
  # Row 1: Pilot 
  (
    (pilot_prim_lm_eff_curve_plot$flanker_ssp_p + ggtitle("Perceptual input")) +
    (pilot_prim_lm_eff_curve_plot$flanker_ssp_interference + ggtitle("Interference") + right_hand_themes) + plot_layout(ncol = 2)
  ) /
  # Row 2: Study 1
  (
    (study1_prim_ssp_eff_curve_plot_study1$p_flanker) +
    (study1_prim_ssp_eff_curve_plot_study1$interference_flanker + right_hand_themes) + plot_layout(ncol = 2)
  ) /
  # Row 3: Study 2
  (
    (study2_prim_aim1_eff_curve_plot_study2$p_flanker.vio_comp) +
    (study2_prim_aim1_eff_curve_plot_study2$interference_flanker.vio_comp + right_hand_themes) + plot_layout(ncol = 2)
  ) /
  # Row 4: Pooled
  (
    (study2_prim_aim1_eff_curve_plot$p_flanker.vio_comp) +
      (study2_prim_aim1_eff_curve_plot$interference_flanker.vio_comp + right_hand_themes) + plot_layout(ncol = 2)
  ) /
  # Row 5: Multiverse filters
  (
    (mult_variance_vio_p_pooled + guides(fill = 'none')) +
      (mult_variance_vio_interference_pooled + right_hand_themes + theme(legend.position = "right")) + plot_layout(ncol = 2)
  ) 

ggsave(plot = study2_aim1_plot, filename = "manuscript/figures/fig1.png", height=14, width = 9)
