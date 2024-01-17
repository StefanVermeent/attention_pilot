library(tidyverse)
library(patchwork)
library(ggsci)
library(here)
library(flextable)


source(here("preregistrations/1_pilot/scripts/custom_functions/functions_corrplot.R"))


cleaned_data = read_csv(here("data/1_pilot/2_cleaned_data.csv"))
pilot_data <- cleaned_data # for easy referencing in the manuscript
load(file = here("preregistrations/1_pilot/analysis_objects/primary_mult_summaries.RData"))
load(file = here("preregistrations/1_pilot/analysis_objects/exploratory_mult_summaries.RData"))

get_alphas <- function(data, string) {
  (data |> 
     select(matches(string)) |> 
     psych::alpha(check.keys = TRUE))$total[[1]] |> round(2)
}



# Descriptives ------------------------------------------------------------

## IV distributions ----
txt_ivs_dist_pilot <- 
  cleaned_data |> select(matches("(mean|total|composite|edu_parents_recoded|age)$"),
                         matches("^(acc|rt)_(flanker|cueing)"), acc_change, rt_change) |> 
  map(function(x) {
    list(
      mean = round(mean(x, na.rm = T),2),
      sd   = round(sd(x, na.rm = T),2)
    )
  })

txt_ivs_alpha_pilot <- 
  c("stai_s\\d\\d", "violence\\d\\d", "unp\\d\\d", 
    "quic(01|02|03|04|05|06|07|08|09)", "quic(10|11|12|13|14|15|16|17|18|19|20|21)", "quic(22|23|24|25|26|27)", "quic(28|29|30|31|32|33|34)", "quic(35|36|37)",
    "quic.*\\d\\d", "chaos\\d\\d", "ses\\d\\d", "impuls\\d\\d", 
    "fos(01|06|07|12|13)", "fos(02|05|08|11|14)", "fos(03|04|09|10|15)", "fos.*\\d\\d", "depression\\d\\d") |> 
  map(function(x) {
    (cleaned_data |> 
       select(matches(x)) |> 
       psych::alpha(check.keys = TRUE))$total[[1]] |> round(2)
  }) |> 
  setNames(c("stai_s", "violence", "unp", 
             "quic_monitoring", "quic_par_predict", "quic_par_env", "quic_phys_env", "quic_safety",
             "quic_total", "chaos", "ses", "impuls", "fos_pa", "fos_tp", "fos_fc", "fos_fo", "depression"))


## Sample characteristics ----

sample_pilot <- cleaned_data |> 
  select(starts_with("dems")) |> 
  mutate(
    dems_ethnicity = ifelse(dems_ethnicity_mixed == 1, "Mixed", dems_ethnicity),
    dems_gender = case_when(
      dems_gender == 1 ~ "Man",
      dems_gender == 2 ~ "Woman",
      dems_gender == 3 ~ "Non-binary",
      dems_gender == 4 ~ "Other",
      dems_gender == 5 ~ "Prefer not to say"
    ))


# Main results  -----------------------------------------------------------

## Plots ----

## General ggplot theme for plots ----
theme_set(
  theme_bw() +
    theme(
      axis.line.y       = element_line(),
      axis.line.x       = element_line(),
      axis.text.y       = element_text(size = rel(1)),
      axis.title.y      = element_text(size = rel(1), margin = margin(1,0,0,0,"lines")),
      axis.ticks.y      = element_line(),
      
      panel.border      = element_blank(), 
      panel.spacing.y   = unit(0.5, "lines"),
      plot.margin       = margin(.25,.25,.25,.25,"lines"),
      plot.background   = element_rect(color = NA),
      plot.title        = element_text(size = 14, hjust = 0.5),
      plot.subtitle     = element_blank(),
      panel.grid        = element_line(color = NA),
      strip.background  = element_blank(), 
      strip.placement   = "outside",
      strip.text        = element_text(size = rel(.85), angle = 0),
      legend.background = element_rect(fill='transparent'), #
      legend.box.background = element_rect(color = 'transparent', fill='transparent'),
    )
)

pval_colors <- c("sig" = "#006D77", "non-sig" = "gray70")

## Objects containing all plot panels ----

pilot_prim_lm_effect_plot <- unique(pilot_prim_lm_effects_sum$dv) |> 
  map(function(x) {
    
    effect_data <- pilot_prim_lm_effects_sum |> 
      filter(dv == x) |> 
      select(decision, dv, term, estimate, p.value) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(names_from = term, values_from = c(estimate, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value_vio_comp < .05, "sig", "non-sig")
      )
    
    vio_min <- cleaned_data$vio_comp |> scale() |>  min()
    vio_max <- cleaned_data$vio_comp |> scale() |>  max()
    y_min <- min(effect_data$estimate_intercept) + vio_max*min(effect_data$estimate_vio_comp) -0.05
    y_max <- max(effect_data$estimate_intercept) + vio_max*max(effect_data$estimate_vio_comp) +0.05
    int_med <- median(effect_data$estimate_intercept)
    
    ggplot() +
      # coord_cartesian(xlim = c(vio_min, vio_max), ylim = c(y_min, y_max)) +
      geom_segment(data = effect_data, aes(x = vio_min, xend = vio_max, y = int_med+estimate_vio_comp*vio_min, yend = int_med+estimate_vio_comp*vio_max, color = p.value_chr)) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = "Violence Exposure",
        y = ""
      ) +
      geom_segment(data = pilot_prim_lm_medians_sum[[x]], aes(x = vio_min, xend = vio_max, y = int_med+med_effect_vio_comp*vio_min, yend = int_med+med_effect_vio_comp*vio_max), size = 1.5) + 
      geom_point(data = pilot_prim_lm_medians_sum[[x]], aes(x = vio_min, y = int_med+vio_min*med_effect_vio_comp), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = pilot_prim_lm_medians_sum[[x]], aes(x = vio_max, y = int_med+vio_max*med_effect_vio_comp), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(pilot_prim_lm_effects_sum$dv))

pilot_prim_lm_eff_curve_plot <- unique(pilot_prim_lm_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_prim_lm_effects_sum |> 
      filter(dv == x, term != "(Intercept)") |>
      mutate(
        p.value_chr = ifelse(p.value < .05, "sig", "non-sig"),
        n = n()) |> 
      arrange(Std_Coefficient) |> 
      mutate(order = seq(1:n())) |> 
      ggplot(aes(order, Std_Coefficient, color = p.value_chr)) +
      geom_ribbon(
        aes(ymin = Std_CI_low, ymax = Std_CI_high, x = order),
        fill = "gray90",
        inherit.aes = F,
        show.legend = F
      ) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), limits = c(-.35, .35)) +
      geom_hline(aes(yintercept = 0), size = .5,linetype = "solid") +
      geom_point(size = 3, shape = 19, show.legend = F) +
      geom_point(
        data = pilot_prim_lm_medians_sum[[x]],
        aes(y = med_effect_std, x = 32),
        shape = 1,
        size  = 3,
        fill  = "white",
        stroke = 3,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 32, y = 0.28, label = paste0(sum(p.value < .05)/n*100, " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_text(
        data = pilot_prim_lm_medians_sum[[x]],
        aes(y = med_effect_std, label = paste0("\u03b2\ = ", as.character(round(med_effect_std,2))), x = 32),
        nudge_y = -.06,
        size = 4,
        show.legend = F,
        inherit.aes = F
      ) +
      scale_color_manual(values = pval_colors) +
      labs(
        y = "",
        x = ""
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()
      )
  }) |> 
  setNames(unique(pilot_prim_lm_effects_sum$dv))

# Plot lm p-value curve
pilot_prim_lm_pvalues_plot <- unique(pilot_prim_lm_effects_sum$dv) |>
  map(function(x) {
    
    pvalues <- pilot_prim_lm_effects_sum |>
      filter(dv == x, term != "(Intercept)") |> 
      ungroup() |> 
      select(decision, p.value)  
    
    
    pvalues |> 
      ggplot(aes(p.value)) +
      geom_histogram(color = "black", size = .2) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      geom_text(
        data = pvalues %>% summarize(p = paste0(as.character(sum(p.value < .05)/n()*100), " % of ps < .05"), x = (min(p.value)+max(p.value))/2, y = 15),
        aes(x = x, label = p, y = y),
        size = 2.25,
        hjust = 0,
        vjust = 1,
        show.legend = F,
        inherit.aes = F
      ) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      scale_y_continuous("Freq", expand = c(0,.15)) +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = rel(.75)),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
  }) |> 
  setNames(unique(pilot_prim_lm_effects_sum$dv))

pilot_prim_lm_variance_plot <- names(pilot_prim_lm_variance_sum) |>
  map(function(x) {
    
    pilot_prim_lm_variance_sum[[x]] |> 
      ggplot(aes(grp, percent, fill = grp)) +
      geom_bar(stat = "identity") +
      scale_fill_uchicago() +
      ylim(0, 100) +
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = ""
      )
  }) |> 
  setNames(names(pilot_prim_lm_variance_sum))



pilot_prim_lmer_effect_plot <- names(pilot_prim_lmer_points_sum) |> 
  map(function(x) {
    
    pilot_prim_lmer_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = level,
        condition = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        condition = case_when(
          str_detect(x, "flanker") & condition == -1 ~ "Congruent",
          str_detect(x, "flanker") & condition == 1 ~ "Incongruent",
          str_detect(x, "cueing") & condition == -1 ~ "Neutral",
          str_detect(x, "cueing") & condition == 1 ~ "Cued",
        )
      ) |> 
      ggplot(aes(condition, predicted, color = p.value_chr, group = decision)) +
      geom_point(size = .5, show.legend = F) +
      geom_line(size = .5, show.legend = F) +
      scale_color_manual(values = pval_colors) +
      stat_summary(
        aes(x = condition, y = predicted, group = iv), 
        geom = "line", 
        fun = "median", 
        color = "black", 
        alpha = 1,
        size = 1,
        show.legend = F
      ) +
      stat_summary(
        aes(x = condition, y = predicted, group = iv, fill = iv),
        geom = "point", 
        fun = "median", 
        color = "black", 
        shape = 21,
        stroke = 1, 
        alpha = 1,
        size = 2, 
        show.legend = T
      ) +
      guides(color = 'none') +
      scale_fill_manual("Violence\nExposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = ""
      ) +
      theme(
        legend.position=c(0.8,0.7),
        legend.background = element_blank())
  }) |> 
  setNames(names(pilot_prim_lmer_points_sum))

pilot_prim_lmer_eff_curve_plot <- unique(pilot_prim_lmer_effects_sum$dv) |> 
  map(function(x) {
    
    pilot_prim_lmer_effects_sum |> 
      filter(dv == x, str_detect(term, ":")) |>
      mutate(p.value_chr = ifelse(p.value < .05, "sig", "non-sig"),
             n = n()) |> 
      arrange(Std_Coefficient) |> 
      mutate(order = seq(1:n())) |> 
      ggplot(aes(order, Std_Coefficient, color = p.value_chr)) +
      geom_ribbon(
        aes(ymin = Std_CI_low, ymax = Std_CI_high, x = order),
        fill = "gray90",
        inherit.aes = F,
        show.legend = F
      ) +
      ylim(-.25, .25) +
      geom_hline(aes(yintercept = 0), size = .5,linetype = "solid") +
      geom_point(size = 1, shape = 19, show.legend = F) +
      geom_point(
        data = pilot_prim_lmer_medians_sum[[x]],
        aes(y = med_effect_std, x = 64),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 64, y = 0.18, label = paste0(sum(p.value < .05)/n*100, " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = pilot_prim_lmer_medians_sum[[x]],
        aes(y = med_effect_std, label = as.character(round(med_effect_std,2)), x = 64),
        nudge_y = .055,
        size = 2.5,
        show.legend = F,
        inherit.aes = F
      ) +
      scale_color_manual(values = pval_colors) +
      labs(
        y = "",
        x = ""
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()
      )
  }) |> 
  setNames(unique(pilot_prim_lmer_effects_sum$dv))


pilot_prim_lmer_pvalues_plot <- names(pilot_prim_lmer_points_sum) |>
  map(function(x) {
    
    pvalues <- pilot_prim_lmer_points_sum[[x]] |> 
      select(decision, p.value) |> 
      distinct()
    
    pvalues |> 
      ggplot(aes(p.value)) +
      geom_histogram(color = "black", size = .2) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      geom_text(
        data = pvalues %>% summarize(p = paste0(as.character(sum(p.value < .05)/n()*100), " % of ps < .05"), y = 15),
        aes(x = .3, label = p, y = y),
        size = 2.25,
        hjust = 0,
        vjust = 1,
        show.legend = F,
        inherit.aes = F
      ) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      scale_y_continuous("Freq", expand = c(0,.15)) +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = rel(.75)),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
  }) |> 
  setNames(names(pilot_prim_lmer_points_sum))

pilot_prim_lmer_variance_plot <- names(pilot_prim_lmer_variance_sum) |>
  map(function(x) {
    
    pilot_prim_lmer_variance_sum[[x]] |> 
      ggplot(aes(grp, percent, fill = grp)) +
      geom_bar(stat = "identity") +
      scale_fill_uchicago() +
      ylim(0, 100) +
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = ""
      )
  }) |> 
  setNames(names(pilot_prim_lmer_variance_sum))





## Multiverse Figure: Violence Exposure ----

right_hand_themes <- theme(
  axis.text.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank()
)

pilot_mult_vio_ddm <-
  # Row 1: Slopes
  (pilot_prim_lmer_effect_plot$cueing_hddm_v + ggtitle("Cued Attention Task\nDrift rate") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$change_hddm_v + ggtitle("Change Detection Task\nDrift rate") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$flanker_ssp_p + ggtitle("Flanker Task\nPerceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$flanker_ssp_interference + ggtitle("Flanker task\nInterference") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 4)) /
  # Row 2: Effect curves
  ((pilot_prim_lmer_eff_curve_plot$cueing_hddm_v + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_eff_curve_plot$change_hddm_v + right_hand_themes) + 
     (pilot_prim_lm_eff_curve_plot$flanker_ssp_p + right_hand_themes) + 
     (pilot_prim_lm_eff_curve_plot$flanker_ssp_interference + right_hand_themes) + plot_layout(ncol = 4)) +
  # Row 3: Explained variances
  ((pilot_prim_lmer_variance_plot$cueing_hddm_v + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_variance_plot$change_hddm_v + right_hand_themes) + 
     (pilot_prim_lm_variance_plot$flanker_ssp_p + right_hand_themes) + 
     (pilot_prim_lm_variance_plot$flanker_ssp_interference + right_hand_themes) + plot_layout(ncol = 4)) 

pilot_mult_vio_ddm_flanker <-
  # Row 1: Slopes
     (pilot_prim_lm_effect_plot$flanker_ssp_p + ggtitle("Flanker Task\nPerceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$flanker_ssp_interference + ggtitle("Flanker task\nInterference") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 2)) /
  # Row 2: Effect curves
  ((pilot_prim_lm_eff_curve_plot$flanker_ssp_p + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_eff_curve_plot$flanker_ssp_interference + right_hand_themes) + plot_layout(ncol = 2)) +
  # Row 3: Explained variances
  ((pilot_prim_lm_variance_plot$flanker_ssp_p + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_variance_plot$flanker_ssp_interference + right_hand_themes) + plot_layout(ncol = 2)) 



pilot_mult_vio_ddm <-
  # Row 1: Slopes
  (pilot_prim_lmer_effect_plot$cueing_hddm_v + ggtitle("Cued Attention Task\nDrift rate") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$change_hddm_v + ggtitle("Change Detection Task\nDrift rate") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$flanker_ssp_p + ggtitle("Flanker Task\nPerceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$flanker_ssp_interference + ggtitle("Flanker task\nInterference") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 4)) /
  # Row 2: Effect curves
  ((pilot_prim_lmer_eff_curve_plot$cueing_hddm_v + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_eff_curve_plot$change_hddm_v + right_hand_themes) + 
     (pilot_prim_lm_eff_curve_plot$flanker_ssp_p + right_hand_themes) + 
     (pilot_prim_lm_eff_curve_plot$flanker_ssp_interference + right_hand_themes) + plot_layout(ncol = 4)) +
  # Row 3: Explained variances
  ((pilot_prim_lmer_variance_plot$cueing_hddm_v + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_variance_plot$change_hddm_v + right_hand_themes) + 
     (pilot_prim_lm_variance_plot$flanker_ssp_p + right_hand_themes) + 
     (pilot_prim_lm_variance_plot$flanker_ssp_interference + right_hand_themes) + plot_layout(ncol = 4)) 


pilot_mult_vio_rt <-
  # Row 1: Slopes
  (pilot_prim_lmer_effect_plot$cueing_rt + ggtitle("Cued Attention Task\nRT") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lm_effect_plot$rt_change + ggtitle("Change Detection Task\nRT") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     pilot_prim_lmer_effect_plot$flanker_rt + ggtitle("Flanker Task\nRT") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 3)) /
  # Row 2: Effect curves
  ((pilot_prim_lmer_eff_curve_plot$cueing_rt + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_eff_curve_plot$rt_change + right_hand_themes) + 
     (pilot_prim_lmer_eff_curve_plot$flanker_rt + right_hand_themes) + plot_layout(ncol = 3)) /
  # Row 3: Explained variances
  ((pilot_prim_lmer_variance_plot$cueing_rt + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (pilot_prim_lm_variance_plot$rt_change + right_hand_themes) + 
     (pilot_prim_lmer_variance_plot$flanker_rt + right_hand_themes) + plot_layout(ncol = 3)) 


## Simple Slopes: Violence exposure ----

pilot_simslopes <- pilot_prim_lmer_simslopes_sum |> 
#  filter(dv %in% "cueing_hddm_v", dv == "flanker_rt") |> 
  mutate(condition = ifelse(modx.value == -1, "con1", "con2")) |> 
  select(decision, dv,  estimate, p.value, condition) |> 
  group_by(dv, condition) |> 
  summarise(
    mean_est = round(mean(estimate),2),
    sum_p    = round(sum(p.value < .05)/n()*100,2)
  ) |> 
  pivot_wider(names_from = "condition",
              values_from = c("mean_est", "sum_p"))




## Main effects: Violence exposure ----

pilot_main_eff_rt_df <- pilot_prim_lmer_effects_sum |> 
  filter(dv %in% c("flanker_rt", "cueing_rt")) |> 
  filter(str_detect(term, "vio_comp|:")) |> 
  group_by(dv, term) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p.value < .05)/n()*100) |> 
  mutate(term = ifelse(str_detect(term, ":"), "Interaction", "Main")) |> 
  pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
  mutate(
    Interaction_CI = paste0("[", median_CI_low_Interaction, ", ", median_CI_high_Interaction, "]"),
    Main_CI        = paste0("[", median_CI_low_Main, ", ", median_CI_high_Main, "]"),
    dv = ifelse(dv == "cueing_rt", "Cued Attention", "Flanker")
  ) |> 
  select(dv, median_effect_Main, Main_CI, p_sum_Main, median_effect_Interaction, Interaction_CI, p_sum_Interaction) |> 
  bind_rows(
    pilot_prim_lm_effects_sum |> 
      filter(dv %in% c("rt_change")) |> 
      filter(term == "vio_comp") |> 
      group_by(dv) |> 
      summarise(
        median_effect_Main = round(median(Std_Coefficient),2),
        median_CI_low = round(median(Std_CI_low), 2),
        median_CI_high = round(median(Std_CI_high), 2),
        p_sum_Main = sum(p.value < .05)/n()*100) |> 
      mutate(
        Main_CI = paste0("[", median_CI_low, ", ", median_CI_high, "]"),
        dv = "Change Detection") |> 
      select(-median_CI_low, -median_CI_high)
  ) |> 
  ungroup() |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate(across(
    -dv,
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  ))  

pilot_main_eff_rt_table <- 
  pilot_main_eff_rt_df |> 
  add_column(empty1 = " ", .after = 4) |> 
  flextable::flextable() |> 
  flextable::set_header_labels(
    dv = "Task", 
    empty1 = ""
  ) |> 
  add_header_row(
    values = c(" ", "Main Effect", "", "Interaction"),
    colwidths = c(1, 3, 1, 3)
  ) |> 
  compose(i = 2, j = c(2,6), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 2, j = c(4,8), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  compose(i = 2, j = c(3,7), as_paragraph("95% CI"), part = "header") %>% 
  align(i = 1:2, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, j = c(2:4,6:7), border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 2, border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 3, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1:2, part = "header") %>% 
  set_table_properties(width = 1, layout = "autofit") %>% 
  add_footer_row(
    values = " ",
    colwidths = 8
  ) %>% 
  add_footer_row(
    values = " ",
    colwidths = 8
  ) %>% 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse."), 
    part = "footer"
  )


pilot_main_eff_ddm_df <- pilot_prim_lmer_effects_sum |> 
  filter(dv %in% c("cueing_rt", "cueing_hddm_v", "flanker_rt", "cueing_hddm_t")) |> 
  filter(str_detect(term, "vio_comp|:")) |> 
  group_by(dv, term) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = formatC(round(median(Std_CI_low), 2), digits = 2, width = 3, flag = "0", format = 'f'),
    median_CI_high = formatC(round(median(Std_CI_high), 2), digits = 2, width = 3, flag = "0", format = 'f'),
    p_sum = sum(p.value < .05)/n()*100) |> 
  mutate(term = ifelse(str_detect(term, ":"), "Interaction", "Main")) |> 
  pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
  mutate(
    Interaction_CI = paste0("[", median_CI_low_Interaction, ", ", median_CI_high_Interaction, "]"),
    Main_CI        = paste0("[", median_CI_low_Main, ", ", median_CI_high_Main, "]"),
    dv             = case_when(
      dv == "cueing_hddm_v" ~ "Cued Attention (v)",
      dv == "cueing_hddm_t" ~ "Cued Attention (t0)",
      dv == "cueing_rt"     ~ "Cued Attention (rt)",
      dv == "flanker_rt"    ~ "Flanker (rt)")
  ) |> 
  select(dv, median_effect_Main, Main_CI, p_sum_Main, median_effect_Interaction, Interaction_CI, p_sum_Interaction) |> 
  bind_rows(
    pilot_prim_lm_effects_sum |> 
      filter(dv %in% c("cueing_fixed_hddm_a", "flanker_ssp_p", "flanker_ssp_t0", "flanker_ssp_interference", "flanker_ssp_a", 
                       "change_hddm_v", "change_hddm_t", "change_hddm_a", "rt_change")) |> 
      filter(term == "vio_comp") |> 
      group_by(dv) |> 
      summarise(
        median_effect_Main = round(median(Std_Coefficient),2),
        median_CI_low = formatC(round(median(Std_CI_low), 2), digits = 2, width = 3, flag = "0", format = 'f'),
        median_CI_high = formatC(round(median(Std_CI_high), 2), digits = 2, width = 3, flag = "0", format = 'f'),
        p_sum_Main = sum(p.value < .05)/n()*100) |> 
      mutate(
        Main_CI = paste0("[", median_CI_low, ", ", median_CI_high, "]"),
        dv = case_when(
          dv == "cueing_fixed_hddm_a" ~ "Cued Attention (a)",
          dv == "change_hddm_v" ~ "Change (v)",
          dv == "change_hddm_t" ~ "Change (t0)",
          dv == "change_hddm_a" ~ "Change (a)",
          dv == "rt_change"     ~ "Change (rt)",
          dv == "flanker_ssp_p" ~ "Flanker (p)",
          dv == "flanker_ssp_t0" ~ "Flanker (t0)",
          dv == "flanker_ssp_interference" ~ "Flanker (interference)",
          dv == "flanker_ssp_a" ~ "Flanker (a)"
        )
      ) |> 
      select(-median_CI_low, -median_CI_high)
  ) |> 
  ungroup() |> 
  mutate(n = c(3,2,1,9,8,7,6,4,13,11,10,12,5)) |> 
  arrange(n) |> 
  select(-n) |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate(across(
    -dv,
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) 

pilot_main_ddm_keys <- pilot_main_eff_ddm_df |> group_keys(dv)

pilot_main_eff_ddm_list <- pilot_main_eff_ddm_df |> 
  group_split(dv) |> 
  set_names(pilot_main_ddm_keys$dv|> str_replace_all(" \\(", "_") |> str_remove("\\)") |> str_replace_all("Cued Attention", "cueing"))


pilot_main_eff_ddm_table <- 
  pilot_main_eff_ddm_df |> 
  mutate(
    dv = case_when(
      dv == "Cued Attention (rt)" ~ "Raw response time",
      dv == "Cued Attention (v)" ~ "Drift rate",
      dv == "Cued Attention (t0)" ~ "Non-decision time",
      dv == "Cued Attention (a)" ~ "Boundary separation",
      dv == "Change (rt)" ~ "Raw response time",
      dv == "Change (v)" ~ "Drift rate",
      dv == "Change (t0)" ~ "Non-decision time",
      dv == "Change (a)" ~ "Boundary separation",
      dv == "Flanker (rt)" ~ "Raw response time",
      dv == "Flanker (p)" ~ "Perceptual input",
      dv == "Flanker (t0)" ~ "Non-decision time",
      dv == "Flanker (interference)" ~ "Interference",
      dv == "Flanker (a)" ~ "Boundary separation",
    )
  ) |> 
  mutate(across(
    -dv,
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  add_row(.before = 1, dv = "Cued Attention Task") |> 
  add_row(.after = 5, dv = "Change Detection Task") |> 
  add_row(.after = 10, dv = "Flanker Task") |> 
  add_column(empty1 = " ", .after = 4) |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  flextable() |> 
  flextable::set_header_labels(
    dv = "", 
    empty1 = ""
  ) |> 
  add_header_row(
    values = c(" ", "Main Effect", "", "Interaction"),
    colwidths = c(1, 3, 1, 3)
  ) |> 
  compose(i = 2, j = c(2,6), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 2, j = c(4,8), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  compose(i = 2, j = c(3,7), as_paragraph("95% CI"), part = "header") %>% 
  align(i = 1:2, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, j = c(2:4,6:8), border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 2, border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 16, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1:2, part = "header") %>% 
  bold(i = c(1,6,11)) |> 
  set_table_properties(width = 1, layout = "autofit") %>% 
  add_footer_row(
    values = " ",
    colwidths = 8
  ) %>% 
  add_footer_row(
    values = " ",
    colwidths = 8
  ) %>% 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse."), 
    part = "footer"
  )




