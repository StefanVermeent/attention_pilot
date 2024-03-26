library(tidyverse)
library(here)
library(ggsci)
library(patchwork)
library(flextable)
library(specr)


cleaned_data = read_csv(here("data/3_study2/2_cleaned_data.csv"))
study2_data <- cleaned_data
load(file = here("preregistrations/3_study2/analysis_objects/primary_multiverse_summaries.RData"))
load(file = here('preregistrations/3_study2/analysis_objects/study2_bootstrap_pvalues.RData'))

get_alphas <- function(data, string) {
  (data |> 
     select(matches(string)) |> 
     psych::alpha(check.keys = TRUE))$total[[1]] |> round(2)
}




# Descriptives ------------------------------------------------------------

## IV distributions ----
txt_ivs_dist_study2 <- 
  cleaned_data |> select(matches("(mean|total|composite|edu_parents_recoded)$")) |> 
  map(function(x) {
    list(
      mean = round(mean(x, na.rm = T),2),
      sd   = round(sd(x, na.rm = T),2)
    )
  })

txt_ivs_alpha_study2 <- 
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


## IV correlations ----




# Primary Aims  -----------------------------------------------------------

## General ggplot theme for plots
theme_set(
  theme_bw() +
    theme(
      axis.line.y       = element_line(linewidth = 1),
      axis.line.x       = element_line(linewidth = 1),
      axis.text.y       = element_text(size = 14),
      axis.text.x       = element_text(size = 14),
      axis.title.y      = element_text(size = rel(1), margin = margin(1,0,0,0,"lines")),
      axis.ticks.y      = element_blank(),
      axis.ticks.x      = element_blank(),
      
      panel.border      = element_blank(), 
      panel.spacing.y   = unit(0.5, "lines"),
      plot.margin       = margin(.25,.25,.25,.25,"lines"),
      plot.background   = element_rect(color = NA),
      plot.title        = element_text(size = 14, hjust = 0, margin = margin(0,0,.5,0, "lines")),
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

## 1.1 Primary Aim 1: Pooled data comparison ----

study2_prim_aim1_effect_plot <- unique(study2_prim_aim1_effects_sum$dv_iv) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
    coefficient_var <- paste0('coefficient_', iv) |> sym()
    med_effect_var <- paste0('med_effect_', iv) |> sym()
    pvalue_var_chr <- paste0('p_', iv, "_chr") |> sym()
    
    effect_data <- study2_prim_aim1_effects_sum |> 
      filter(dv_iv == x) |> 
      select(decision, dv_iv, parameter, coefficient, p) |> 
      mutate(parameter = ifelse(parameter == "(Intercept)", "intercept", parameter)) |> 
      pivot_wider(names_from = parameter, values_from = c(coefficient, p)) |> 
      mutate(
        across(
          starts_with('p'), ~ifelse(. < .05, "sig", "non-sig"), .names = "{.col}_chr"
        )
      )
    
    iv_min <- cleaned_data |> pull(iv) |> scale() |>  min()
    iv_max <- cleaned_data |> pull(iv) |> scale() |>  max()
    y_min <- min(effect_data$coefficient_intercept) + iv_max*min(effect_data[paste0("coefficient_", iv)]) -0.05
    y_max <- max(effect_data$coefficient_intercept) + iv_max*max(effect_data[paste0("coefficient_", iv)]) +0.05
    int_med <- median(effect_data$coefficient_intercept)
    
    ggplot() +
      geom_segment(data = effect_data, aes(x = iv_min, xend = iv_max, y = int_med + !!coefficient_var*iv_min, yend = int_med + !!coefficient_var*iv_max, color = !!pvalue_var_chr)) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = ifelse(iv == "vio_comp", "Violence Exposure", "Unpredictability"),
        y = "Predicted\n"
      ) +
      geom_segment(data = study2_prim_aim1_medians_sum[[x]], aes(x = iv_min, xend = iv_max, y = int_med + !!med_effect_var*iv_min, yend = int_med + !!med_effect_var*iv_max), linewidth = 1.5) + 
      geom_point(data = study2_prim_aim1_medians_sum[[x]], aes(x = iv_min, y = int_med + iv_min*!!med_effect_var), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = study2_prim_aim1_medians_sum[[x]], aes(x = iv_max, y = int_med + iv_max*!!med_effect_var), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum$dv_iv))


study2_prim_aim1_eff_curve_plot <- unique(study2_prim_aim1_effects_sum$dv_iv) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
    med_effect_std_var <- paste0('med_effect_std_', iv) |> sym()
    midpoint <- study2_prim_aim1_effects_sum |>
      filter(dv_iv == x, parameter == iv) |>
      pull(decision) |> 
      unique() |> 
      as.numeric() |> 
      (\(x) length(x)/2 |> round())()
    
    
    study2_prim_aim1_effects_sum |> 
      filter(dv_iv == x, parameter == iv) |>
      mutate(p.value_chr = ifelse(p < .05, "sig", "non-sig"),
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
        data = study2_prim_aim1_medians_sum[[x]],
        aes(y = !!med_effect_std_var, x = midpoint),
        shape = 1,
        size  = 3,
        fill  = "white",
        stroke = 3,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = midpoint, y = 0.28, label = paste0(round(sum(p < .05)/n*100,1), " % of p-values < .05")),
        size = 5,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_text(
        data = study2_prim_aim1_medians_sum[[x]],
        aes(y = !!med_effect_std_var, label = paste0("\u03b2\ = ", as.character(round(!!med_effect_std_var,2))), x = midpoint),
        nudge_y = -.09,
        size = 5,
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
  setNames(unique(study2_prim_aim1_effects_sum$dv_iv))



study2_prim_aim1_eff_curve_plot_study2 <- unique(study2_prim_aim1_effects_sum_study2$dv_iv) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
    med_effect_std_var <- paste0('med_effect_std_', iv) |> sym()
    midpoint <- study2_prim_aim1_effects_sum |>
      filter(dv_iv == x, parameter == iv) |>
      pull(decision) |> 
      unique() |> 
      as.numeric() |> 
      (\(x) length(x)/2 |> round())()
    
    
    study2_prim_aim1_effects_sum_study2 |> 
      filter(dv_iv == x, parameter == iv) |>
      mutate(p.value_chr = ifelse(p < .05, "sig", "non-sig"),
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
      
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), limits = c(-0.35, 0.35)) +
      geom_hline(aes(yintercept = 0), size = .5,linetype = "solid") +
      geom_point(size = 3, shape = 19, show.legend = F) +
      geom_point(
        data = study2_prim_aim1_medians_sum_study2[[x]],
        aes(y = !!med_effect_std_var, x = midpoint),
        shape = 1,
        size  = 3,
        fill  = "white",
        stroke = 3,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = midpoint, y = 0.28, label = paste0(round(sum(p < .05)/n*100,1), " % of p-values < .05")),
        size = 5,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_text(
        data = study2_prim_aim1_medians_sum_study2[[x]],
        aes(y = !!med_effect_std_var, label = paste0("\u03b2\ = ", as.character(round(!!med_effect_std_var,2))), x = midpoint),
        nudge_y = -.065,
        size = 5,
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
  setNames(unique(study2_prim_aim1_effects_sum_study2$dv_iv))

# Plot lm p-value curve
study2_prim_aim1_pvalues_plot <- unique(study2_prim_aim1_effects_sum$dv_iv) |>
  map(function(x) {
    
    pvalues <- study2_prim_aim1_effects_sum |>
      filter(dv_iv == x, parameter != "(Intercept)") |> 
      ungroup() |> 
      select(decision, p)  
    
    
    pvalues |> 
      ggplot(aes(p)) +
      geom_histogram(color = "black", size = .2, bins = 100) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      annotate(
        geom = 'text', 
        label = pvalues %>% summarize(p = paste0(as.character(round(sum(p < .05)/n()*100, 2)), " %")), 
        x = Inf, 
        y = Inf, 
        hjust = 1.1, 
        vjust = 1.1) +
      theme_classic() +
      scale_x_continuous(expression(italic(p),"-",value), breaks = seq(0, 1, 0.1)) +
      xlim(c(-0.1,1)) +
      scale_y_continuous("", expand = c(0,.15)) +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = 12),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum$dv_iv))

study2_prim_aim1_pvalues_plot_study2 <- unique(study2_prim_aim1_effects_sum_study2$dv_iv) |>
  map(function(x) {
    
    pvalues <- study2_prim_aim1_effects_sum_study2 |>
      filter(dv_iv == x, parameter != "(Intercept)") |> 
      ungroup() |> 
      select(decision, p)  
    
    
    pvalues |> 
      ggplot(aes(p)) +
      geom_histogram(color = "black", size = .2, bins = 100) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      annotate(
        geom = 'text', 
        label = pvalues %>% summarize(p = paste0(as.character(round(sum(p < .05)/n()*100, 2)), " %")), 
        x = Inf, 
        y = Inf, 
        hjust = 1.1, 
        vjust = 1.1) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      xlim(c(-0.1,1)) +
      scale_y_continuous("", expand = c(0,.15)) +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = 12),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
  }) |> 
  setNames(unique(study2_prim_aim1_effects_sum_study2$dv_iv))


study2_prim_aim1_variance_plot <- names(study2_prim_aim1_variance_sum) |>
  map(function(x) {
    
    study2_prim_aim1_variance_sum[[x]] |> 
      ggplot(aes(grp, percent, fill = grp)) +
      geom_bar(stat = "identity") +
      scale_fill_uchicago() +
      ylim(0, 100) +
      coord_flip() +
      labs(
        x = "",
        y = "Explained variance (%)"
      ) +
      theme_classic() +
      theme(
        axis.text.y = element_blank()
      )
  }) |> 
  setNames(names(study2_prim_aim1_variance_sum))

study2_prim_aim1_variance_plot_study2 <- names(study2_prim_aim1_variance_sum_study2) |>
  map(function(x) {
    
    study2_prim_aim1_variance_sum_study2[[x]] |> 
      ggplot(aes(grp, percent, fill = grp)) +
      geom_bar(stat = "identity") +
      scale_fill_uchicago() +
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "Explained variance in estimate (%)"
      ) +
      theme_classic() +
      theme(
        axis.text.y = element_blank()
      )
  }) |> 
  setNames(names(study2_prim_aim1_variance_sum_study2))

## 1.2 Primary Aim 2: Global-Local Task ----

study2_prim_aim2_effect_plot <- names(study2_prim_aim2_points_sum) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
    iv_print <- ifelse(iv == 'unp_comp', "Unpredictability", "Violence Exposure")
    
    study2_prim_aim2_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = level,
        condition = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        condition = case_when(
          condition == -1 ~ "Local",
          condition == 1 ~ "Global",
        ),
        condition = factor(condition, levels = c("Local", "Global"))
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
      scale_fill_manual(iv_print, labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = ""
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(study2_prim_aim2_points_sum))


study2_prim_aim2_eff_curve_plot <- unique(study2_prim_aim2_effects_sum$dv_iv) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
   
    midpoint <- study2_prim_aim2_effects_sum |>
      filter(dv_iv == x, parameter == iv) |>
      pull(decision) |> 
      unique() |> 
      as.numeric() |> 
      (\(x) length(x)/2 |> round())()
    
    study2_prim_aim2_effects_sum |> 
      filter(dv_iv == x, str_detect(parameter, ":")) |>
      mutate(p.value_chr = ifelse(p < .05, "sig", "non-sig"),
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
        data = study2_prim_aim2_medians_sum[[x]],
        aes(y = med_effect_std, x = midpoint),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = midpoint, y = 0.18, label = paste0(round(sum(p < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = study2_prim_aim2_medians_sum[[x]],
        aes(y = med_effect_std, label = as.character(round(med_effect_std,2)), x = midpoint),
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
  setNames(unique(study2_prim_aim2_effects_sum$dv_iv))



study2_prim_aim2_pvalues_plot <- names(study2_prim_aim2_points_sum) |>
  map(function(x) {
    
    pvalues <- study2_prim_aim2_points_sum[[x]] |> 
      select(decision, p) |> 
      distinct()
    
    pvalues |> 
      ggplot(aes(p)) +
      geom_histogram(color = "black", size = .2, bins = 100) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      annotate(
        geom = 'text', 
        label = pvalues %>% summarize(p = paste0(as.character(round(sum(p < .05)/n()*100, 2)), " %")), 
        x = Inf, 
        y = Inf, 
        hjust = 1.1, 
        vjust = 1.1) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      xlim(c(-0.1,1)) +
      scale_y_continuous("", expand = c(0,.15)) +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = 12),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
  }) |> 
  setNames(names(study2_prim_aim2_points_sum))

study2_prim_aim2_variance_plot <- names(study2_prim_aim2_variance_sum) |>
  map(function(x) {
    
    study2_prim_aim2_variance_sum[[x]] |> 
      ggplot(aes(grp, percent, fill = grp)) +
      geom_bar(stat = "identity") +
      scale_fill_uchicago() +
      ylim(0, 100) +
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = ""
      ) +
      theme_classic() +
      theme(
        axis.text.y = element_blank()
      )
  }) |> 
  setNames(names(study2_prim_aim2_variance_sum))



## 1.3 Primary Aim 3: Within Subjects Global Local and Flanker ----

study2_prim_aim3_effect_plot <- names(study2_prim_aim3_points_sum) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
    iv_print <- ifelse(iv == 'unp_comp', "Unpredictability", "Violence Exposure")
    
    study2_prim_aim3_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = adversity,
        task = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        task = case_when(
          task == -1 ~ "Flanker\n(Perceptual input)",
          task == 1 ~ "Global-Local\ndifference",
        ),
        task = factor(task, levels = c("Flanker\n(Perceptual input)", "Global-Local\ndifference"))
      ) |> 
      ggplot(aes(task, predicted, color = p.value_chr, group = decision)) +
      geom_point(size = .5, show.legend = F) +
      geom_line(size = .5, show.legend = F) +
      scale_color_manual(values = pval_colors) +
      stat_summary(
        aes(x = task, y = predicted, group = iv), 
        geom = "line", 
        fun = "median", 
        color = "black", 
        alpha = 1,
        size = 1,
        show.legend = F
      ) +
      stat_summary(
        aes(x = task, y = predicted, group = iv, fill = iv),
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
      scale_fill_manual(iv_print, labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "",
        y = ""
      ) +
      theme(
        legend.position=c(0.8,0.7),
        axis.text.x = element_text(angle = 20, hjust = 1))
  }) |> 
  setNames(names(study2_prim_aim3_points_sum))


study2_prim_aim3_eff_curve_plot <- unique(study2_prim_aim3_effects_sum$dv_iv) |> 
  map(function(x) {
    
    iv <- str_remove(x, "^.*\\.")
    
    midpoint <- study2_prim_aim3_effects_sum |>
      filter(dv_iv == x, parameter == iv) |>
      pull(decision) |> 
      unique() |> 
      as.numeric() |> 
      (\(x) length(x)/2 |> round())()
    
    study2_prim_aim3_effects_sum |> 
      filter(dv_iv == x, str_detect(parameter, ":")) |>
      mutate(p.value_chr = ifelse(p < .05, "sig", "non-sig"),
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
      geom_point(size = 3, shape = 19, show.legend = F) +
      geom_point(
        data = study2_prim_aim3_medians_sum[[x]],
        aes(y = med_effect_std, x = midpoint),
        shape = 1,
        size  = 3,
        fill  = "white",
        stroke = 3,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = midpoint, y = 0.25, label = paste0(round(sum(p < .05)/n*100,1), " % of p-values < .05")),
        size = 5,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_text(
        data = study2_prim_aim3_medians_sum[[x]],
        aes(y = med_effect_std, label = paste0("\u03b2\ = ", as.character(round(med_effect_std,2))), x = midpoint),
        nudge_y = -0.025,
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
  setNames(unique(study2_prim_aim3_effects_sum$dv_iv))



study2_prim_aim3_pvalues_plot <- names(study2_prim_aim3_points_sum) |>
  map(function(x) {
    
    pvalues <- study2_prim_aim3_points_sum[[x]] |> 
      select(decision, p) |> 
      distinct()
    
    pvalues |> 
      ggplot(aes(p)) +
      geom_histogram(color = "black", size = .2, bins = 100) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      annotate(
        geom = 'text', 
        label = pvalues %>% summarize(p = paste0(as.character(round(sum(p < .05)/n()*100, 2)), " %")), 
        x = Inf, 
        y = Inf, 
        hjust = 1.1, 
        vjust = 1.1) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      xlim(c(-0.1,1)) +
      scale_y_continuous("", expand = c(0,.15)) +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = 12),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
  }) |> 
  setNames(names(study2_prim_aim3_points_sum))

study2_prim_aim3_variance_plot <- names(study2_prim_aim3_variance_sum) |>
  map(function(x) {
    
    study2_prim_aim3_variance_sum[[x]] |> 
      ggplot(aes(grp, percent, fill = grp)) +
      geom_bar(stat = "identity") +
      scale_fill_uchicago() +
      ylim(0, 100) +
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = ""
      ) +
      theme_classic() +
      theme(
        axis.text.y = element_blank()
      )
  }) |> 
  setNames(names(study2_prim_aim3_variance_sum))


# Multiverse Figures ------------------------------------------------------


## Primary Aim 2: Global-Local Task ----

right_hand_themes <- theme(
  axis.text.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank()
)

study2_aim2_plot <- 
  (study2_prim_aim2_effect_plot$v_globloc.vio_comp + ylim(1,3) + ggtitle("Perceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     study2_prim_aim2_effect_plot$v_globloc.unp_comp  + ylim(1,3) + ggtitle("Perceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 2)) /
  # Row 2: Effect curves
  ((study2_prim_aim2_eff_curve_plot$v_globloc.vio_comp + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study2_prim_aim2_eff_curve_plot$v_globloc.unp_comp + right_hand_themes) + plot_layout(ncol = 2)) +
  # Row 3: Explained variances
  ((study2_prim_aim2_variance_plot$v_globloc.vio_comp + ylim(0,100) + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study2_prim_aim2_variance_plot$v_globloc.unp_comp + ylim(0,100) + right_hand_themes) + plot_layout(ncol = 2))


## Primary Aim 3: Within Subjects Global Local and Flanker ----

study2_aim3_plot <- 
  (study2_prim_aim3_effect_plot$value.vio_comp + ylim(-1,1) + labs(tag = "A") + ggtitle("Violence Exposure") + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
     study2_prim_aim3_effect_plot$value.unp_comp  + ylim(-1,1) + ggtitle("Unpredictability") + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + plot_layout(ncol = 2)) /
  # Row 2: Effect curves
  ((study2_prim_aim3_eff_curve_plot$value.vio_comp + ylim(-0.1, 0.3) + labs(tag = "B") + theme(plot.title = element_text(hjust = 1, size = 14, face = "bold"))) +
     (study2_prim_aim3_eff_curve_plot$value.unp_comp + ylim(-0.1, 0.3) + right_hand_themes) + plot_layout(ncol = 2))

ggsave(plot = study2_aim3_plot, filename = "manuscript/figures/fig4.png", height=10, width = 9)


# Multiverse Tables -------------------------------------------------------

## Primary Aim 1: Study 2 only ----

study2_aim1_results_study2_list <- study2_prim_aim1_effects_sum_study2 |> 
  filter(!parameter %in% c("(Intercept)")) |> 
  select(decision, dv, iv, dv_iv, parameter, Std_Coefficient, p, Std_CI_low, Std_CI_high) |> 
  group_by(dv_iv) |> 
  summarise(
    median_effect_Main = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p < .05)/n()*100) |> 
  ungroup() |> 
  left_join(
    bind_rows(study2_aim1_vio_boot$result_sum, study2_aim1_unp_boot$result_sum) |> unite(col = "dv_iv", c(dv,iv), sep = ".")
  ) |> 
  mutate(across(
    -c(dv_iv, boot_p),
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  mutate(boot_p = formatC(boot_p, digits = 3, width = 3, flag = "0", format = "f") %>% str_remove("^0") %>% str_replace_all("^.000$", "< .001")) |> 
  mutate(CI = paste0("[", median_CI_low, ", ", median_CI_high, "]")) |> 
  group_split(dv_iv) |> 
  setNames(study2_prim_aim1_effects_sum_study2 |> group_keys(dv_iv) |> pull(dv_iv))



## Primary Aim 1: Pooled data comparison ----

study2_aim1_results_list <- study2_prim_aim1_effects_sum |> 
  filter(!parameter %in% c("(Intercept)", "study.study1-intercept")) |> 
  select(decision, dv, iv, dv_iv, parameter, Std_Coefficient, p, Std_CI_low, Std_CI_high) |> 
  group_by(dv_iv) |> 
  summarise(
    median_effect_Main = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p < .05)/n()*100) |> 
  ungroup() |> 
  left_join(
    bind_rows(study2_aim1_vio_boot$result_sum, study2_aim1_unp_boot$result_sum) |> unite(col = "dv_iv", c(dv,iv), sep = ".")
  ) |> 
  mutate(across(
    -c(dv_iv, boot_p),
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  mutate(boot_p = formatC(boot_p, digits = 3, width = 3, flag = "0", format = "f") %>% str_remove("^0") %>% str_replace_all("^.000$", "< .001")) |> 
  mutate(CI = paste0("[", median_CI_low, ", ", median_CI_high, "]")) |> 
  group_split(dv_iv) |> 
  setNames(study2_prim_aim1_effects_sum |> group_keys(dv_iv) |> pull(dv_iv))

## Primary Aim 2: Global-Local Task ----

study2_aim2_results_df <- study2_prim_aim2_effects_sum |> 
  filter(parameter != "(Intercept)") |> 
  group_by(dv_iv, parameter) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p < .05)/n()*100) |> 
  ungroup() |> 
  left_join(
    bind_rows(
      study2_aim2_vio_boot_main1$result_sum |> mutate(parameter = "vio_comp"),
      study2_aim2_vio_boot_main2$result_sum |> mutate(parameter = "condition"),
      study2_aim2_vio_boot_int$result_sum |> mutate(parameter = "vio_comp:condition"),
      study2_aim2_unp_boot_main1$result_sum |> mutate(parameter = "unp_comp"),
      study2_aim2_unp_boot_int$result_sum |> mutate(parameter = "unp_comp:condition")
   ) |> unite(col = "dv_iv", c(dv,iv), sep = ".")
  ) |> 
  mutate(
    dv_iv = str_replace_all(dv_iv, "v_globloc.", ""),
    parameter = case_when(
      str_detect(parameter, ":") & dv_iv == "unp_comp" ~ "Interaction_unp",
      parameter == "condition" & dv_iv == "unp_comp" ~ "Condition_unp",
      str_detect(parameter, "comp") & dv_iv == "unp_comp"~ "Main_unp",
      
      str_detect(parameter, ":") & dv_iv == "vio_comp" ~ "Interaction_vio",
      parameter == "condition" & dv_iv == "vio_comp" ~ "Condition_vio",
      str_detect(parameter, "comp") & dv_iv == "vio_comp"~ "Main_vio"
    )) |> 
  mutate(across(
    -c(parameter, dv_iv, boot_p),
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  mutate(boot_p = formatC(boot_p, digits = 3, width = 3, flag = "0", format = "f") %>% str_remove("^0") %>% str_replace_all("^.000$", "< .001")) |> 
  mutate(CI = paste0("[", median_CI_low, ", ", median_CI_high, "]")) |> 
  select(parameter, matches("median_effect"), ends_with("CI"), matches("p_sum"), boot_p) |>
  mutate(n = c(2,1,3,5,4,6)) |> 
  arrange(n) |> 
  select(-n)  

study2_aim2_results_keys <- study2_aim2_results_df |> 
  group_keys(parameter) |> 
  pull()

study2_aim2_results_list <- study2_aim2_results_df |> 
  group_split(parameter) |> 
  setNames(study2_aim2_results_keys)

study2_aim2_results_table <- study2_aim2_results_df |> 
  add_row(.before = 1, parameter = "Unpredictability") |> 
  add_row(.after = 4, parameter = "Violence Exposure") |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate( 
    parameter = str_replace_all(parameter, "_unp|_vio", " effect")) |> 
  flextable() |> 
  width(j = 1, width = .75) %>% 
  set_header_labels(
    parameter = ""
  ) |> 
  set_header_labels(
    parameter = " ", 
    median_effect = "Median Effect",
    CI = "95% CI",
    p_sum = "% p"
) |> 
  compose(i = 1, j = c(2), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 1, j = c(4), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  compose(i = 1, j = c(3), as_paragraph("95% CI"), part = "header") %>% 
  compose(i = 1, j = c(5), as_paragraph(as_i("p")), part = "header") %>%
  align(i = 1, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, border.top = fp_border_default(), part = "body") %>% 
  border(i = 8, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1, part = "header") %>% 
  bold(i = c(1,5)) |> 
  set_table_properties(width = 1, layout = "autofit") %>% 
  add_footer_row(
    values = " ",
    colwidths = 5
  ) %>% 
  add_footer_row(
    values = " ",
    colwidths = 5
  ) %>% 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "Task conditions were sum-coded (local = -1, global = 1). The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse. We computed overall p-values using a bootstrapped resampling method, which reflect the probability of obtaining an effect size as extreme or more extreme given
the median effect is 0."), 
    part = "footer"
  )

study2_aim2_simpslopes1_df <- 
  study2_prim_aim2_simslopes1 |> 
  select(dv_iv, condition, Slope, p.value, condition) |> 
  group_by(dv_iv, condition) |> 
  summarise(
    median_ss = median(Slope),
    p_sum     = sum(p.value<.05)/n()*100
  ) |> 
  ungroup() |> 
  mutate(
    dv_iv = str_replace_all(dv_iv, "v_globloc.", ""),
    condition = ifelse(condition == -1, "Local", "Global")
  ) |> 
  unite("parameter", c(dv_iv, condition), sep = ".") |> 
  mutate(
    across(
      -c(parameter),
      ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
    )
  ) 

study2_aim2_simslopes1_keys <- study2_aim2_simpslopes1_df  |> 
  group_keys(parameter)|> 
  pull()

study2_aim2_simslopes1_list <- study2_aim2_simpslopes1_df |> 
  group_split(parameter) |> 
  setNames(study2_aim2_simslopes1_keys)


study2_aim2_simpslopes2_df <- 
  study2_prim_aim2_simslopes2 |> 
  select(decision, dv_iv, adversity, Slope, p.value) |> 
  group_by(dv_iv, adversity) |> 
  summarise(
    median_ss = median(Slope),
    p_sum     = sum(p.value<.05)/n()*100
  ) |> 
  ungroup() |> 
  mutate(
    dv_iv = str_replace_all(dv_iv, "v_globloc.", ""),
    adversity = ifelse(adversity == -1, "Low", "High")
  ) |> 
  unite("parameter", c(dv_iv, adversity), sep = ".") |> 
  mutate(
    across(
      -c(parameter),
      ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
    )
  ) 

study2_aim2_simslopes2_keys <- study2_aim2_simpslopes2_df  |> 
  group_keys(parameter)|> 
  pull()

study2_aim2_simslopes2_list <- study2_aim2_simpslopes2_df |> 
  group_split(parameter) |> 
  setNames(study2_aim2_simslopes2_keys)


## Primary Aim 3: Within Subjects Global Local & Flanker ----

study2_aim3_results_df <- study2_prim_aim3_effects_sum |>
  filter(parameter != "(Intercept)") |> 
  group_by(dv_iv, parameter) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p < .05)/n()*100) |> 
  ungroup() |> 
  left_join(
    bind_rows(
      study2_aim3_vio_boot_main1$result_sum |> mutate(parameter = "vio_comp"),
      study2_aim3_vio_boot_main2$result_sum |> mutate(parameter = "task"),
      study2_aim3_vio_boot_int$result_sum |> mutate(parameter = "vio_comp:task"),
      study2_aim3_unp_boot_main1$result_sum |> mutate(parameter = "unp_comp"),
      study2_aim3_unp_boot_int$result_sum |> mutate(parameter = "unp_comp:task")
    ) |> unite(col = "dv_iv", c(dv,iv), sep = ".")
  ) |> 
  mutate(
    dv_iv = str_replace_all(dv_iv, "value.", ""),
    parameter = case_when(
      str_detect(parameter, ":") & dv_iv == "unp_comp" ~ "Interaction_unp",
      parameter == "task" & dv_iv == "unp_comp" ~ "Task_unp",
      str_detect(parameter, "comp") & dv_iv == "unp_comp"~ "Main_unp",
      
      str_detect(parameter, ":") & dv_iv == "vio_comp" ~ "Interaction_vio",
      parameter == "task" & dv_iv == "vio_comp" ~ "Task_vio",
      str_detect(parameter, "comp") & dv_iv == "vio_comp"~ "Main_vio"
    )) |> 
  mutate(across(
    -c(parameter, dv_iv, boot_p),
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |>
  mutate(boot_p = formatC(boot_p, digits = 3, width = 3, flag = "0", format = "f") %>% str_remove("^0") %>% str_replace_all("^.000$", "< .001")) |> 
  mutate(CI = paste0("[", median_CI_low, ", ", median_CI_high, "]")) |> 
  select(parameter, matches("median_effect"), ends_with("CI"), matches("p_sum"), boot_p) |>
  mutate(n = c(2,1,3,5,4,6)) |> 
  arrange(n) |> 
  select(-n)  

study2_aim3_results_keys <- study2_aim3_results_df |> 
  group_keys(parameter) |> 
  pull()

study2_aim3_results_list <- study2_aim3_results_df |> 
  group_split(parameter) |> 
  setNames(study2_aim3_results_keys)

study2_aim3_results_table <- study2_aim3_results_df |> 
  add_row(.before = 1, parameter = "Unpredictability") |> 
  add_row(.after = 4, parameter = "Violence Exposure") |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate( 
    parameter = str_replace_all(parameter, "_unp|_vio", " effect")) |> 
  flextable() |> 
  width(j = 1, width = .75) %>% 
  set_header_labels(
    parameter = ""
  ) |> 
  set_header_labels(
    parameter = " ", 
    median_effect = "Median Effect",
    CI = "95% CI",
    p_sum = "% p"
  ) |> 
  compose(i = 1, j = c(2), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 1, j = c(4), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  compose(i = 1, j = c(3), as_paragraph("95% CI"), part = "header") %>% 
  compose(i = 1, j = c(5), as_paragraph(as_i("p")), part = "header") %>%
  align(i = 1, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, border.top = fp_border_default(), part = "body") %>% 
  border(i = 8, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1, part = "header") %>% 
  bold(i = c(1,5)) |> 
  set_table_properties(width = 1, layout = "autofit") %>% 
  add_footer_row(
    values = " ",
    colwidths = 5
  ) %>% 
  add_footer_row(
    values = " ",
    colwidths = 5
  ) %>% 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "Task conditions were sum-coded (Flanker = -1, global-Local preference = 1). The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse. We computed overall p-values using a bootstrapped resampling method, which reflect the probability of obtaining an effect size as extreme or more extreme given the median effect is 0."), 
    part = "footer"
  )

study2_aim3_simpslopes1_df <- 
  study2_prim_aim3_simslopes1 |> 
  select(dv_iv, Slope, p.value, task) |> 
  group_by(dv_iv, task) |> 
  summarise(
    median_ss = median(Slope),
    p_sum     = sum(p.value<.05)/n()*100
  ) |> 
  ungroup() |> 
  mutate(
    dv_iv = str_replace_all(dv_iv, "value.", ""),
    task = ifelse(task == -1, "Flanker", "GlobalLocal")
  ) |> 
  unite("parameter", c(dv_iv, task), sep = ".") |> 
  mutate(
    across(
      -c(parameter),
      ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
    )
  ) 

study2_aim3_simslopes1_keys <- study2_aim3_simpslopes1_df  |> 
  group_keys(parameter)|> 
  pull()

study2_aim3_simslopes1_list <- study2_aim3_simpslopes1_df |> 
  group_split(parameter) |> 
  setNames(study2_aim3_simslopes1_keys)


save(txt_ivs_dist_study2, txt_ivs_alpha_study2, sample_pilot, study2_prim_aim1_effect_plot, study2_prim_aim1_eff_curve_plot, study2_prim_aim1_eff_curve_plot_study2, study2_prim_aim1_pvalues_plot, study2_prim_aim1_pvalues_plot_study2, 
     study2_prim_aim1_variance_plot, study2_prim_aim1_variance_plot_study2, study2_prim_aim2_effect_plot, study2_prim_aim2_eff_curve_plot, study2_prim_aim2_pvalues_plot, study2_prim_aim2_variance_plot, 
     study2_prim_aim3_effect_plot, study2_prim_aim3_eff_curve_plot, study2_prim_aim3_pvalues_plot, study2_prim_aim3_variance_plot, study2_aim2_plot, study2_aim3_plot, study2_aim1_results_study2_list, 
     study2_aim1_results_list, study2_aim2_results_df, study2_aim2_results_list, study2_aim2_results_table, study2_aim2_simpslopes1_df, study2_aim2_simslopes1_list, study2_aim2_simpslopes2_df, 
     study2_aim2_simslopes2_list, study2_aim3_results_df, study2_aim3_results_list, study2_aim3_results_table, study2_aim3_simpslopes1_df, study2_aim3_simslopes1_list,
     file = "manuscript/study2_staged_results.RData")
