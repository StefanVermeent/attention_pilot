library(tidyverse)
library(here)
library(ggsci)
library(patchwork)
library(flextable)
library(specr)


cleaned_data = read_csv(here("data/2_study1/2_cleaned_data.csv"))
study1_data <- cleaned_data
load(file = here("preregistrations/2_study1/analysis_objects/primary_multiverse_summaries.RData"))
load(file = here("preregistrations/2_study1/analysis_objects/exploratory_multiverse_summaries.RData"))

get_alphas <- function(data, string) {
  (data |> 
     select(matches(string)) |> 
     psych::alpha(check.keys = TRUE))$total[[1]] |> round(2)
}




# Descriptives ------------------------------------------------------------

## IV distributions ----
txt_ivs_dist_study1 <- 
  cleaned_data |> select(matches("(mean|total|composite|edu_parents_recoded)$")) |> 
  map(function(x) {
    list(
      mean = round(mean(x, na.rm = T),2),
      sd   = round(sd(x, na.rm = T),2)
    )
  })

txt_ivs_alpha_study1 <- 
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




# Main results  -----------------------------------------------------------



## General ggplot theme for plots
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
      plot.title        = element_text(size = rel(.85), hjust = 0, margin = margin(0,0,.5,0, "lines")),
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

## 1.1 Pooled data comparison ----

study1_prim_ssp_pooled_effect_plot <- unique(prim_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    effect_data <- prim_ssp_pooled_effects_sum |> 
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
      geom_segment(data = effect_data, aes(x = vio_min, xend = vio_max, y = int_med+estimate_vio_comp*vio_min, yend = int_med+estimate_vio_comp*vio_max, color = p.value_chr), alpha = 0.2) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = "Violence Exposure",
        y = ""
      ) +
      geom_segment(data = prim_ssp_pooled_medians_sum[[x]], aes(x = vio_min, xend = vio_max, y = int_med+med_effect_vio_comp*vio_min, yend = int_med+med_effect_vio_comp*vio_max), size = 1.5) + 
      geom_point(data = prim_ssp_pooled_medians_sum[[x]], aes(x = vio_min, y = int_med+vio_min*med_effect_vio_comp), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = prim_ssp_pooled_medians_sum[[x]], aes(x = vio_max, y = int_med+vio_max*med_effect_vio_comp), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(prim_ssp_pooled_effects_sum$dv))

study1_prim_ssp_pooled_eff_curve_plot <- unique(prim_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    prim_ssp_pooled_effects_sum |> 
      filter(dv == x, term != "(Intercept)") |>
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
        data = prim_ssp_pooled_medians_sum[[x]],
        aes(y = med_effect_std, x = 64),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 64, y = 0.18, label = paste0(round(sum(p.value < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = prim_ssp_pooled_medians_sum[[x]],
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
  setNames(unique(prim_ssp_pooled_effects_sum$dv))

# Plot lm p-value curve
study1_prim_ssp_pooled_pvalues_plot <- unique(prim_ssp_pooled_effects_sum$dv) |>
  map(function(x) {
    
    pvalues <- prim_ssp_pooled_effects_sum |>
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
  setNames(unique(prim_ssp_pooled_effects_sum$dv))

study1_prim_ssp_pooled_variance_plot <- names(prim_ssp_pooled_variance_sum) |>
  map(function(x) {
    
    prim_ssp_pooled_variance_sum[[x]] |> 
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
  setNames(names(prim_ssp_pooled_variance_sum))

## 1.2 Standard - Enhanced comparison ----

study1_prim_ssp_enh_effect_plot <- names(prim_ssp_enh_points_sum) |> 
  map(function(x) {
    
    prim_ssp_enh_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = level,
        condition = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        condition = case_when(
          condition == 0 ~ "Standard",
          condition == 1 ~ "Enhanced",
        ),
        condition = factor(condition, levels = c("Standard", "Enhanced"))
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
      scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = ""
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(prim_ssp_enh_points_sum))


study1_prim_ssp_enh_eff_curve_plot <- unique(prim_ssp_enh_effects_sum$dv) |> 
  map(function(x) {
    
    prim_ssp_enh_effects_sum |> 
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
        data = prim_ssp_enh_medians_sum[[x]],
        aes(y = med_effect_std, x = 32),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 32, y = 0.18, label = paste0(round(sum(p.value < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = prim_ssp_enh_medians_sum[[x]],
        aes(y = med_effect_std, label = as.character(round(med_effect_std,2)), x = 32),
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
  setNames(unique(prim_ssp_enh_effects_sum$dv))



study1_prim_ssp_enh_pvalues_plot <- names(prim_ssp_enh_points_sum) |>
  map(function(x) {
    
    pvalues <- prim_ssp_enh_points_sum[[x]] |> 
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
  setNames(names(prim_ssp_enh_points_sum))

study1_prim_ssp_enh_variance_plot <- names(prim_ssp_enh_variance_sum) |>
  map(function(x) {
    
    prim_ssp_enh_variance_sum[[x]] |> 
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
  setNames(names(prim_ssp_enh_variance_sum))



## 1.3 Standard - Degraded comparison ----

study1_prim_ssp_deg_effect_plot <- names(prim_ssp_deg_points_sum) |> 
  map(function(x) {
    
    prim_ssp_deg_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = level,
        condition = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        condition = case_when(
          condition == 0 ~ "Standard",
          condition == 1 ~ "Degraded",
        ),
        condition = factor(condition, levels = c("Standard", "Degraded"))
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
      scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = ""
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(prim_ssp_deg_points_sum))

study1_prim_ssp_deg_eff_curve_plot <- unique(prim_ssp_deg_effects_sum$dv) |> 
  map(function(x) {
    
    prim_ssp_deg_effects_sum |> 
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
        data = prim_ssp_deg_medians_sum[[x]],
        aes(y = med_effect_std, x = 32),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 32, y = 0.18, label = paste0(round(sum(p.value < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = prim_ssp_deg_medians_sum[[x]],
        aes(y = med_effect_std, label = as.character(round(med_effect_std,2)), x = 32),
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
  setNames(unique(prim_ssp_deg_effects_sum$dv))


study1_prim_ssp_deg_pvalues_plot <- names(prim_ssp_deg_points_sum) |>
  map(function(x) {
    
    pvalues <- prim_ssp_deg_points_sum[[x]] |> 
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
  setNames(names(prim_ssp_deg_points_sum))

study1_prim_ssp_deg_variance_plot <- names(prim_ssp_deg_variance_sum) |>
  map(function(x) {
    
    prim_ssp_deg_variance_sum[[x]] |> 
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
  setNames(names(prim_ssp_deg_variance_sum))




# Secondary results ---------------------------------------------------------

## Pooled analyses ----

## 1.1 Pooled data comparison ----

study1_expl_ssp_pooled_effect_plot <- unique(expl_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    effect_data <- expl_ssp_pooled_effects_sum |> 
      filter(dv == x) |> 
      select(decision, dv, term, estimate, p.value) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(names_from = term, values_from = c(estimate, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value_unp_comp < .05, "sig", "non-sig")
      )
    
    unp_min <- cleaned_data$unp_comp |> scale() |>  min()
    unp_max <- cleaned_data$unp_comp |> scale() |>  max()
    y_min <- min(effect_data$estimate_intercept) + unp_max*min(effect_data$estimate_unp_comp) -0.05
    y_max <- max(effect_data$estimate_intercept) + unp_max*max(effect_data$estimate_unp_comp) +0.05
    int_med <- median(effect_data$estimate_intercept)
    
    ggplot() +
      # coord_cartesian(xlim = c(unp_min, unp_max), ylim = c(y_min, y_max)) +
      geom_segment(data = effect_data, aes(x = unp_min, xend = unp_max, y = int_med+estimate_unp_comp*unp_min, yend = int_med+estimate_unp_comp*unp_max, color = p.value_chr), alpha = 0.1) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = "Unpredictability",
        y = ""
      ) +
      geom_segment(data = expl_ssp_pooled_medians_sum[[x]], aes(x = unp_min, xend = unp_max, y = int_med+med_effect_unp_comp*unp_min, yend = int_med+med_effect_unp_comp*unp_max), size = 1.5) + 
      geom_point(data = expl_ssp_pooled_medians_sum[[x]], aes(x = unp_min, y = int_med+unp_min*med_effect_unp_comp), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = expl_ssp_pooled_medians_sum[[x]], aes(x = unp_max, y = int_med+unp_max*med_effect_unp_comp), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(expl_ssp_pooled_effects_sum$dv))

study1_expl_ssp_pooled_eff_curve_plot <- unique(expl_ssp_pooled_effects_sum$dv) |> 
  map(function(x) {
    
    expl_ssp_pooled_effects_sum |> 
      filter(dv == x, term != "(Intercept)") |>
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
        data = expl_ssp_pooled_medians_sum[[x]],
        aes(y = med_effect_std, x = 64),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 64, y = 0.18, label = paste0(round(sum(p.value < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = expl_ssp_pooled_medians_sum[[x]],
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
  setNames(unique(expl_ssp_pooled_effects_sum$dv))

# Plot lm p-value curve
study1_expl_ssp_pooled_pvalues_plot <- unique(expl_ssp_pooled_effects_sum$dv) |>
  map(function(x) {
    
    pvalues <- expl_ssp_pooled_effects_sum |>
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
  setNames(unique(expl_ssp_pooled_effects_sum$dv))

study1_expl_ssp_pooled_variance_plot <- names(expl_ssp_pooled_variance_sum) |>
  map(function(x) {
    
    expl_ssp_pooled_variance_sum[[x]] |> 
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
  setNames(names(expl_ssp_pooled_variance_sum))

## Plots: Standard - Enhanced ----

study1_expl_ssp_enh_effect_plot <- names(expl_ssp_enh_points_sum) |> 
  map(function(x) {
    
    expl_ssp_enh_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = level,
        condition = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        condition = case_when(
          condition == 0 ~ "Standard",
          condition == 1 ~ "Enhanced",
        ),
        condition = factor(condition, levels = c("Standard", "Enhanced"))
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
      scale_fill_manual("Unpredictability:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = ""
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(expl_ssp_enh_points_sum))


study1_expl_ssp_enh_eff_curve_plot <- unique(expl_ssp_enh_effects_sum$vars) |> 
  map(function(x) {
    
    expl_ssp_enh_effects_sum |> 
      filter(vars == x, str_detect(term, ":")) |>
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
        data = expl_ssp_enh_medians_sum[[x]],
        aes(y = med_effect_std, x = 32),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 32, y = 0.18, label = paste0(round(sum(p.value < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = expl_ssp_enh_medians_sum[[x]],
        aes(y = med_effect_std, label = as.character(round(med_effect_std,2)), x = 32),
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
  setNames(unique(expl_ssp_enh_effects_sum$vars))



study1_expl_ssp_enh_pvalues_plot <- names(expl_ssp_enh_points_sum) |>
  map(function(x) {
    
    pvalues <- expl_ssp_enh_points_sum[[x]] |> 
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
  setNames(names(expl_ssp_enh_points_sum))

study1_expl_ssp_enh_variance_plot <- names(expl_ssp_enh_variance_sum) |>
  map(function(x) {
    
    expl_ssp_enh_variance_sum[[x]] |> 
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
  setNames(names(expl_ssp_enh_variance_sum))



## 1.3 Standard - Degraded comparison ----

study1_expl_ssp_deg_effect_plot <- names(expl_ssp_deg_points_sum) |> 
  map(function(x) {
    
    expl_ssp_deg_points_sum[[x]] |> 
      select(-iv) |> 
      rename(
        iv = level,
        condition = group
      ) |> 
      mutate(
        decision = paste0(iv, decision, sep="_"),
        iv = factor(iv, levels = c(-1, 1), labels = c("-1 SD", "+1 SD")),
        condition = case_when(
          condition == 0 ~ "Standard",
          condition == 1 ~ "Degraded",
        ),
        condition = factor(condition, levels = c("Standard", "Degraded"))
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
      scale_fill_manual("Unpredictability:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = ""
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(expl_ssp_deg_points_sum))

study1_expl_ssp_deg_eff_curve_plot <- unique(expl_ssp_deg_effects_sum$vars) |> 
  map(function(x) {
    
    expl_ssp_deg_effects_sum |> 
      filter(vars == x, str_detect(term, ":")) |>
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
        data = expl_ssp_deg_medians_sum[[x]],
        aes(y = med_effect_std, x = 32),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      )  +
      geom_text(
        aes(x = 32, y = 0.18, label = paste0(round(sum(p.value < .05)/n*100,1), " % of p-values < .05")),
        size = 3,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = expl_ssp_deg_medians_sum[[x]],
        aes(y = med_effect_std, label = as.character(round(med_effect_std,2)), x = 32),
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
  setNames(unique(expl_ssp_deg_effects_sum$vars))


study1_expl_ssp_deg_pvalues_plot <- names(expl_ssp_deg_points_sum) |>
  map(function(x) {
    
    pvalues <- expl_ssp_deg_points_sum[[x]] |> 
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
  setNames(names(expl_ssp_deg_points_sum))

study1_expl_ssp_deg_variance_plot <- names(expl_ssp_deg_variance_sum) |>
  map(function(x) {
    
    expl_ssp_deg_variance_sum[[x]] |> 
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
  setNames(names(expl_ssp_deg_variance_sum))

## Multiverse Figure: Violence Exposure ----

right_hand_themes <- theme(
  axis.text.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank()
)

study1_mult_pooled_ddm <- 
  (study1_prim_ssp_pooled_effect_plot$p_flanker + ylim(0.4,0.6) + ggtitle("Perceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     study1_expl_ssp_pooled_effect_plot$p_flanker  + ylim(0.4,0.6) + ggtitle("Perceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     study1_prim_ssp_pooled_effect_plot$interference_flanker  + ylim(80,120) + ggtitle("Interference") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     study1_expl_ssp_pooled_effect_plot$interference_flanker  + ylim(80,120) + ggtitle("Interference") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 4)) /
  # Row 2: Effect curves
  ((study1_prim_ssp_pooled_eff_curve_plot$p_flanker + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_expl_ssp_pooled_eff_curve_plot$p_flanker + right_hand_themes) + 
     (study1_prim_ssp_pooled_eff_curve_plot$interference_flanker + right_hand_themes) + 
     (study1_expl_ssp_pooled_eff_curve_plot$interference_flanker + right_hand_themes) + plot_layout(ncol = 4)) +
  # Row 3: Explained variances
  ((study1_prim_ssp_pooled_variance_plot$p_flanker + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_expl_ssp_pooled_variance_plot$p_flanker + right_hand_themes) + 
     (study1_prim_ssp_pooled_variance_plot$interference_flanker + right_hand_themes) + 
     (study1_expl_ssp_pooled_variance_plot$interference_flanker + right_hand_themes) + plot_layout(ncol = 4))

study1_mult_pooled_ddm <- 
  (study1_expl_ssp_pooled_effect_plot$p_flanker  + ylim(0.4,0.6) + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     study1_expl_ssp_pooled_effect_plot$interference_flanker  + ylim(80,120) + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 2)) 
  

ggsave(filename = here("manuscript/figures/pooled.png"), study1_mult_pooled_ddm, width = 10, height = 4, dpi = 600)


study1_mult_vio_ddm <-
  # Row 1: Slopes
  (study1_prim_ssp_enh_effect_plot$p_flanker + ylim(0.5,0.6) + ggtitle("Perceptual input\nEnhanced") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"), legend.position=c(0.7,0.3))  +
     study1_prim_ssp_deg_effect_plot$p_flanker + ylim(0.5,0.6) + ggtitle("Perceptual input\nDegraded") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") +
     study1_prim_ssp_enh_effect_plot$interference_flanker + ylim(80,120) + ggtitle("Interference\nEnhanced") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") +
     study1_prim_ssp_deg_effect_plot$interference_flanker + ylim(80,120) + ggtitle("Interference\nDegraded") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") + plot_layout(ncol = 4)) /
  # Row 2: Effect curves
  ((study1_prim_ssp_enh_eff_curve_plot$p_flanker + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_prim_ssp_deg_eff_curve_plot$p_flanker + right_hand_themes) + 
     (study1_prim_ssp_enh_eff_curve_plot$interference_flanker + right_hand_themes) + 
     (study1_prim_ssp_deg_eff_curve_plot$interference_flanker + right_hand_themes) + plot_layout(ncol = 4)) +
  # Row 3: Explained variances
  ((study1_prim_ssp_enh_variance_plot$p_flanker + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_prim_ssp_deg_variance_plot$p_flanker + right_hand_themes) + 
     (study1_prim_ssp_enh_variance_plot$interference_flanker + right_hand_themes) + 
     (study1_prim_ssp_deg_variance_plot$interference_flanker + right_hand_themes) + plot_layout(ncol = 4))

ggsave(filename = here("manuscript/figures/figure4.png"), study1_mult_vio_ddm, width = 10, height = 8, dpi = 600)

## Multiverse Figure: Unpredictability ----

right_hand_themes <- theme(
  axis.text.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank()
)


study1_mult_vio_unp_ddm <- 
  # Row 1: Slopes
  (study1_prim_ssp_enh_effect_plot$p_flanker + ylim(0.5,0.6) + ggtitle("Perceptual input\nEnhanced") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"), legend.position=c(0.7,0.3))  +
     study1_prim_ssp_deg_effect_plot$p_flanker + ylim(0.5,0.6) + ggtitle("Perceptual input\nDegraded") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") +
     study1_expl_ssp_enh_effect_plot$`unp_comp-p_flanker` + ylim(0.5,0.6) + ggtitle("Perceptual input\nEnhanced") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"), legend.position=c(0.7,0.3)) +
     study1_expl_ssp_deg_effect_plot$`unp_comp-p_flanker` + ylim(0.5,0.6) + ggtitle("Perceptual input\nDegraded") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") + plot_layout(ncol = 4)) /
  # Row 2: Effect curves
  ((study1_prim_ssp_enh_eff_curve_plot$p_flanker + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_prim_ssp_deg_eff_curve_plot$p_flanker + right_hand_themes) + 
     (study1_expl_ssp_enh_eff_curve_plot$`unp_comp-p_flanker` + right_hand_themes) + 
     (study1_expl_ssp_deg_eff_curve_plot$`unp_comp-p_flanker` + right_hand_themes) + plot_layout(ncol = 4)) +
  # Row 3: Explained variances
  ((study1_prim_ssp_enh_variance_plot$p_flanker + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_prim_ssp_deg_variance_plot$p_flanker + right_hand_themes) + 
     (study1_expl_ssp_enh_variance_plot$`unp_comp-p_flanker` + right_hand_themes) + 
     (study1_expl_ssp_deg_variance_plot$`unp_comp-p_flanker` + right_hand_themes) + plot_layout(ncol = 4))

ggsave(filename = here("manuscript/figures/p_all.png"), study1_mult_vio_unp_ddm, width = 10, height = 8, dpi = 600)

study1_mult_unp_pooled_ddm <- 
  (study1_expl_ssp_pooled_effect_plot$p_flanker + ylim(0.4,0.6) + ggtitle("Perceptual input") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) +
     study1_expl_ssp_pooled_effect_plot$interference_flanker  + ylim(80,120) + ggtitle("Interference") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + plot_layout(ncol = 2)) /
  # Row 2: Effect curves
  ((study1_expl_ssp_pooled_eff_curve_plot$p_flanker + ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_expl_ssp_pooled_eff_curve_plot$interference_flanker + right_hand_themes) + plot_layout(ncol = 2)) +
  # Row 3: Explained variances
  ((study1_expl_ssp_pooled_variance_plot$p_flanker + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_expl_ssp_pooled_variance_plot$interference_flanker + right_hand_themes) + plot_layout(ncol = 2)) 






study1_mult_unp_ddm <-  
  # Row 1: Slopes
  (study1_expl_ssp_enh_effect_plot$`unp_comp-p_flanker` + ylim(0.5,0.6) + ggtitle("Perceptual input\nEnhanced") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"), legend.position=c(0.7,0.3)) +
     study1_expl_ssp_deg_effect_plot$`unp_comp-p_flanker` + ylim(0.5,0.6) + ggtitle("Perceptual input\nDegraded") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") +
     study1_expl_ssp_enh_effect_plot$`unp_comp-interference_flanker` + ylim(80,120) + ggtitle("Interference\nEnhanced") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") +
     study1_expl_ssp_deg_effect_plot$`unp_comp-interference_flanker` + ylim(80,120) + ggtitle("Interference\nDegraded") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold")) + guides(fill="none") + plot_layout(ncol = 4)) /
  # Row 2: Effect curves
  ((study1_expl_ssp_enh_eff_curve_plot$`unp_comp-p_flanker` +  ggtitle("Effect curves") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_expl_ssp_deg_eff_curve_plot$`unp_comp-p_flanker` + right_hand_themes) + 
     (study1_expl_ssp_enh_eff_curve_plot$`unp_comp-interference_flanker` + right_hand_themes) + 
     (study1_expl_ssp_deg_eff_curve_plot$`unp_comp-interference_flanker` + right_hand_themes) + plot_layout(ncol = 4)) +
  # Row 3: Explained variances
  ((study1_expl_ssp_enh_variance_plot$`unp_comp-p_flanker` + ggtitle("Explained variance (%)") + theme(plot.title = element_text(hjust = 0.5, size = rel(0.9), face = "bold"))) +
     (study1_expl_ssp_deg_variance_plot$`unp_comp-p_flanker` + right_hand_themes) + 
     (study1_expl_ssp_enh_variance_plot$`unp_comp-interference_flanker` + right_hand_themes) + 
     (study1_expl_ssp_deg_variance_plot$`unp_comp-interference_flanker` + right_hand_themes) + plot_layout(ncol = 4)) 

ggsave(filename = here("manuscript/figures/figure5.png"), study1_mult_unp_ddm, width = 10, height = 8, dpi = 600)


## Multiverse Tables ----

### Pooled analyses ----

study1_pooled_results_df <- prim_ssp_pooled_effects_sum |> 
  filter(term != "(Intercept)") |> 
  select(decision, iv, dv, term, Std_Coefficient, p.value, Std_CI_low, Std_CI_high) |> 
  group_by(dv) |> 
  summarise(
    median_effect_Main = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p.value < .05)/n()*100) |> 
  mutate(across(
    -c(dv),
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  mutate(CI = paste0("[", median_CI_low, ", ", median_CI_high, "]")) |> 
  group_split(dv) |> 
  setNames(prim_ssp_pooled_effects_sum |> group_keys(dv) |> pull(dv))



condition_pooled_results_df <- prim_ssp_pooled_effects_sum |> 
  filter(term != "(Intercept)", dv != "rt_diff") |> 
  group_by(dv, term) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p.value < .05)/n()*100) |> 
  ungroup() |> 
  mutate(term = case_when(
    str_detect(term, ":") ~ "Interaction",
    term == "condition" ~ "Condition",
    term == "vio_comp" ~ "Violence"
  )) |> 
  pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
  mutate(across(
    -dv,
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  mutate(
    CI    = paste0("[", median_CI_low_Violence, ", ", median_CI_high_Violence, "]"),
    dv             = case_when(
      dv == "a_flanker" ~ "a",
      dv == "interference_flanker" ~ "interference",
      dv == "p_flanker" ~ "p",
      dv == "t0_flanker" ~ "t0"
    ),
    iv = "vio"
  ) |> 
  rename(
    median_effect = median_effect_Violence,
    p_sum = p_sum_Violence
  ) |> 
  select(dv, matches("median_effect"), ends_with("CI"), matches("p_sum"), iv) |>
  bind_rows(
    expl_ssp_pooled_effects_sum |> 
      filter(term != "(Intercept)") |> 
      group_by(dv, term) |> 
      summarise(
        median_effect = round(median(Std_Coefficient),2),
        median_CI_low = round(median(Std_CI_low), 2),
        median_CI_high = round(median(Std_CI_high), 2),
        p_sum = sum(p.value < .05)/n()*100) |> 
      ungroup() |> 
      mutate(term = case_when(
        str_detect(term, ":") ~ "Interaction",
        term == "condition" ~ "Condition",
        term == "unp_comp" ~ "Unpredictability"
      )) |> 
      pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
      mutate(across(
        -dv,
        ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
      )) |> 
      mutate(
        CI    = paste0("[", median_CI_low_Unpredictability, ", ", median_CI_high_Unpredictability, "]"),
        dv             = case_when(
          dv == "a_flanker" ~ "a",
          dv == "interference_flanker" ~ "interference",
          dv == "p_flanker" ~ "p",
          dv == "t0_flanker" ~ "t0"
        ),
        iv = "unp"
      ) |> 
      rename(
        median_effect = median_effect_Unpredictability,
        p_sum = p_sum_Unpredictability
      ) |> 
      select(dv, matches("median_effect"), ends_with("CI"), matches("p_sum"), iv)
    ) |> 
  mutate(n = c(4,2,1,3,8,6,5,7)) |> 
  arrange(n) |> 
  select(-n)  

condition_pooled_results_keys <- condition_pooled_results_df |> 
  group_keys(dv, iv) |> 
  unite("keys", dv, iv) |> pull()

condition_pooled_results_list <- condition_pooled_results_df |> 
  group_split(dv, iv) |> 
  setNames(condition_pooled_results_keys)

condition_pooled_results_table <- condition_pooled_results_df |> 
  select(dv, median_effect, CI, p_sum) |> 
  add_row(.before = 1, dv = "Violence exposure (primary)") |> 
  add_row(.after = 5, dv = "Unpredictability (exploratory)") |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate(
    dv = case_when(
      dv == "p" ~ "Perceptual input",
      dv == "interference" ~ "Interference",
      dv == "t0" ~ "Non-decision time",
      dv == "a" ~ "Boundary separation",
      TRUE ~ dv
    )
  ) |> 
  flextable() |> 
  width(j = 1, width = .75) %>% 
  set_header_labels(
    dv = ""
  ) |> 
  compose(i = 1, j = c(2), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 1, j = c(4), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  compose(i = 1, j = c(3), as_paragraph("95% CI"), part = "header") %>% 
  align(i = 1, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 10, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1, part = "header") %>% 
  bold(i = c(1,6)) |> 
  set_table_properties(width = 1, layout = "autofit") %>% 
  add_footer_row(
    values = " ",
    colwidths = 4
  ) %>% 
  add_footer_row(
    values = " ",
    colwidths = 4
  ) %>% 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse."), 
    part = "footer"
  )



### Condition effects ----

#### Violence exposure ----

study1_condition_vio_results_df <- prim_ssp_enh_effects_sum |> 
  group_by(dv, term) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p.value < .05)/n()*100) |> 
  ungroup() |> 
  mutate(term = case_when(
    str_detect(term, ":") ~ "Interaction",
    term == "condition" ~ "Condition",
    term == "vio_comp" ~ "Violence"
  )) |> 
  pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
  mutate(across(
    -dv,
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  mutate(
    Interaction_CI = paste0("[", median_CI_low_Interaction, ", ", median_CI_high_Interaction, "]"),
    condition_CI   = paste0("[", median_CI_low_Condition, ", ", median_CI_high_Condition, "]"),
    Violence_CI    = paste0("[", median_CI_low_Violence, ", ", median_CI_high_Violence, "]"),
    dv             = case_when(
      dv == "rt_diff" ~ "RT",
      dv == "a_flanker" ~ "a",
      dv == "interference_flanker" ~ "interference",
      dv == "p_flanker" ~ "p",
      dv == "t0_flanker" ~ "t0"
    ),
    condition = "enhanced"
  ) |> 
  select(dv, condition, matches("median_effect"), ends_with("CI"), matches("p_sum")) |>
  bind_rows(
    prim_ssp_deg_effects_sum |> 
      group_by(dv, term) |> 
      summarise(
        median_effect = round(median(Std_Coefficient),2),
        median_CI_low = round(median(Std_CI_low), 2),
        median_CI_high = round(median(Std_CI_high), 2),
        p_sum = sum(p.value < .05)/n()*100) |> 
      ungroup() |> 
      mutate(term = case_when(
        str_detect(term, ":") ~ "Interaction",
        term == "condition" ~ "Condition",
        term == "vio_comp" ~ "Violence"
      )) |> 
      pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
      mutate(across(
        -dv,
        ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
      )) |> 
      mutate(
        Interaction_CI = paste0("[", median_CI_low_Interaction, ", ", median_CI_high_Interaction, "]"),
        condition_CI   = paste0("[", median_CI_low_Condition, ", ", median_CI_high_Condition, "]"),
        Violence_CI    = paste0("[", median_CI_low_Violence, ", ", median_CI_high_Violence, "]"),
        dv             = case_when(
          dv == "rt_diff"   ~ "rt",
          dv == "a_flanker" ~ "a",
          dv == "interference_flanker" ~ "interference",
          dv == "p_flanker" ~ "p",
          dv == "t0_flanker" ~ "t0"
        ),
        condition = "degraded"
      ) |> 
      select(dv, condition, matches("median_effect"), ends_with("CI"), matches("p_sum")) 
  ) |> 
  mutate(n = c(5,3,2,1,4,10,8,7,6,9)) |> 
  arrange(n) |> 
  select(-n)  

study1_condition_vio_results_keys <- study1_condition_vio_results_df |> 
  group_keys(dv, condition) |> 
  unite("keys", dv, condition) |> pull()

study1_condition_vio_results_list <- study1_condition_vio_results_df |> 
  group_split(dv, condition) |> 
  setNames(study1_condition_vio_results_keys)

study1_condition_vio_results_table <- study1_condition_vio_results_df |> 
  select(dv, median_effect_Condition, condition_CI, p_sum_Condition, median_effect_Violence, Violence_CI, p_sum_Violence, median_effect_Interaction, Interaction_CI, p_sum_Interaction) |> 
  add_row(.before = 1, dv = "Standard - Enhanced") |> 
  add_row(.after = 6, dv = "Standard - Degraded") |> 
  add_column(empty1 = "", .after = "p_sum_Condition") |> 
  add_column(empty2 = "", .after = "p_sum_Violence") |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate(
    dv = case_when(
      dv == "rt" ~ "RT (difference score)",
      dv == "p" ~ "Perceptual input",
      dv == "interference" ~ "Interference",
      dv == "t0" ~ "Non-decision time",
      dv == "a" ~ "Boundary separation",
      TRUE ~ dv
    )
  ) |> 
  flextable() |> 
  width("empty1", width = .2) %>% 
  width("empty2", width = .2) %>% 
  width(j = 1, width = .75) %>% 
  set_header_labels(
    dv = "",
    empty1 = "",
    empty2 = ""
  ) |> 
  add_header_row(
    values = c(" ", "Task condition", " ", "Violence exposure", " ", "Interaction"),
    colwidths = c(1, 3, 1, 3, 1, 3)
  ) |> 
  compose(i = 2, j = c(2,6, 10), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 2, j = c(4,8,12), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  compose(i = 2, j = c(3,7,11), as_paragraph("95% CI"), part = "header") %>% 
  align(i = 1:2, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, j = c(2:4,6:8,10:12), border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 2, border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 12, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1:2, part = "header") %>% 
  bold(i = c(1,7)) |> 
  set_table_properties(width = 1, layout = "autofit") %>% 
  add_footer_row(
    values = " ",
    colwidths = 12
  ) %>% 
  add_footer_row(
    values = " ",
    colwidths = 12
  ) %>% 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "Task conditions were dummy-coded with the standard condition as the reference. The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse."), 
    part = "footer"
  )


#### Unpredictability ---- 

study1_simpslopes_unp_df <- 
  bind_rows(
    exploratory_ssp_simslopes_enh |> 
      filter(iv == "unp_comp") |>
      select(dv, estimate, p.value, modx.value) |> 
      group_by(dv, modx.value) |> 
      summarise(
        median_ss = median(estimate),
        p_sum     = sum(p.value<.05)/n()*100
      ) |> 
      ungroup() |> 
      mutate(
        modx.value = ifelse(modx.value == 0, "Standard", "enhanced"),
        condition = "Enhanced") |> 
      pivot_wider(names_from = modx.value, values_from = c(median_ss, p_sum)),
    exploratory_ssp_simslopes_deg |> 
      filter(iv == "unp_comp") |>
      select(dv, estimate, p.value, modx.value) |> 
      group_by(dv, modx.value) |> 
      summarise(
        median_ss = median(estimate),
        p_sum     = sum(p.value<.05)/n()*100
      ) |> 
      ungroup() |> 
      mutate(
        modx.value = ifelse(modx.value == 0, "Standard", "degraded"),
        condition = "Degraded") |> 
      pivot_wider(names_from = modx.value, values_from = c(median_ss, p_sum))
  ) |> 
  mutate(
    across(
      -dv,
      ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
    )) 

study1_simslopes_unp_keys <- study1_simpslopes_unp_df  |> 
  group_keys(dv, condition) |> 
  unite("keys", dv, condition) |> pull()

study1_simslopes_unp_list <- study1_simpslopes_unp_df |> 
  group_split(dv, condition) |> 
  setNames(study1_simslopes_unp_keys)





study1_condition_unp_results_df <- expl_ssp_enh_effects_sum |> 
  filter(str_detect(vars, "unp_comp"), str_detect(term, "^condition$", negate = T)) |>
  group_by(vars, term) |> 
  summarise(
    median_effect = round(median(Std_Coefficient),2),
    median_CI_low = round(median(Std_CI_low), 2),
    median_CI_high = round(median(Std_CI_high), 2),
    p_sum = sum(p.value < .05)/n()*100) |> 
  ungroup() |> 
  mutate(term = case_when(
    str_detect(term, ":") ~ "Interaction",
    term == "unp_comp" ~ "Unpredictability"
  )) |> 
  pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
  mutate(across(
    -vars,
    ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
  )) |> 
  separate(vars, into = c("iv", "dv"), sep = "-", remove = F) |> 
  mutate(
    Interaction_CI      = paste0("[", median_CI_low_Interaction, ", ", median_CI_high_Interaction, "]"),
    Unpredictability_CI = paste0("[", median_CI_low_Unpredictability, ", ", median_CI_high_Unpredictability, "]"),
    dv  = case_when(
      dv == "a_flanker" ~ "a",
      dv == "interference_flanker" ~ "interference",
      dv == "p_flanker" ~ "p",
      dv == "t0_flanker" ~ "t0"
    ),
    condition = "enhanced"
  ) |> 
  select(dv, condition, vars, matches("median_effect"), ends_with("CI"), matches("p_sum")) |>
  bind_rows(
    expl_ssp_deg_effects_sum |> 
      filter(str_detect(vars, "unp_comp"), str_detect(term, "^condition$", negate = T)) |>
      group_by(vars, term) |> 
      summarise(
        median_effect = round(median(Std_Coefficient),2),
        median_CI_low = round(median(Std_CI_low), 2),
        median_CI_high = round(median(Std_CI_high), 2),
        p_sum = sum(p.value < .05)/n()*100) |> 
      ungroup() |> 
      mutate(term = case_when(
        str_detect(term, ":") ~ "Interaction",
        term == "unp_comp" ~ "Unpredictability"
      )) |> 
      pivot_wider(names_from = "term", values_from = c(median_effect, median_CI_low, median_CI_high, p_sum)) |> 
      mutate(across(
        -vars,
        ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
      )) |> 
      separate(vars, into = c("iv", "dv"), sep = "-", remove = F) |> 
      mutate(
        Interaction_CI      = paste0("[", median_CI_low_Interaction, ", ", median_CI_high_Interaction, "]"),
        Unpredictability_CI = paste0("[", median_CI_low_Unpredictability, ", ", median_CI_high_Unpredictability, "]"),
        dv  = case_when(
          dv == "a_flanker" ~ "a",
          dv == "interference_flanker" ~ "interference",
          dv == "p_flanker" ~ "p",
          dv == "t0_flanker" ~ "t0"
        ),
        condition = "degraded"
      ) |> 
      select(dv, vars, condition, matches("median_effect"), ends_with("CI"), matches("p_sum"))
  ) |> 
  mutate(n = c(4,2,1,3,8,6,5,7)) |> 
  arrange(n) |> 
  select(-n)  

study1_condition_unp_results_keys <- study1_condition_unp_results_df |> 
  group_keys(dv, condition) |> 
  unite("keys", dv, condition) |> pull()

study1_condition_unp_results_list <- study1_condition_unp_results_df |> 
  group_split(dv, condition) |> 
  setNames(study1_condition_unp_results_keys)


study1_condition_unp_results_table <- study1_condition_unp_results_df |> 
  select(dv, median_effect_Unpredictability, Unpredictability_CI, p_sum_Unpredictability, median_effect_Interaction, Interaction_CI, p_sum_Interaction) |> 
  add_row(.before = 1, dv = "Standard - Enhanced") |> 
  add_row(.after = 5, dv = "Standard - Degraded") |> 
  add_column(empty1 = "", .after = "p_sum_Unpredictability") |> 
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) |> 
  mutate(
    dv = case_when(
      dv == "p" ~ "Perceptual input",
      dv == "interference" ~ "Interference",
      dv == "t0" ~ "Non-decision time",
      dv == "a" ~ "Boundary separation",
      TRUE ~ dv
    )
  ) |> 
  flextable() |> 
  width("empty1", width = .2) |> 
  width(j = 1, width = .75) |>  
  add_header_row(
    values = c(" ", "Unpredictability", " ", "Interaction"),
    colwidths = c(1, 3, 1, 3)
  ) |> 
  set_header_labels(
    dv     = "",
    empty1 = ""
    ) |> 
  compose(i = 2, j = c(2,6), as_paragraph("\U1D6FD"), part = "header") %>% 
  compose(i = 2, j = c(3,7), as_paragraph("95% CI"), part = "header") %>% 
  compose(i = 2, j = c(4,8), as_paragraph(as_i("p "), "(%)"), part = "header") %>% 
  align(i = 1:2, align = "center", part = "header") |> 
  border_remove() %>% 
  border(i = 1, border.top = fp_border_default(), part = "header") %>% 
  border(i = 1, j = c(2:4,6:8), border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 2, border.bottom = fp_border_default(), part = "header") %>% 
  border(i = 10, border.bottom = fp_border_default(), part = "body") %>% 
  bold(i = 1:2, part = "header") %>% 
  bold(i = c(1,6)) |> 
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
    as_paragraph(as_i("Note: "), "Task conditions were dummy-coded with the standard condition as the reference. The p (%) column reflects the number of analyses that produced p-values < .05 for a given multiverse."), 
    part = "footer"
  )

