
library(cowplot)
library(tidyverse)
library(specr)
library(ggsci)


load("preregistrations/2_study1/analysis_objects/primary_multiverse_summaries.RData")
load("preregistrations/2_study1/analysis_objects/exploratory_multiverse_summaries.RData")

cleaned_data <- read_csv('data/2_study1/2_cleaned_data.csv')


cleaned_data |> 
  select(id, starts_with("rt_")) |> 
  pivot_longer(
    starts_with("rt"),
    names_to = c("type", "task", "congruency", "condition"),
    names_sep = "_",
    values_to = "rt") |> 
  ggplot(aes(rt)) +
  geom_histogram(aes(fill = congruency), alpha = 0.5) +
  facet_grid(condition~.)


# 1. Primary Analyses: Plots ----------------------------------------------

# ggplot2 theme -----------------------------------------------------------
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
    
    ggplot() +
      # coord_cartesian(xlim = c(vio_min, vio_max), ylim = c(y_min, y_max)) +
      geom_segment(data = effect_data, aes(x = vio_min, xend = vio_max, y = estimate_vio_comp*vio_min, yend = estimate_vio_comp*vio_max, color = p.value_chr)) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = "Violence Exposure",
        y = "Predicted\n"
      ) +
      geom_segment(data = prim_ssp_pooled_medians_sum[[x]], aes(x = vio_min, xend = vio_max, y = med_effect_vio_comp*vio_min, yend = med_effect_vio_comp*vio_max), size = 1.5) + 
      geom_point(data = prim_ssp_pooled_medians_sum[[x]], aes(x = vio_min, y = vio_min*med_effect_vio_comp), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = prim_ssp_pooled_medians_sum[[x]], aes(x = vio_max, y = vio_max*med_effect_vio_comp), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(prim_ssp_pooled_effects_sum$dv))


study1_prim_ssp_pooled_decisions_pval_plot <- names(prim_ssp_pooled_decisions_sum) |> 
  map(function(x) {
    
    prim_ssp_pooled_decisions_sum[[x]] |>  
      ggplot(aes(filters_plot, sum_pvalue, fill = filters)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylim(0, 100) +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "% p < .05"
      ) +
      scale_fill_uchicago() 
    
  }) |> 
  setNames(names(prim_ssp_pooled_decisions_sum))

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
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "Explained variance in estimate (%)"
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
      scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = "Predicted\n"
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(prim_ssp_enh_points_sum))


study1_prim_ssp_enh_decisions_pval_plot<- names(prim_ssp_enh_decisions_sum) |> 
  map(function(x) {
    
    prim_ssp_enh_decisions_sum[[x]] |> 
      ggplot(aes(filters_plot, sum_pvalue, fill = filters)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylim(0, 100) +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "% p < .05"
        
      ) +
      scale_fill_uchicago()
    
  }) |> 
  setNames(names(prim_ssp_enh_decisions_sum))


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
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "Explained variance in estimate (%)"
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
          condition == 1 ~ "degraded",
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
      scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = "Predicted\n"
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(prim_ssp_deg_points_sum))


study1_prim_ssp_deg_decisions_pval_plot<- names(prim_ssp_deg_decisions_sum) |> 
  map(function(x) {
    
    prim_ssp_deg_decisions_sum[[x]] |> 
      ggplot(aes(filters_plot, sum_pvalue, fill = filters)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylim(0, 100) +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "% p < .05"
        
      ) +
      scale_fill_uchicago()
    
  }) |> 
  setNames(names(prim_ssp_deg_decisions_sum))


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
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "Explained variance in estimate (%)"
      )
  }) |> 
  setNames(names(prim_ssp_deg_variance_sum))


# Exploratory analyses: Simple slopes ---------------------------------------------


## Median Effects and Simple slopes ----
study1_expl_enh_interaction_table <- expl_ssp_enh_effects_sum |> 
  filter(str_detect(vars, "p_|interference")) |> 
  filter(str_detect(term, ":")) |> 
  group_by(iv, dv) |> 
  summarise(
    median_effect = median(Std_Coefficient),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  arrange(desc(pvalue_sum))

study1_expl_enh_ss_table <- exploratory_ssp_simslopes_enh |> 
  filter(str_detect(dv, "p_|interference")) |> 
  group_by(iv, dv, modx.value) |> 
  summarise(
    median_effect = median(estimate),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  left_join(study1_expl_enh_interaction_table  |> select(iv, dv, pvalue_overall = pvalue_sum)) |> 
  filter(pvalue_overall > 0) |> 
  arrange(desc(pvalue_overall))

study1_expl_enh_main_table <- expl_ssp_enh_effects_sum |> 
  filter(str_detect(vars, "p_|interference")) |> 
  filter(str_detect(term, "condition", negate = T)) |> 
  group_by(iv, dv) |> 
  summarise(
    median_effect = median(Std_Coefficient),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  arrange(desc(pvalue_sum))

study1_expl_deg_interaction_table <- expl_ssp_deg_effects_sum |> 
  filter(str_detect(vars, "p_|interference")) |> 
  filter(str_detect(term, ":")) |> 
  group_by(iv, dv) |> 
  summarise(
    median_effect = median(Std_Coefficient),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  arrange(desc(pvalue_sum))

study1_expl_deg_ss_table <- exploratory_ssp_simslopes_deg |> 
  filter(str_detect(dv, "p_|interference")) |> 
  group_by(iv, dv, modx.value) |> 
  summarise(
    median_effect = median(estimate),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  left_join(study1_expl_deg_interaction_table|> select(iv, dv, pvalue_overall = pvalue_sum)) |> 
  filter(pvalue_overall > 0) |> 
  arrange(desc(pvalue_overall))


# Exploratory analyses: Simple slopes ---------------------------------------------

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
      scale_fill_manual("Adversity:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = "Predicted\n"
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(expl_ssp_enh_points_sum))


study1_expl_ssp_enh_decisions_pval_plot <- names(expl_ssp_enh_decisions_sum) |> 
  map(function(x) {
    
    expl_ssp_enh_decisions_sum[[x]] |> 
      ggplot(aes(filters_plot, sum_pvalue, fill = filters)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylim(0, 100) +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "% p < .05"
        
      ) +
      scale_fill_uchicago()
    
  }) |> 
  setNames(names(expl_ssp_enh_decisions_sum))


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
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "Explained variance in estimate (%)"
      )
  }) |> 
  setNames(names(expl_ssp_enh_variance_sum))

## Plots: Standard - Degraded ----

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
      scale_fill_manual("Adversity:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = "Predicted\n"
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(expl_ssp_deg_points_sum))


study1_expl_ssp_deg_decisions_pval_plot <- names(expl_ssp_deg_decisions_sum) |> 
  map(function(x) {
    
    expl_ssp_deg_decisions_sum[[x]] |> 
      ggplot(aes(filters_plot, sum_pvalue, fill = filters)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylim(0, 100) +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "% p < .05"
        
      ) +
      scale_fill_uchicago()
    
  }) |> 
  setNames(names(expl_ssp_deg_decisions_sum))


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
      coord_flip() +
      guides(fill = 'none') +
      labs(
        x = "",
        y = "Explained variance in estimate (%)"
      )
  }) |> 
  setNames(names(expl_ssp_deg_variance_sum))


