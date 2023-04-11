library(tidyverse)
library(cowplot)
library(ggsci)


cleaned_data = read_csv("data/1_pilot/2_cleaned_data.csv")
load(file = "preregistrations/1_pilot/analysis_objects/primary_mult_summaries.RData")
load(file = "preregistrations/1_pilot/analysis_objects/exploratory_mult_summaries.RData")

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


exploratory_variable_set <- "ssp_p|ssp_a|ssp_t0|ssp_interference|rt_"




# 1. Primary Analyses: Lists of all plots ----------------------------------



pilot_prim_lm_effect_plot <- unique(prim_lm_effects_sum$dv) |> 
  map(function(x) {
    
    effect_data <- prim_lm_effects_sum |> 
      filter(dv == x) |> 
      select(decision, dv, term, estimate, p.value) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(names_from = term, values_from = c(estimate, p.value)) |> 
      mutate(
        p.value_chr = ifelse(p.value_violence_composite < .05, "sig", "non-sig")
      )
    
    vio_min <- cleaned_data$violence_composite |> scale() |>  min()
    vio_max <- cleaned_data$violence_composite |> scale() |>  max()
    y_min <- min(effect_data$estimate_intercept) + vio_max*min(effect_data$estimate_violence_composite) -0.05
    y_max <- max(effect_data$estimate_intercept) + vio_max*max(effect_data$estimate_violence_composite) +0.05
    
    ggplot() +
      # coord_cartesian(xlim = c(vio_min, vio_max), ylim = c(y_min, y_max)) +
      geom_segment(data = effect_data, aes(x = vio_min, xend = vio_max, y = estimate_violence_composite*vio_min, yend = estimate_violence_composite*vio_max, color = p.value_chr)) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = "Violence Exposure",
        y = "Predicted\n"
      ) +
      geom_segment(data = prim_lm_medians_sum[[x]], aes(x = vio_min, xend = vio_max, y = med_effect_violence_composite*vio_min, yend = med_effect_violence_composite*vio_max), size = 1.5) + 
      geom_point(data = prim_lm_medians_sum[[x]], aes(x = vio_min, y = vio_min*med_effect_violence_composite), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = prim_lm_medians_sum[[x]], aes(x = vio_max, y = vio_max*med_effect_violence_composite), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(prim_lm_effects_sum$dv))

# Plot lm decisions
pilot_prim_lm_decisions_pval_plot <- names(prim_lm_decisions_sum) |> 
  map(function(x) {
    
    prim_lm_decisions_sum[[x]] |>  
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
  setNames(names(prim_lm_decisions_sum))


# Plot lm p-value curve
pilot_prim_lm_pvalues_plot <- unique(prim_lm_effects_sum$dv) |>
  map(function(x) {
    
    pvalues <- prim_lm_effects_sum |>
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
  setNames(unique(prim_lm_effects_sum$dv))

pilot_prim_lm_variance_plot <- names(prim_lm_variance_sum) |>
  map(function(x) {
    
    prim_lm_variance_sum[[x]] |> 
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
  setNames(names(prim_lm_variance_sum))



pilot_prim_lmer_effect_plot <- names(prim_lmer_points_sum) |> 
  map(function(x) {
    
    prim_lmer_points_sum[[x]] |> 
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
      scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = "Predicted\n"
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(prim_lmer_points_sum))


pilot_prim_lmer_decisions_eff_plot <- unique(prim_lmer_decisions_sum$dv) |> 
  map(function(x) {
    
    prim_lmer_decisions_sum |> 
      ggplot(aes(filters_plot, med_estimate, color = filters)) +
      geom_hline(data = prim_lmer_medians_sum[[x]], aes(yintercept = med_effect_std), linetype = "dashed", size = 1) +
      geom_pointrange(aes(ymin = med_estimate - se, ymax = med_estimate + se)) +
      scale_y_continuous(labels = function(y) format(y, nsmall = 1)) +
      coord_flip() +
      labs(
        x = "",
        y = "Median Standardized Effect"
      ) +
      guides(color = 'none') +
      theme(axis.text.y = element_blank()) +
      scale_color_uchicago() 
  }) |> 
  setNames(unique(prim_lmer_decisions_sum$dv))


pilot_prim_lmer_decisions_pval_plot <- names(prim_lmer_decisions_sum) |> 
  map(function(x) {
    
    prim_lmer_decisions_sum[[x]] |> 
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
  setNames(names(prim_lmer_decisions_sum))

pilot_prim_lmer_pvalues_plot <- names(prim_lmer_points_sum) |>
  map(function(x) {
    
    pvalues <- prim_lmer_points_sum[[x]] |> 
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
  setNames(names(prim_lmer_points_sum))

pilot_prim_lmer_variance_plot <- names(prim_lmer_variance_sum) |>
  map(function(x) {
    
    prim_lmer_variance_sum[[x]] |> 
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
  setNames(names(prim_lmer_variance_sum))



# 2. Exploratory Analyses: Effects -------------------------------------------

pilot_expl_lmer_median_effect_table <- expl_lmer_effects_sum |> 
  filter(str_detect(dv, "hddm_v|hddm_t|rt_")) |> 
  filter(str_detect(term, ":")) |> 
  group_by(task, iv, dv) |> 
  summarise(
    median_effect = median(Std_Coefficient),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  arrange(desc(pvalue_sum))

pilot_expl_ss_table <- exploratory_lmer_simslopes |> 
  filter(str_detect(dv, "hddm_v|hddm_t|rt_")) |> 
  group_by(task, iv, dv, modx.value) |> 
  summarise(
    median_effect = median(estimate),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  left_join(expl_median_effect_table |> select(task, iv, dv, pvalue_overall = pvalue_sum)) |> 
  filter(pvalue_overall > 0) |> 
  arrange(desc(pvalue_overall))


pilot_expl_lm_median_effect_table <- expl_lm_effects_sum |> 
  filter(str_detect(dv, "ssp_p|ssp_t0|ssp_interference")) |> 
  filter(term != "(Intercept)") |> 
  group_by(task, iv, dv) |> 
  summarise(
    median_effect = median(Std_Coefficient),
    pvalue_sum    = sum(p.value < .05) / n() * 100
  ) |> 
  arrange(desc(pvalue_sum))


# exploratory plots
pilot_expl_lm_effect_plot <- unique(expl_lm_effects_sum$vars) |> 
  map(function(x) {
    
    iv = expl_lm_effects_sum |> 
      filter(vars == x) |> pull(iv) |> unique()
    
    effect_data <- expl_lm_effects_sum |> 
      filter(vars == x) |> 
      select(decision, vars, dv, iv, term, estimate, p.value) |> 
      mutate(term = ifelse(term == "(Intercept)", "intercept", term)) |> 
      pivot_wider(names_from = term, values_from = c(estimate, p.value)) |> 
      mutate(
        p.value_chr = ifelse("p.value_{{x}}" < .05, "sig", "non-sig")
      ) |> 
      rename(
        !!"estimate_main" := paste0("estimate_", {{iv}}),
        !!"p.value_main" := paste0("p.value_", {{iv}}),
      )
    
    iv = effect_data$iv |> unique()
    
    unp_min <- cleaned_data[[effect_data$iv |> unique()]] |> scale() |>  min()
    unp_max <- cleaned_data[[effect_data$iv |> unique()]] |> scale() |>  max()
    y_min <- min(effect_data$estimate_intercept) + unp_max*min(effect_data$estimate_main) -0.05
    y_max <- max(effect_data$estimate_intercept) + unp_max*max(effect_data$estimate_main) +0.05
    
    ggplot() +
      # coord_cartesian(xlim = c(unp_min, unp_max), ylim = c(y_min, y_max)) +
      geom_segment(data = effect_data, aes(x = unp_min, xend = unp_max, y = estimate_main*unp_min, yend = estimate_main*unp_max, color = p.value_chr)) +
      scale_color_manual(values = pval_colors) +
      guides(color = 'none') +
      labs(
        x = "Unpredictability",
        y = "Predicted\n"
      ) +
      geom_segment(data = expl_lm_medians_sum[[x]], aes(x = unp_min, xend = unp_max, y = med_effect_main*unp_min, yend = med_effect_main*unp_max), size = 1.5) + 
      geom_point(data = expl_lm_medians_sum[[x]], aes(x = unp_min, y = unp_min*med_effect_main), size = 2, fill = "white", color = "black", shape = 21) +
      geom_point(data = expl_lm_medians_sum[[x]], aes(x = unp_max, y = unp_max*med_effect_main), size = 2, fill = "white", color = "black", shape = 21) 
    
  }) |> 
  setNames(unique(expl_lm_effects_sum$vars))

# Plot lm decisions
pilot_expl_lm_decisions_pval_plot <- names(expl_lm_decisions_sum) |> 
  map(function(x) {
    
    expl_lm_decisions_sum[[x]] |>  
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
  setNames(names(expl_lm_decisions_sum))


# Plot lm p-value curve
pilot_expl_lm_pvalues_plot <- unique(expl_lm_effects_sum$vars) |>
  map(function(x) {
    
    pvalues <- expl_lm_effects_sum |>
      filter(vars == x, term != "(Intercept)") |> 
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
  setNames(unique(expl_lm_effects_sum$vars))

pilot_expl_lm_variance_plot <- names(expl_lm_variance_sum) |>
  map(function(x) {
    
    expl_lm_variance_sum[[x]] |> 
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
  setNames(names(expl_lm_variance_sum))



pilot_expl_lmer_effect_plot <- names(expl_lmer_points_sum) |> 
  map(function(x) {
    
    expl_lmer_points_sum[[x]] |> 
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
      scale_fill_manual("Unpredictability:", labels = c("Low", "High"),values = c("white","gray60")) +
      labs(
        x = "Condition",
        y = "Predicted\n"
      ) +
      theme(legend.position=c(0.8,0.7))
  }) |> 
  setNames(names(expl_lmer_points_sum))


pilot_expl_lmer_decisions_eff_plot <- unique(expl_lmer_decisions_sum$dv) |> 
  map(function(x) {
    
    expl_lmer_decisions_sum |> 
      ggplot(aes(filters_plot, med_estimate, color = filters)) +
      geom_hline(data = expl_lmer_medians_sum[[x]], aes(yintercept = med_effect_std), linetype = "dashed", size = 1) +
      geom_pointrange(aes(ymin = med_estimate - se, ymax = med_estimate + se)) +
      scale_y_continuous(labels = function(y) format(y, nsmall = 1)) +
      coord_flip() +
      labs(
        x = "",
        y = "Median Standardized Effect"
      ) +
      guides(color = 'none') +
      theme(axis.text.y = element_blank()) +
      scale_color_uchicago() 
  }) |> 
  setNames(unique(expl_lmer_decisions_sum$dv))


pilot_expl_lmer_decisions_pval_plot <- names(expl_lmer_decisions_sum) |> 
  map(function(x) {
    
    expl_lmer_decisions_sum[[x]] |> 
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
  setNames(names(expl_lmer_decisions_sum))

pilot_expl_lmer_pvalues_plot <- names(expl_lmer_points_sum) |>
  map(function(x) {
    
    pvalues <- expl_lmer_points_sum[[x]] |> 
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
  setNames(names(expl_lmer_points_sum))

pilot_expl_lmer_variance_plot <- names(expl_lmer_variance_sum) |>
  map(function(x) {
    
    expl_lmer_variance_sum[[x]] |> 
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
  setNames(names(expl_lmer_variance_sum))








save(pilot_prim_lm_effect_plot, pilot_prim_lm_decisions_pval_plot, pilot_prim_lm_pvalues_plot, 
     pilot_pilot_prim_lm_variance_plot, pilot_prim_lmer_effect_plot, pilot_prim_lmer_decisions_eff_plot, 
     pilot_prim_lmer_decisions_pval_plot, pilot_prim_lmer_pvalues_plot, pilot_prim_lmer_variance_plot,
     pilot_expl_lmer_median_effect_table, pilot_expl_ss_table, pilot_expl_lm_median_effect_table,
     "preregistrations/1_pilot/analysis_objects/")


# Correlations ------------------------------------------------------------

cleaned_data |> 
  select(
    change_hddm_v, cueing_neutral_hddm_v, cueing_cued_hddm_v, flanker_con_hddm_v, flanker_incon_hddm_v,
    flanker_ssp_p, flanker_ssp_interference, flanker_ssp_t0, flanker_ssp_a
  ) |> 
  filter(is.finite(flanker_ssp_interference)) |> 
  cor(use = 'complete.obs') |> 
  corrplot::corrplot(method = "number")

cleaned_data |> 
  filter(is.finite(flanker_ssp_interference)) |> 
  ggplot(aes(flanker_con_hddm_v, flanker_fixed_hddm_a)) +
  stat_binhex(size=2) 
