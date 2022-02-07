library(tidyverse)
library(cowplot)

load(here("data", "1_pilot", "secondary_analyses", "flanker", "3_multiverse_extracted_effects.Rdata"))


# ggplot2 theme -----------------------------------------------------------
theme_set(
  theme_bw() +
    theme(
      axis.line.y       = element_line(),
      axis.text.y       = element_text(size = rel(.75)),
      axis.title.y      = element_text(size = rel(1), margin = margin(1,0,0,0,"lines")),
      axis.ticks.y      = element_line(),
      axis.line.x       = element_blank(),
      axis.text.x       = element_blank(),
      axis.ticks.x      = element_blank(),
      axis.title.x      = element_blank(),
      panel.border      = element_blank(), 
      panel.spacing.y   = unit(0.5, "lines"),
      plot.margin       = margin(.25,.25,.25,.25,"lines"),
      plot.background   = element_rect(color = NA),
      plot.title        = element_text(size = rel(.85), hjust = 0, margin = margin(0,0,.5,0, "lines")),
      plot.subtitle     = element_blank(),
      panel.grid        = element_line(color = NA),
      strip.background  = element_blank(), 
      strip.placement   = "outside",
      strip.text        = element_text(size = rel(.85), angle = 0)
    )
)

pval_colors <- c("pos-sig" = "#006D77", "non" = "gray70")
my_alphas <- c(.5, 1)
my_limits <- c(1,64)

exploratory_effects_unp_flanker %<>%
  mutate(dv = factor(dv, levels = c("Perceptual input (p)", "Rate of spotlight\nshrinking (rd)", "Interference\n(sda/rd)" )))


# specification curve -----------------------------------------------------
spec_curves_ssp <- 
  map(c("DDM"), function(x){
    
    # Setup
    dv_which <- x
    
    effs <- 
      exploratory_effects_unp_flanker %>% 
      filter(dv_group == x, mod_term_group == "Main Effect") %>% 
      mutate(
        #  dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    medians <- 
      exploratory_effects_unp_flanker %>% 
      filter(dv_group == x, mod_term_group == "Main Effect") %>% 
      mutate(
        #   dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    spec_grid_data <- 
      effs %>% 
      select(mod_term_label, dv, dv_group, starts_with("spec"), mod_p.value, mod_sig) %>% 
      mutate(
        #   dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)) %>% 
      pivot_longer(cols = spec_no_resize:spec_outliers, names_to = "spec_var", values_to = "spec_value") %>% 
      ungroup()
    
    
    # Plots
    eff_curve <- 
      map(c("EFA - daily", "EFA - routine", "EFA - spatial", "EFA - clutter", "EFA - social", "Perc. + QUIC", "CHAOS", "Obj. unpredictability"), function(x) {
        effs %>% 
          filter(spec_iv_type == x) %>%
          ggplot(aes(y = mod_std_coefficient, x = spec_rank, color = mod_sig)) +
          geom_ribbon(
            aes(ymin = mod_ci_low, ymax = mod_ci_high, x = spec_rank),
            fill = "gray90",
            inherit.aes = F,
            show.legend = F
          ) +
          geom_hline(aes(yintercept = 0), size = .5,linetype = "solid") +
          geom_point(size = 1, shape = 19, show.legend = F) + 
          geom_point(
            data = medians %>% filter(spec_iv_type == x),
            aes(y = median_dbl, x = 32),
            shape = 21,
            size  = 2.5,
            fill  = "white",
            stroke = 1,
            show.legend = F,
            inherit.aes = F
          ) +
          geom_label(
            data = medians %>% filter(spec_iv_type == x),
            aes(y = median_dbl, label = median_chr, x = 32),
            nudge_y = .055,
            size = 2.5,
            show.legend = F,
            inherit.aes = F
          ) +
          #scale_x_continuous("Specification Rank", limits = my_limits) + 
          scale_y_continuous(expression(beta)) +
          scale_color_manual(values = pval_colors) +
          scale_fill_manual(values = pval_colors) +
          scale_alpha_manual(values = my_alphas) +
          facet_wrap(~dv, ncol = 5) +
          ggtitle("Effect Size Curve") +
          theme(
           # strip.text   = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines"))
          )})
      
    
    sample_sizes <- 
      map(c("EFA - daily", "EFA - routine", "EFA - spatial", "EFA - clutter", "EFA - social", "Perc. + QUIC", "CHAOS", "Obj. unpredictability"), function(x) {
        effs %>% 
          filter(spec_iv_type == x) %>%
          ggplot(aes(x = spec_rank, y = n, color = mod_sig)) +
          geom_segment(aes(y = 0, yend = n, x = spec_rank, xend = spec_rank), show.legend = F) +
          geom_point(size = .75, shape = 19, show.legend = F) + 
         # scale_x_continuous(limits = my_limits) +
          scale_y_continuous(expression(italic(N))) +
          scale_color_manual(values = pval_colors) +
          scale_alpha_manual(values = my_alphas) +
          facet_wrap(~dv, ncol = 5) +
          ggtitle("Sample Sizes") +
          theme(
            strip.text   = element_blank(),
            axis.text.y  = element_text(angle = 0),
            axis.title.y = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines"))
          )})
    
    spec_grid <- 
      map(c("EFA - daily", "EFA - routine", "EFA - spatial", "EFA - clutter", "EFA - social", "Perc. + QUIC", "CHAOS", "Obj. unpredictability"), function(x) {
        spec_grid_data %>% 
          filter(spec_iv_type == x) %>%
          ggplot(aes(x = spec_rank, y = spec_value, color = mod_sig)) +
          geom_point(size = 4, shape = 73, show.legend = F) +
          geom_text(
            data = spec_grid_data %>% 
              group_by(mod_term_label, spec_var, spec_value, dv) %>% 
              summarize(
                n_sig    = sum(mod_p.value < .05),
                prop_sig = (sum(mod_p.value < .05)/n()),
                prop_sig = ifelse(prop_sig %in% c(0,1), NA, round(prop_sig,2) %>% paste0() %>% str_remove("^0")),
              ) %>%
              group_by(dv) %>%
              mutate(prop_sig = ifelse(is.na(prop_sig) & !all(is.na(prop_sig)), 0, prop_sig)) %>%
              ungroup(),
            aes(x = 64, y = spec_value, label = prop_sig), 
            size = 2, 
            nudge_x = 4,
            show.legend = F,
            inherit.aes = F
          ) +
          geom_vline(aes(xintercept = 64), show.legend = F) +
          geom_segment(aes(y = 1, yend = 1, x = 64, xend = 63), inherit.aes = F, show.legend = F) +
          geom_segment(aes(y = 2, yend = 2, x = 64, xend = 63), inherit.aes = F, show.legend = F) +
         # scale_x_continuous("",limits = my_limits) +
          scale_y_discrete() +
          scale_color_manual(values = pval_colors) +
          scale_alpha_manual(values = my_alphas) +
          facet_grid(spec_var~dv, scales = "free") +
          ggtitle("Specifications") +
          theme(
            strip.text      = element_blank(),
            panel.spacing.y = unit(0.1,"lines"), 
            axis.text.y     = element_text(angle = 0, hjust = 1, vjust = .5, size = rel(.95)),
            axis.title.y    = element_blank(),
            axis.line.x     = element_line(),
            axis.text.x     = element_text(size = rel(.75)),
            axis.ticks.x    = element_line()#,
            # axis.title.x    = element_text() 
          )})
        
    p_curve <- 
      map(c("EFA - daily", "EFA - routine", "EFA - spatial", "EFA - clutter", "EFA - social", "Perc. + QUIC", "CHAOS", "Obj. unpredictability"), function(x) {
        effs %>% 
          filter(spec_iv_type == x) %>%
      ggplot(aes(x = mod_p.value)) +
      geom_histogram(color = "black", size = .2) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      geom_text(
        data = effs %>% filter(spec_iv_type == x) %>% group_by(dv) %>% summarize(p = unique(pval_prop), y = 30) %>% ungroup(),
        aes(x = .3, label = p, y = y),
        size = 2.25,
        hjust = 0,
        vjust = 1,
        show.legend = F,
        inherit.aes = F,
      ) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      scale_y_continuous("Freq", expand = c(0,.15)) +
      scale_alpha_manual(values = my_alphas) +
      facet_wrap(~dv, ncol = 5) +
      ggtitle("P-Curve") +
      theme(
        axis.line.x     = element_line(),
        axis.text.x     = element_text(size = rel(.75)),
        axis.ticks.x    = element_line(),
        axis.title.x    = element_text(),
        axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )})
    
    list(
      eff_curve    = eff_curve,
      sample_sizes = sample_sizes,
      spec_grid    = spec_grid,
      p_curve      = p_curve
    )
  })

# Put them together -------------------------------------------------------

fig1_daily_unp <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[1]],
      spec_curves_ssp[[1]]$p_curve[[1]],
      spec_curves_ssp[[1]]$sample_sizes[[1]],
      spec_curves_ssp[[1]]$spec_grid[[1]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("EFA - daily", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

fig2_routine_unp <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[2]],
      spec_curves_ssp[[1]]$p_curve[[2]],
      spec_curves_ssp[[1]]$sample_sizes[[2]],
      spec_curves_ssp[[1]]$spec_grid[[2]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("EFA - routine", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

fig3_spatial_unp <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[3]],
      spec_curves_ssp[[1]]$p_curve[[3]],
      spec_curves_ssp[[1]]$sample_sizes[[3]],
      spec_curves_ssp[[1]]$spec_grid[[3]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("EFA - spatial", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

fig4_clutter_unp <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[4]],
      spec_curves_ssp[[1]]$p_curve[[4]],
      spec_curves_ssp[[1]]$sample_sizes[[4]],
      spec_curves_ssp[[1]]$spec_grid[[4]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("EFA - clutter", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

fig5_social_unp <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[5]],
      spec_curves_ssp[[1]]$p_curve[[5]],
      spec_curves_ssp[[1]]$sample_sizes[[5]],
      spec_curves_ssp[[1]]$spec_grid[[5]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("EFA - social", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

fig6_unp_quic <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[6]],
      spec_curves_ssp[[1]]$p_curve[[6]],
      spec_curves_ssp[[1]]$sample_sizes[[6]],
      spec_curves_ssp[[1]]$spec_grid[[6]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("Perceived + QUIC", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")


fig7_chaos <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[7]],
      spec_curves_ssp[[1]]$p_curve[[7]],
      spec_curves_ssp[[1]]$sample_sizes[[7]],
      spec_curves_ssp[[1]]$spec_grid[[7]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("CHAOS", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")


fig8_obj <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_ssp[[1]]$eff_curve[[8]],
      spec_curves_ssp[[1]]$p_curve[[8]],
      spec_curves_ssp[[1]]$sample_sizes[[8]],
      spec_curves_ssp[[1]]$spec_grid[[8]],
      nrow  =4,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.25, .15, .2, .4)
    ) +
      #draw_plot_label(c("(faster)","(slower)"), x = 0.075, y = c(.95, .78), size = 8, vjust = 1, hjust = 0, fontface = "italic") +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) + 
  draw_label("Objective unpredictability", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")


