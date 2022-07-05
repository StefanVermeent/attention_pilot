library(tidyverse)
library(cowplot)
library(magrittr)
library(here)
library(flextable)

load(here("data", "2_study1", "primary_vio_pooled_results.Rdata"))


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
my_limits <- c(1,133)

primary_ssp_effects_pooled %<>%
  filter(!dv %in% c("Shrinking rate (rd)", "Initial width (sda)")) %>% 
  mutate(dv = str_replace_all(dv, "\\s\\(", "\n\\(")) %>%
  mutate(dv = factor(dv, levels = c("Perceptual input\n(p)", "sda / rd\n(interference)", 
                                    "Non-decision time\n(t0)", "Boundary separation\n(a)")))




# specification curve -----------------------------------------------------
spec_curves_vio_pooled <- 
  map(c("DDM"), function(x){
    
    # Setup
    dv_which <- x
    
    effs <- 
      primary_ssp_effects_pooled %>% 
      filter(mod_term_group == "Main Effect", mod_term_label == "Violence exposure") %>% 
      mutate(
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    regression_lines <- 
      primary_ssp_effects_pooled %>% 
      filter(mod_term_group %in% c("Main Effect", "Intercept"), mod_term_label %in% c("Intercept", "Violence exposure")) %>% 
      select(mod_term_group, mod_std_coefficient, spec_number, dv) %>%
      pivot_wider(names_from = 'mod_term_group', values_from = 'mod_std_coefficient') %>%
      left_join(
        primary_ssp_effects_pooled %>%
          select(spec_number, mod_sig, mod_term_group, mod_term_label) %>%
          filter(mod_term_group == "Main Effect", mod_term_label == "Violence exposure")
      ) %>%
      mutate(
        #  dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    medians <- 
      primary_ssp_effects_pooled %>% 
      filter(mod_term_group == "Main Effect", mod_term_label == "Violence exposure") %>% 
      mutate(
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    spec_grid_data <- 
      effs %>% 
      select(mod_term_label, dv, starts_with("spec"), mod_p.value, mod_sig) %>% 
      mutate(
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)) %>% 
      pivot_longer(cols = spec_exit_fullscreen:spec_outliers, names_to = "spec_var", values_to = "spec_value") %>% 
      ungroup()
    
    
    # Plots
    eff_curve <- 
      effs %>% 
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
        data = medians,
        aes(y = median_dbl, x = 32),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = medians,
        aes(y = median_dbl, label = median_chr, x = 32),
        nudge_y = .055,
        size = 2.5,
        show.legend = F,
        inherit.aes = F
      ) +
      scale_x_continuous("Specification Rank") + 
      scale_y_continuous(expression(beta)) +
      scale_color_manual(values = pval_colors) +
      scale_fill_manual(values = pval_colors) +
      scale_alpha_manual(values = my_alphas) +
      facet_wrap(~dv, ncol = 4) +
      #  ggtitle("Effect Size Curve") +
      theme(
        strip.text   = element_text(size = rel(1), hjust = 0.5, face = "bold", margin = margin(0,0,1,0,"lines")),
        axis.title.y = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines"))
      )
    
    sample_sizes <- 
      effs %>% 
      ggplot(aes(x = spec_rank, y = (n/2), color = mod_sig)) +
      geom_segment(aes(y = 0, yend = n/2, x = spec_rank, xend = spec_rank), show.legend = F) +
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
      )
    
    
    main_effects <- 
      regression_lines %>%
      group_by(dv) %>%
      mutate(median = median(`Main Effect`)) %>%
      ggplot() +
      coord_cartesian(ylim = c(-0.85, 1), xlim = c(-1.5, 5)) +
      geom_segment(aes(x = -1.030932, xend = 4.624944, y = `Main Effect` * -1.030932, yend = `Main Effect` * 4.624944, color = mod_sig)) +
      geom_segment(aes(x = -1.030932, xend = 4.624944, y = median * -1.030932, yend = median * 4.624944), color = "black", size = 1.5) +
      geom_point(aes(x = -1.030932, y = median * -1.030932), size = 2, fill = "white", color = "black") +
      geom_point(aes(x = 4.624944, y = median * 4.624944), size = 2, fill = "white", color = "black") +
      scale_color_manual(values = pval_colors) +
      scale_alpha_manual(values = c(.1,.8)) +
      facet_wrap(~dv, scales = "free_y", ncol = 4) +
      guides(alpha = "none", color = "none") +
      labs(x = "Violence Exposure",
           y = "") +
      theme(
        axis.line.y          = element_line(),
        axis.text.y          = element_text(),
        axis.ticks.y         = element_line(),
        axis.line.x          = element_line(),
        axis.ticks.x         = element_line(),
        axis.title.y         = element_blank(),
        axis.text.x          = element_text(size = rel(.75)),
        legend.background    = element_blank(), 
        legend.direction     = "vertical",
        legend.key.width     = unit(.5,"lines"),
        legend.key.height    = unit(.5,"lines"),
        legend.key.size      = unit(.5,units = "points"), 
        legend.text          = element_text(size = rel(.6),margin = margin(0,0,0,0)),
        legend.margin        = margin(0,0,0,0),
        legend.position      = c(.25,.2),
        legend.title.align   = .5,
        legend.title         = element_text(size = rel(.75),margin = margin(0,0,0,0)),
        strip.text           = element_text(size = rel(1), hjust = 0.5, face = "bold", margin = margin(0,0,1,0,"lines")) 
      ) 
    
    spec_grid <- 
      spec_grid_data %>% 
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
        aes(x = 130, y = spec_value, label = prop_sig), 
        size = 2, 
        nudge_x = 4,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_vline(aes(xintercept = 130), show.legend = F) +
      geom_segment(aes(y = 1, yend = 1, x = 130, xend = 129), inherit.aes = F, show.legend = F) +
      geom_segment(aes(y = 2, yend = 2, x = 130, xend = 129), inherit.aes = F, show.legend = F) +
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
      )
    
    p_curve <- 
      effs %>% 
      ggplot(aes(x = mod_p.value)) +
      geom_histogram(color = "black", size = .2) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      geom_text(
        data = effs %>% group_by(dv) %>% summarize(p = unique(pval_prop), y = 30) %>% ungroup(),
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
      )
    
    
    
    list(
      eff_curve    = eff_curve,
      sample_sizes = sample_sizes,
      spec_grid    = spec_grid,
      p_curve      = p_curve,
      main_effects = main_effects
    )
  })

# Put them together -------------------------------------------------------
primary_vio_pooled_fig <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_vio_pooled[[1]]$main_effects,
      spec_curves_vio_pooled[[1]]$eff_curve,
      spec_curves_vio_pooled[[1]]$p_curve,
#      spec_curves_vio_pooled[[1]]$sample_sizes,
    #  spec_curves_vio_pooled[[1]]$spec_grid,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5, .25, .25)
    ) #+
    #  draw_plot_label(c("a","b","c"), x = 1, y = c(.95, .75, .6), size = 10, vjust = 1, hjust = 1), 
   # x = 0, y = 0, width = 1, height = .95
  ) #+ 
#  draw_label("Flanker: Violence exposure X condition interaction on raw scores", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

save(primary_vio_pooled_fig, file = here("data", "2_study1", "primary_vio_pooled_fig.RData"))
