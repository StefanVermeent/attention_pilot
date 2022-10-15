library(tidyverse)
library(cowplot)
library(magrittr)
library(here)
library(flextable)

load(here("data", "1_pilot", "1_primary_analyses", "flanker", "3_multiverse_extracted_effects.Rdata"))


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

primary_effects_flanker %<>%
  filter(!dv %in% c("*M*~Congr~ - *M*~Incongr~", "Shrinking rate (rd)")) %>% 
  mutate(dv = str_replace_all(dv, "\\s\\(", "\n\\(")) %>%
  mutate(dv = factor(dv, levels = c("RT", "RT\n(log)", "Perceptual input\n(p)", "Boundary separation\n(a)", 
                                    "Non-decision time\n(t0)", "Initial width\n(sda)", "sda / rd\n(interference)")))

# specification curve -----------------------------------------------------
spec_curves_flanker_raw <- 
  map(c("Raw scores"), function(x){
    
    # Setup
    dv_which <- x
    
    effs <- 
      primary_effects_flanker %>% 
      filter(dv_group == x, mod_term_group == "Interaction") %>% 
      mutate(
        #  dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    medians <- 
      primary_effects_flanker %>% 
      filter(dv_group == x, mod_term_group == "Interaction") %>% 
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
    
    int_points <- 
      primary_effects_points_flanker %>% 
      filter(dv_group == dv_which) %>%
      mutate(dv = factor(dv, levels = c("RT", "RT (log)", "Perceptual input (p)", "Boundary separation (a)", 
                                        "Non-decision time (t0)", "Initial attention width (sda)", "Rate of spotlight shrinking (rd)"))) %>%
      mutate(
        mod_sig   = ifelse(mod_sig != "non", "pos-sig", mod_sig),
       # x         = ifelse(iv_order == 3, x *-1, x),
        x         = factor(x, levels = c(-1,1), labels = c("Low","High")),
        x_spec    = paste0(x,"_",spec_number),
        group     = as.numeric(group),
        group_adj = ifelse(x == "Low", group - .1, group + .1),
        group_jit = jitter(group_adj,.05, .05),
        group_bx  = ifelse(group == 1, group_adj - .4, group_adj + .4)
      )
    
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
        nudge_y = .002,
        size = 2.5,
        show.legend = F,
        inherit.aes = F
      ) +
      scale_x_continuous("Specification Rank") + 
      scale_y_continuous(expression(beta)) +
      scale_color_manual(values = pval_colors) +
      scale_fill_manual(values = pval_colors) +
      scale_alpha_manual(values = my_alphas) +
      facet_wrap(~dv) +
       ggtitle("Effect Size Curve") +
      theme(
        strip.text   = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines"))
      )
    
    sample_sizes <- 
      effs %>% 
      ggplot(aes(x = spec_rank, y = n, color = mod_sig)) +
      geom_segment(aes(y = 0, yend = n, x = spec_rank, xend = spec_rank), show.legend = F) +
      geom_point(size = .75, shape = 19, show.legend = F) + 
    #  scale_x_continuous(limits = my_limits) +
      scale_y_continuous(expression(italic(N))) +
      scale_color_manual(values = pval_colors) +
      scale_alpha_manual(values = my_alphas) +
      facet_wrap(~dv) +
      ggtitle("Sample Sizes") +
      theme(
        strip.text   = element_blank(),
        axis.text.y  = element_text(angle = 0),
        axis.title.y = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines"))
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
    #  scale_x_continuous("",limits = my_limits) +
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
      facet_wrap(~dv) +
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
    
    int_plots <- 
      int_points %>% 
      ggplot(aes(x = group_jit, y = predicted, group = x_spec, color = mod_sig), alpha = 0.8) +
      geom_line(size = .5, show.legend = F) +
      geom_point(size = .5, shape = 19, show.legend = F) + 
      stat_summary(
        aes(x = group_adj, y = predicted, group = x), 
        geom = "line", 
        fun = "median", 
        color = "black", 
        alpha = 1,
        size = 1,
        show.legend = F
      ) +
      stat_summary(
        aes(x = group_adj, y = predicted, group = x, fill = x),
        geom = "point", 
        fun = "median", 
        color = "black", 
        shape = 21,
        stroke = 1, 
        alpha = 1,
        size = 2, 
        show.legend = T
      ) +
      geom_boxplot(
        aes(x = group_bx, y = predicted, group = interaction(x,group), fill = x),
        fatten = 1,
        color = "black",
        position = position_identity(), 
        width = .1, 
        inherit.aes = F, 
        show.legend = F
      ) +
      scale_x_continuous(" ", breaks = c(1,2), labels = c("Congruent","Incongruent"), expand = c(.1,.1)) +
      scale_color_manual(values = pval_colors) +
      scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
      scale_alpha_manual(values = c(.1,.8)) +
      facet_wrap(~dv, scales = "free_y") +
      guides(alpha = "none", color = "none") +
      theme(
        axis.line.y          = element_line(),
        axis.text.y          = element_text(),
        axis.ticks.y         = element_line(),
        axis.line.x          = element_line(),
        axis.ticks.x         = element_line(),
        axis.text.x          = element_text(size = rel(.75)),
        legend.background    = element_blank(), 
        legend.direction     = "vertical",
        legend.key.width     = unit(.5,"lines"),
        legend.key.height    = unit(.5,"lines"),
        legend.key.size      = unit(.5,units = "points"), 
        legend.text          = element_text(size = rel(.6),margin = margin(0,0,0,0)),
        legend.margin        = margin(0,0,0,0),
        legend.position      = c(.4,.2),
        legend.title.align   = .5,
        legend.title         = element_text(size = rel(.75),margin = margin(0,0,0,0)),
        strip.text           = element_text(size = rel(1), hjust = 0.5, face = "bold", margin = margin(0,0,1,0,"lines")) 
      ) 
    
    
    list(
      eff_curve    = eff_curve,
      sample_sizes = sample_sizes,
      spec_grid    = spec_grid,
      p_curve      = p_curve,
      int_plots    = int_plots
    )
  })



# specification curve -----------------------------------------------------
spec_curves_flanker_ssp <- 
  map(c("DDM"), function(x){
    
    # Setup
    dv_which <- x
    
    effs <- 
      primary_effects_flanker %>% 
      filter(dv_group == x, mod_term_group == "Main Effect") %>% 
      mutate(
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    medians <- 
      primary_effects_flanker %>% 
      filter(dv_group == x, mod_term_group == "Main Effect") %>% 
      mutate(
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    spec_grid_data <- 
      effs %>% 
      select(mod_term_label, dv, dv_group, starts_with("spec"), mod_p.value, mod_sig) %>% 
      mutate(
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)) %>% 
      pivot_longer(cols = spec_no_resize:spec_outliers, names_to = "spec_var", values_to = "spec_value") %>% 
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
      facet_wrap(~dv, ncol = 5) +
    #  ggtitle("Effect Size Curve") +
      theme(
        strip.text   = element_text(size = rel(1), hjust = 0.5, face = "bold", margin = margin(0,0,1,0,"lines")),
        axis.title.y = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines"))
      )
    
    sample_sizes <- 
      effs %>% 
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
      p_curve      = p_curve
    )
  })

# Put them together -------------------------------------------------------
primary_flanker_fig1 <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_flanker_raw[[1]]$int_plots,
      spec_curves_flanker_raw[[1]]$eff_curve,
      spec_curves_flanker_raw[[1]]$p_curve,
      spec_curves_flanker_raw[[1]]$sample_sizes,
      spec_curves_flanker_raw[[1]]$spec_grid,
      nrow  = 5,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.3,.3,.2,.2)
    ) +
      draw_plot_label(c("a","b","c","d"), x = 1, y = c(.95, .75, .6, .4), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  ) #+ 
#  draw_label("Flanker: Violence exposure X condition interaction on raw scores", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")

primary_flanker_fig2 <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves_flanker_ssp[[1]]$eff_curve,
      spec_curves_flanker_ssp[[1]]$p_curve,
      spec_curves_flanker_ssp[[1]]$sample_sizes,
      spec_curves_ssp[[1]]$spec_grid,
      nrow  =3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.4, .3, .3)
     # rel_heights = c(.25, .15, .2, .4)
    ) +
      draw_plot_label(c("a","b","c"), x = 1, y = c(.95, .6, .3), size = 10, vjust = 1, hjust = 1), 
    x = 0, y = 0, width = 1, height = .95
  )# + 
 # draw_label("Flanker: Violence exposure main effect on DDM parameters", x = 0.6, y = .975, hjust = .5, vjust = 0, fontface = "bold")


flanker_simple_slopes_table <- primary_simple_slopes %>%
  group_by(dv,level) %>%
  summarise(
    median_b = median(beta, na.rm=T),
    percent_p = (sum(p_value < .05)/n()) * 100
  ) %>%
  ungroup() %>%
  split(.$dv) %>%
  map(function(x) {
    x %>%
      pivot_wider(names_from = "level", values_from = c("median_b", "percent_p")) %>%
      select(dv, median_b_congruent, percent_p_congruent, median_b_incongruent, percent_p_incongruent, median_b_low, percent_p_low, median_b_high, percent_p_high)
  }) %>%
  bind_rows %>%
  mutate(across(!dv, ~round(., 2))) %>%
  flextable() %>%
  set_header_labels(
    values = list(
      dv = "",
      median_b_congruent = "b (congr)",
      median_b_incongruent = "b (incongr)",
      median_b_low = "b (low)",
      median_b_high = "b (high)",
      percent_p_congruent = "p (%)",
      percent_p_incongruent = "p (%)",
      percent_p_low = "p (%)",
      percent_p_high = "p (%)"
  ))

save(primary_flanker_fig1, primary_flanker_fig2, flanker_simple_slopes_table, file = here("preregistrations", "1_pilot", "presentations", "primary_figures_flanker.Rdata"))
