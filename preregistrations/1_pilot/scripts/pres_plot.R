library(tidyverse)
library(cowplot)
library(magrittr)
library(here)
library(flextable)

load(here("data", "1_pilot", "1_primary_analyses", "change_detection", "3_multiverse_extracted_effects.Rdata"))
load(here("data", "1_pilot", "1_primary_analyses", "flanker", "3_multiverse_extracted_effects.Rdata"))
load(here("data", "1_pilot", "1_primary_analyses", "attention_cueing", "3_multiverse_extracted_effects.Rdata"))

# ggplot2 theme -----------------------------------------------------------
theme_set(
  theme_bw() +
    theme(
      axis.line       = element_line(),
      axis.text       = element_text(size = rel(1)),
      axis.title      = element_text(size = rel(1.2), margin = margin(1,0,0,0,"lines")),
      axis.ticks      = element_line(),
   #   axis.line.x       = element_blank(),
   #   axis.text.x       = element_blank(),
   #   axis.ticks.x      = element_blank(),
     # axis.title.x      = element_blank(),
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
my_limits <- c(1,80)

primary_effects <- 
  bind_rows(
    primary_effects_change %>% mutate(task = "change"),
    primary_effects_cueing %>% mutate(task = "cueing"),
    primary_effects_flanker %>% mutate(task = "flanker")
  ) %>%
  filter(dv %in% c("Drift rate (v)", "Initial width (sda)", "Perceptual input (p)", "Shrinking rate (rd)", "sda / rd (interference)")) %>%
  mutate(
    task = case_when(
      task == "flanker" & dv == "Perceptual input (p)" ~ "flanker_p",
      task == "flanker" & dv == "Initial width (sda)" ~ "flanker_sda",
      task == "flanker" & dv == "Shrinking rate (rd)" ~ "flanker_rd",
      task == "flanker" & dv == "sda / rd (interference)" ~ "flanker_int",
      TRUE ~ task
    ),
    dv = case_when(
      task == "change" ~ "Change Detection\nDrift rate (v)",
      task == "cueing" ~ "Attention Cueing\nDrift rate (v)",
      task == "flanker_p" ~ "Flanker\nPercep. input (p)",
      task == "flanker_sda" ~ "Flanker\nInitial width (sda)",
      task == "flanker_rd" ~ "Flanker\nShrinking rate (rd)",
      task == "flanker_int" ~ "Flanker\nInterference"
    )
    
  ) %>%
  mutate(
    dv = factor(dv, levels = c("Change Detection\nDrift rate (v)", "Attention Cueing\nDrift rate (v)",
                               "Flanker\nPercep. input (p)",
                               "Flanker\nInitial width (sda)",
                               "Flanker\nShrinking rate (rd)",
                               "Flanker\nInterference"))
  )




spec_curves <- 
  map(c("cueing", "change", "flanker_p", "flanker_int"), function(x){
    
    
    # Setup
    dv_which <- x
    term = ifelse(x == "cueing", "Interaction", "Main Effect")
    median_point = ifelse(x %in% c("flanker_p", "flanker_sda", "flanker_rd"), 32, 64)
    
    
    effs <- 
      primary_effects %>% 
      filter(task == x, mod_term_group == term) %>% 
      mutate(
        #  dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
        mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
      )
    
    if(x != "cueing") {
      regression_lines <- 
        primary_effects %>% 
        filter(task == x, mod_term_group %in% c("Main Effect", "Intercept")) %>% 
        select(mod_term_group, mod_std_coefficient, spec_number, dv) %>%
        pivot_wider(names_from = 'mod_term_group', values_from = 'mod_std_coefficient') %>%
        left_join(
          primary_effects %>%
            select(spec_number, task, mod_sig, mod_term_group) %>%
            filter(task == x, mod_term_group == "Main Effect")
        ) %>%
        mutate(
          #  dv = factor(dv, levels = c("Drift rate (v)", "Boundary separation (a)", "Non-decision time (t0)")),
          mod_sig = ifelse(mod_sig != "non", "pos-sig", mod_sig)
        )
    }
    
    medians <- 
      primary_effects %>% 
      filter(task == x, mod_term_group == term) %>% 
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
    
    if(x == "cueing") {
      int_points <- 
        primary_effects_points_cueing %>% 
        filter(dv == "Drift rate (v)") %>%
        mutate(dv = factor(dv, levels = c("Drift rate (v)"), labels = "Attention Cueing\nDrift rate (v)")) %>%
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
        scale_x_continuous(" ", breaks = c(1,2), labels = c("Uncued","Cued"), expand = c(.1,.1)) +
        scale_color_manual(values = pval_colors) +
        scale_fill_manual("Violence Exposure:", labels = c("Low", "High"),values = c("white","gray60")) +
        scale_alpha_manual(values = c(.1,.8)) +
        facet_wrap(~dv, scales = "free_y") +
        guides(alpha = "none", color = "none") +
        theme(
        #  axis.line.y          = element_line(),
        #  axis.text.y          = element_text(),
        #  axis.ticks.y         = element_line(),
        #  axis.line.x          = element_line(),
        #  axis.ticks.x         = element_line(),
        #  axis.text.x          = element_text(size = rel(.75)),
          legend.background    = element_blank(), 
          legend.direction     = "vertical",
          legend.key.width     = unit(.5,"lines"),
          legend.key.height    = unit(.5,"lines"),
          legend.key.size      = unit(.5,units = "points"), 
          legend.text          = element_text(size = rel(.6),margin = margin(0,0,0,0)),
          legend.margin        = margin(0,0,0,0),
          legend.position      = c(.8,.15),
          legend.title.align   = .5,
          legend.title         = element_text(size = rel(.75),margin = margin(0,0,0,0)),
          strip.text           = element_text(size = rel(1), hjust = 0.5, face = "bold", margin = margin(0,0,1,0,"lines")) 
        ) 
    }
    
    if(x != "cueing") {
      # Plots
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
        facet_wrap(~dv, scales = "free_y") +
        guides(alpha = "none", color = "none") +
        labs(x = "Violence Exposure",
             y = "") +
        theme(
       #  axis.line.y          = element_line(),
       #  axis.text.y          = element_text(),
       #  axis.ticks.y         = element_line(),
       #  axis.line.x          = element_line(),
       #  axis.ticks.x         = element_line(),
       #  axis.title.y         = element_blank(),
       #  axis.text.x          = element_text(size = rel(.75)),
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
    }
    

    
    nudge = if(x %in% c("flanker_p", "flanker_sda", "flanker_rd")) {
      0.055
    } else {
      if(x == "cueing") {
        0.01375
      } else {
        0.02525
      }
    }
      
      
    
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
        aes(y = median_dbl, x = median_point),
        shape = 21,
        size  = 2.5,
        fill  = "white",
        stroke = 1,
        show.legend = F,
        inherit.aes = F
      ) +
      geom_label(
        data = medians,
        aes(y = median_dbl, label = median_chr, x = median_point),
        nudge_y = nudge,
        size = 2.5,
        show.legend = F,
        inherit.aes = F
      ) +
      #  scale_x_continuous("Specification Rank", limits = my_limits) + 
      scale_y_continuous(expression(beta)) +
      scale_color_manual(values = pval_colors) +
      scale_fill_manual(values = pval_colors) +
      scale_alpha_manual(values = my_alphas) +
      facet_wrap(~dv) +
      ggtitle("Effect Size Curve") +
      theme(
        strip.text = element_blank(),
        # strip.text   = element_text(size = rel(1), hjust = 0.5, face = "bold", margin = margin(0,0,1,0,"lines")),
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
      labs(x="") +
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
        aes(x = 131, y = spec_value, label = prop_sig), 
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
        axis.ticks.x    = element_line(),
         axis.title.x    = element_blank() 
      )
    
    p_curve <- 
      effs %>% 
      ggplot(aes(x = mod_p.value)) +
      geom_histogram(color = "black", size = .2) +
      geom_vline(aes(xintercept = .05), linetype = "dashed") +
      coord_cartesian(ylim = c(0, 20)) +
      geom_text(
        data = effs %>% group_by(dv) %>% summarize(p = unique(pval_prop), y = 15) %>% ungroup(),
        aes(x = .06, label = p, y = y),
        size = 3,
        hjust = 0,
        vjust = 1,
        show.legend = F,
        inherit.aes = F,
      ) +
      scale_x_continuous(expression(italic(p),"-",value), expand = c(0.05,.05)) +
      scale_y_continuous("Freq", expand = c(0,.15)) +
      scale_alpha_manual(values = my_alphas) +
      facet_wrap(~dv) +
     # ggtitle("P-Curve") +
      theme(
      #  axis.line.x     = element_line(),
      #  axis.text.x     = element_text(size = rel(.75)),
      #  axis.ticks.x    = element_line(),
      #  axis.title.x    = element_text(),
      #  axis.title.y    = element_text(angle = 0, vjust = .5,margin = margin(0,0.25,0,0, "lines")),
        legend.position = "none",
        strip.text      = element_blank()
      )
    
    if(x == "cueing") {
      return(
        list(
          main_effects = int_plots,
          eff_curve    = eff_curve,
          sample_sizes = sample_sizes,
          spec_grid    = spec_grid,
          p_curve      = p_curve
        ))
    }
    
    if(x != "cueing") {
      return(
        list(
          main_effects = main_effects,
          eff_curve    = eff_curve,
          sample_sizes = sample_sizes,
          spec_grid    = spec_grid,
          p_curve      = p_curve
        )
      )}
  })

fig_cueing <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves[[1]]$main_effects,
      spec_curves[[1]]$p_curve,
      spec_curves[[1]]$sample_sizes,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5,.25,.25)
    )
  )

fig_change <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves[[2]]$main_effects,
      spec_curves[[2]]$p_curve,
      spec_curves[[2]]$sample_sizes,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5, .25, .25)
    )
  )

fig_flanker_p <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves[[3]]$main_effects,
     # spec_curves[[3]]$eff_curve,
      spec_curves[[3]]$p_curve,
     # spec_curves[[3]]$sample_sizes,
      spec_curves[[3]]$spec_grid,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5, .25, .25)
    )
  )

fig_flanker_sda <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves[[4]]$main_effects,
      spec_curves[[4]]$p_curve,
      spec_curves[[4]]$sample_sizes,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5, .25, .25)
    ) 
  )

fig_flanker_rd <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves[[5]]$main_effects,
      spec_curves[[5]]$p_curve,
      spec_curves[[5]]$sample_sizes,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5, .25, .25)
    ) 
  )

fig_flanker_int <- 
  ggdraw() +
  draw_plot(
    plot_grid(
      spec_curves[[4]]$main_effects,
   #   spec_curves[[4]]$eff_curve,
      spec_curves[[4]]$p_curve,
      spec_curves[[4]]$sample_sizes,
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.5, .25, .25)
    ) 
  )


pres_plot <- ggdraw() +
  draw_plot(
    plot_grid(
      fig_cueing,
      fig_change,
      fig_flanker_p,
    #  fig_flanker_sda,
     # fig_flanker_rd,
      fig_flanker_int,
      nrow = 1,
      ncol = 4,
      align = "h",
      axis = "lr",
      rel_widths = c(.25,.25,.25,.25)
    )
  )

ggsave(pres_plot, file = "presplot.png", width = 14, height = 6)
