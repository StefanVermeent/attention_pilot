theme_no_legend <- theme(
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "transparent"),
    legend.text = element_text(color = "transparent"),
    legend.title = element_text(color = "transparent"),
    legend.key = element_rect(fill = "transparent")
)

theme_legend_cowplot <- theme(
  legend.position = c(0.4, 0.80)
)


load(here("data", "1_pilot", "2_cleaned_data.Rdata"))


# Change Detection Task ---------------------------------------------------

supp_fig1a <- cleaned_data %>%
  select(rt_change) %>%
  ggplot() +
  geom_density(aes(rt_change*1000), fill = "#345995") +
  scale_alpha_manual(name = NULL, 
                     values = c(1)) +
  guides(alpha = 'none') +
  scale_fill_manual(
    values = c("#345995"),
    guide = guide_legend(override.aes = list(color = "transparent", fill = "transparent"))
  ) +
  theme_bw() +
  theme_no_legend + 
  labs(
    x = "\nReaction Time (ms)",
    y = "Density\n",
    title = ""
  )

supp_fig1b <- cleaned_data %>%
  select(acc_change) %>%
  ggplot() +
  geom_density(aes(acc_change*100), fill = "#345995") +
  scale_alpha_manual(name = NULL, 
                     values = c(1)) +
  guides(alpha = 'none') +
  scale_fill_manual(
    values = c("#345995"),
    guide = guide_legend(override.aes = list(color = "transparent", fill = "transparent"))
  ) +
  theme_bw() +
  theme_no_legend + 
  labs(
    x = "\nAccuracy (%)",
    y = "Density\n",
    title = ""
  )

supp_fig1 <- cowplot::plot_grid(supp_fig1a, supp_fig1b, labels = "AUTO")



supp_fig2a <- cleaned_data %>%
  select(rt_flanker_congruent, rt_flanker_incongruent) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "rt") %>%
  ggplot() +
  geom_density(aes(rt*1000, group = condition, fill = condition, alpha = condition)) +
  scale_alpha_manual(name = NULL, 
                     values = c(1, 0.5)) +
  guides(alpha = 'none') +
  scale_fill_manual(
    values = c("#345995", "#AD343E"),
    guide = guide_legend(override.aes = list(color = "transparent", fill = "transparent"))
  ) +
  theme_bw() +
  theme_no_legend + 
  labs(
    x = "\nReaction Time (ms)",
    y = "Density\n",
    title = ""
  )
    
supp_fig2b <- cleaned_data %>%
  select(acc_flanker_congruent, acc_flanker_incongruent) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "acc") %>%
  ggplot(aes(acc*100, group = condition, fill = condition, alpha = condition)) +
  geom_density() +
  scale_alpha_manual(name = NULL, 
                     values = c(1, 0.5)) +
  guides(alpha = 'none') +
  scale_fill_manual(values = c("#345995", "#AD343E"),
                    labels = c("Congruent", "Incongruent")) +
  theme_bw() +
  theme_legend_cowplot +
  labs(
    x = "\nAccuracy (%)",
    y = "Density\n",
    title = ""
  )

supp_fig2 <- cowplot::plot_grid(supp_fig2a, supp_fig2b, labels = "AUTO")



supp_fig3a <- cleaned_data %>%
  select(rt_cueing_cued, rt_cueing_neutral) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "rt") %>%
  ggplot() +
  geom_density(aes(rt*1000, group = condition, fill = condition, alpha = condition)) +
  scale_alpha_manual(name = NULL, 
                     values = c(1, 0.5)) +
  guides(alpha = 'none') +
  scale_fill_manual(
    values = c("#345995", "#AD343E"),
    guide = guide_legend(override.aes = list(color = "transparent", fill = "transparent"))
  ) +
  theme_bw() +
  theme_no_legend +
  labs(
    x = "\nReaction Time (ms)",
    y = "Density\n",
    title = ""
  )

supp_fig3b <- cleaned_data %>%
  select(acc_cueing_cued, acc_cueing_neutral) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "acc") %>%
  ggplot() +
  geom_density(aes(acc*100, group = condition, fill = condition, alpha = condition)) +
  scale_alpha_manual(name = NULL, 
                     values = c(1, 0.5)) +
  guides(alpha = 'none') +
  scale_fill_manual(values = c("#345995", "#AD343E"),
                    labels = c("Neutral", "Cued")) +
  theme_bw() +
  theme_legend_cowplot +
  labs(
    x = "\nAccuracy (%)",
    y = "Density\n",
    title = ""
  )

supp_fig3 <- cowplot::plot_grid(supp_fig3a, supp_fig3b, labels = "AUTO")


save(supp_fig1, supp_fig2, supp_fig3,
     file = here("data", "1_pilot", "plots", "supplemental_plots.RData"))
