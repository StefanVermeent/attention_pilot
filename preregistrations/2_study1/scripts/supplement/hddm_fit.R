load("preregistrations/1_pilot/analysis_objects/hddm_model_objects.Rdata")

pilot_traces_cueing <- cueing_hddm_traces |> 
  mutate(parameter = case_when(
    parameter == "a" ~ "Bound. sep.",
    parameter == "t01" ~ "Non-dec. time - Neutral",
    parameter == "t02" ~ "Non-dec. time - Cued",
    parameter == "v1" ~ "Drift rate - Neutral",
    parameter == "v2" ~ "Drift rate - cued",
  )) |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

pilot_recov_rt_cueing <- cueing_hddm_fit |> 
  mutate(percentile = case_when(
    percentile == "RT_25" ~ "25th percentile",
    percentile == "RT_50" ~ "50th percentile",
    percentile == "RT_75" ~ "75th percentile"
  ))
  ggplot(aes(RT_sim, RT)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(condition~percentile, scales = 'free') +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered"
  )

pilot_recov_acc_cueing <- cueing_hddm_fit |>
  select(id, condition, acc_sim, acc) |> 
  distinct() |> 
  ggplot(aes(acc_sim, acc)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(~condition, scales = 'free') +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered"
  )


pilot_traces_change <- change_hddm_traces |> 
  mutate(parameter = case_when(
    parameter == "a" ~ "Bound. sep.",
    parameter == "t0" ~ "Non-dec. time",
    parameter == "v" ~ "Drift rate"
  )) |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

pilot_recov_rt_change <- change_hddm_fit |> 
  mutate(percentile = case_when(
    percentile == "RT_25" ~ "25th percentile",
    percentile == "RT_50" ~ "50th percentile",
    percentile == "RT_75" ~ "75th percentile"
  )) |> 
  ggplot(aes(RT_sim, RT)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~percentile, scales = 'free') +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered"
  )

pilot_recov_acc_change <- change_hddm_fit |>
  select(id, acc_sim, acc) |> 
  distinct() |> 
  ggplot(aes(acc_sim, acc)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered"
  )

save(pilot_traces_cueing, pilot_recov_rt_cueing, pilot_recov_acc_cueing,
     pilot_traces_change, pilot_recov_rt_change, pilot_recov_acc_change,
     file = "preregistrations/1_pilot/analysis_objects/hddm_recovery_plots.RData")
