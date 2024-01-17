library(tidyverse)
library(here)
library(furrr)
library(parallel)
library(flankr)

load(here("data", "3_study2", "1_SSP_objects.Rdata"))
load(here("data", "3_study2", "2_cleaned_data.Rdata"))
  
  
# Plot model fit ----------------------------------------------------------

cores <- detectCores()- 4

plan('multisession', workers = cores)

suspect_participants <- 
  ssp_results_refit %>% select(matches("^(a|t0|p|rd|sda|bbic|g2)_flanker"), id) %>%
  mutate(fit = pmap(., function(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker, g2_flanker, bbic_flanker, id, condition) {
    
    id_i = id
    
    plotFitSSP(
      modelFit = list(bestParameters = c(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker), g2 = g2_flanker, bBic = bbic_flanker),
      data     = cleaned_data %>% 
                    filter(id == id_i) %>% 
                    select(flanker_data_long) %>% 
                    unnest(flanker_data_long) %>% 
                    mutate(rt = rt/1000) %>%
                    select(subject = id, rt = rt, congruency, accuracy = correct),
      multipleSubjects = F
    )
    
  }))


# Please note that this code takes at least several minutes (but likely longer) to run, depending on the number of cores that you use for parallel processing.  

cores = parallel::detectCores()
plan("multisession", workers = cores-2)

predicted_quantiles <- 
  bind_rows(
    ssp_results_refit %>% select(matches("^(a|t0|p|rd|sda|bbic|g2)_flanker_std"), id) %>% rename_with(.cols = !matches("id"), ~str_replace_all(.x, "_std", "")) %>% mutate(condition = "standard"),
    ssp_results_refit %>% select(matches("^(a|t0|p|rd|sda|bbic|g2)_flanker_deg"), id) %>% rename_with(.cols = !matches("id"), ~str_replace_all(.x, "_deg", "")) %>% mutate(condition = "degraded"),
    ssp_results_refit %>% select(matches("^(a|t0|p|rd|sda|bbic|g2)_flanker_enh"), id) %>% rename_with(.cols = !matches("id"), ~str_replace_all(.x, "_enh", "")) %>% mutate(condition = "enhanced")
  ) %>%
  select(-g2_flanker, -bbic_flanker) %>%
#  filter(id %in% c(73, 99, 176, 304, 340, 483)) %>%
  split(.$id) %>%
  map(function(x) {
    
    x %>%
      mutate(
        sim_data = future_pmap(., function(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker, id, condition) {
          
          simulateSSP(parms = c(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker), nTrials = 50000) %>%
            mutate(id = id) %>%
            group_by(id, congruency) %>%
            summarise(
              prop_acc_pred = sum(accuracy == 1) / n(),
              quan_rt_pred  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25), na.rm = T))
            ) %>%
            ungroup() %>%
            unnest(quan_rt_pred) %>%
            group_by(congruency)  %>%
            mutate(quantile = c("25th Percentile", "50th Percentile", "75th Percentile"), na.rm = TRUE)
        }, .options = furrr_options(seed = TRUE))
      )
  }) 

predicted_quantiles2 <- predicted_quantiles %>%
  bind_rows %>%
  select(sim_data) %>%
  unnest(sim_data)


observed_quantiles <- cleaned_data %>%
  select(flanker_data_long) %>%
  unnest(flanker_data_long) %>%
  group_by(id, congruency) %>%
  summarise(
    prop_acc_obs = sum(correct == 1) / n(),
    quan_rt_obs  = list(quantile(rt, probs = seq(0.25, 0.75, 0.25)))
  ) %>%
  unnest(quan_rt_obs) %>%
  group_by(id, congruency)  %>%
  mutate(
    quantile = c("25th Percentile", "50th Percentile", "75th Percentile"),
    quan_rt_obs  = quan_rt_obs / 1000) 


qq_data <- left_join(predicted_quantiles2, observed_quantiles) %>%
  mutate(
    label_all = id)

# Plot predicted and observed RTs
ssp_fit_rt <- qq_data %>%
  group_by(congruency, condition, quantile) %>%
  mutate(r = str_c("r = ", round(cor(quan_rt_pred, quan_rt_obs, use = "complete.obs"),2))) %>%
  ungroup() %>%
  split(.$condition) %>%
  map(function(x) {
    
    condition_name = unique(x$condition)
    
    x %>%
      ggplot() +
      geom_point(aes(quan_rt_obs, quan_rt_pred)) +
      geom_abline(slope = 1, intercept = 0) +
      facet_grid(congruency~quantile) +
      geom_text(aes(label=r),  x=-Inf, y=Inf, hjust=-0.2, vjust=1.2) +
      theme_classic() +
      labs(
        x = "\nObserved",
        y = "Predicted\n",
        title = unique(condition_name)
      ) %>%
    print
  })


study2_ssp_fit_rt <- ssp_fit_rt %>%
  ggdraw() +
  draw_plot(
    plot_grid(
      ssp_fit_rt[[3]],
      ssp_fit_rt[[2]],
      ssp_fit_rt[[1]],
      nrow  = 3,
      ncol  = 1, 
      align = "v", 
      axis  = "lr",
      rel_heights = c(.333,.333,.333)
    ))


# Plot predicted and observed Accuracy
study2_ssp_fit_acc <- qq_data %>%
  select(-quantile) %>%
  distinct() %>%
  group_by(congruency, condition) %>%
  mutate(r = str_c("r = ", round(cor(prop_acc_pred, prop_acc_obs, use = "complete.obs"),2))) %>%
  ungroup() %>%
ggplot() +
  geom_point(aes(prop_acc_obs, prop_acc_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(ylim = c(0.5, 1)) +
  geom_text(aes(label=r),  x=-Inf, y=Inf, hjust=-0.2, vjust=1.2) +
  facet_grid(congruency~condition) +
  theme_classic() +
  labs(
    x = "\nObserved RTs (s)",
    y = "Predicted RTs (s)\n"
  ) 



# Correlations between parameters -----------------------------------------

ssp_cor_plot <- bind_rows(
  cleaned_data %>%
    select(id, matches("^(t0|p|a|sda|rd|t0|interference)_flanker_(std|enh)"), matches("^rt.*(std|enh)")) %>%
    mutate(
      rt_diff_flanker_std = rt_flanker_congruent_std - rt_flanker_incongruent_std,
      rt_diff_flanker_enh = rt_flanker_congruent_enh - rt_flanker_incongruent_enh) %>%
    mutate(comparison = "standard - enhanced") %>%
    rename_with(~gsub("_enh", "_alt", .x, fixed = T)),
  #  rename_with(~gsub("rt_flanker_congruent", "rt_congruent_flanker", .x, fixed = T)) %>%
  #  rename_with(~gsub("rt_flanker_incongruent", "rt_incongruent_flanker", .x, fixed = T)),
  cleaned_data %>%
    select(id, matches("^(t0|p|a|sda|rd|t0|interference)_flanker_(incongruent|congruent|std|deg)"), matches("^rt.*(std|deg)")) %>%
    mutate(comparison = "standard - degraded")  %>%
    mutate(
      rt_diff_flanker_std = rt_flanker_congruent_std - rt_flanker_incongruent_std,
      rt_diff_flanker_deg = rt_flanker_congruent_deg - rt_flanker_incongruent_deg) %>%
    rename_with(~gsub("_deg", "_alt", .x, fixed = T)),
  #  rename_with(~gsub("rt_flanker_congruent", "rt_congruent_flanker", .x, fixed = T)) %>%
  #  rename_with(~gsub("rt_flanker_incongruent", "rt_incongruent_flanker", .x, fixed = T)),
  cleaned_data %>%
    select(id, matches("^(t0|p|a|sda|rd|t0|interference)_flanker_(incongruent|congruent|deg|enh)"), matches("^rt.*(enh|deg)")) %>%
    mutate(comparison = "enhanced - degraded")  %>%
    mutate(
      rt_diff_flanker_deg = rt_flanker_congruent_deg - rt_flanker_incongruent_deg,
      rt_diff_flanker_enh = rt_flanker_congruent_enh - rt_flanker_incongruent_enh) %>%
    rename_with(~gsub("_enh", "_std", .x, fixed = T)) %>%
    rename_with(~gsub("_deg", "_alt", .x, fixed = T))
  #  rename_with(~gsub("rt_flanker_congruent", "rt_congruent_flanker", .x, fixed = T)) %>%
  #  rename_with(~gsub("rt_flanker_incongruent", "rt_incongruent_flanker", .x, fixed = T)),
) %>%
  pivot_longer(matches("^((a|t0|p|interference||rt_diff)_flanker_(std|alt))"),
               names_to = c("parameter", "task", ".value"),
               names_pattern = "(^[a-z0-9_]*)(_flanker_)(.*)") %>%
  drop_na() %>%
  select(-task) %>%
 # mutate(across(c(std, alt), ~ifelse(parameter == "interference", log(.), .))) %>%
  mutate(parameter = case_when(
    parameter == "rt_diff" ~ "RT Difference Score",
  #  parameter == "sda" ~ "Attentional width",
  #  parameter == "rd" ~ "Shrinking rate",
    parameter == "a" ~ "Boundary Separation",
    parameter == "t0" ~ "Non-Decision Time",
    parameter == "p" ~ "Perceptual Input",
    parameter == "interference" ~ "Interference"
  ),
  parameter = factor(parameter, 
                     levels = c("RT Difference Score", "Perceptual Input", "Interference", "Non-Decision Time", "Boundary Separation"))
  ) %>%
#  mutate(color = ifelse(std > alt, "red", "blue")) %>%
  group_by(parameter, comparison) %>%
  mutate(r = str_c("R = ", round(cor(std, alt, use="complete.obs"), 2))) %>%
  ggplot() +
 # geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgrey") + 
  geom_point(aes(std, alt)) +
  geom_smooth(aes(std, alt), method = "lm", color = "black") +
  geom_text(aes(label=r),  x=-Inf, y=Inf, hjust=-0.2, vjust=1.2) +
  scale_color_discrete(type = c("darkgrey", "black")) +
  facet_wrap(parameter~comparison, scales = "free", ncol = 3) +
  theme_classic() +
  guides(color = "none") +
  labs(
    x = "Standard condition",
    y = "Adapted condition"
  )


save(study2_ssp_fit_rt, study2_ssp_fit_acc, file = here("data", "3_study2", "ssp_fit.RData"))
