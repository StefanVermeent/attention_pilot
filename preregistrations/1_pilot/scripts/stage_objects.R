load(here("data", "1_pilot", "2_cleaned_data.Rdata"))
load(here("data", "1_pilot", "3_primary_analyses_results.Rdata"))
load(here("data", "1_pilot", "3_secondary_analyses_results.Rdata"))



# Functions ---------------------------------------------------------------

tidy_lmer_table <- function(model_tidy, std_effects, iv, iv_rename) {
  
  model_tidy %>%
    left_join(std_effects %>% select(term=Parameter, Std_Coefficient)) %>%
    filter(term != "(Intercept)", effect == "fixed") %>%
    select(term, estimate, Std_Coefficient, std.error, statistic, p.value) %>%
    mutate(
      term = case_when(
        term == str_glue("{iv}") ~ str_glue("{iv_rename}"),
        term == "condition_sum" ~ "Task condition",
        term == str_glue("{iv}:condition_sum") ~ str_glue("{iv_rename}*Condition"),
        term == "event_during_task" ~ "Browser event during task",
        term == "scale_factor" ~ "Task scaling"
      )
    ) %>%
    arrange(factor(term, levels = c(str_glue("{iv_rename}"), "Task condition", str_glue("{iv_rename}*Condition"), "Browser event during task", "Task scaling"))) %>%
    rename(
      IV   = term,
      b    = estimate,
      Beta = Std_Coefficient,
      SE   = std.error,
      t    = statistic,
      p    = p.value,
    ) %>%
    mutate(across(c(b, Beta, SE, t), ~round(.,2))) %>%
    mutate(p = ifelse(p < .001, "< .001", round(p, 3)))
}

tidy_lm_table <- function(model_tidy, std_effects, iv, iv_rename) {
  model_tidy %>%
    left_join(std_effects %>% select(term=Parameter, Std_Coefficient)) %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate, Std_Coefficient, std.error, statistic, p.value) %>%
    mutate(
      term = case_when(
        term == str_glue("{iv}") ~ str_glue("{iv_rename}"),
        term == "event_during_task" ~ "Browser event during task",
        term == "scale_factor" ~ "Task scaling"
      )
    ) %>%
    rename(
      b  = estimate,
      Beta = Std_Coefficient,
      SE = std.error,
      t  = statistic,
      p  = p.value
    ) %>%
    mutate(across(c(b, Beta, SE, t), ~round(.,2))) %>%
    mutate(p = ifelse(p < .001, "< .001", round(p, 3)))
}

# Correlations between IV components --------------------------------------

## Violence ----

cor(cleaned_data$violence_mean, cleaned_data$fighting_mean)

## Unpredictability

unp_cors <- cleaned_data %>%
  select(unp_mean, quic_total_mean, chaos_mean, change_env_mean, unp_partners_mother_binned, unp_partners_father_binned, unp_moving_binned, unp_household_size) %>%
  rename(
    Perceived = unp_mean,
    QUIC = quic_total_mean,
    CHAOS = chaos_mean,
    Environmental_changes = change_env_mean,
    `Partners Mother - Binned` = unp_partners_mother_binned,
    `Partners Father - Binned` = unp_partners_father_binned,
    `Residential Changes - Binned` = unp_moving_binned,
    `Household Size` = unp_household_size) %>%
  corr_table(numbered = T, 
             stats = "",
             c.names = names(.) %>% str_replace("_"," ") %>% str_to_title() %>% str_replace("Ses","SES") %>% str_replace("Agg","Composite") %>% str_replace("Neighborhood","Neigh."),
             change = T) 

cor(cleaned_data$unpredictability_obj, cleaned_data$unpredictability_subj) 



# Covariates --------------------------------------------------------------

covariates <- cleaned_data %>%
  mutate(across(starts_with("event_during"), ~ifelse(. == FALSE, 0, 1))) %>%
  mutate(scale_factor = ifelse(round(scale_factor, 4) != '0.3081', 0, 1)) %>%
  select(rt_change, acc_change, rt_cueing_cued, rt_cueing_neutral, rt_flanker_congruent, rt_flanker_incongruent,
         depression_mean, stai_s_mean, hungry, sleep, rested, 
         starts_with("event_during"), scale_factor, meta_resolution_width,meta_resolution_height) %>%
  mutate(across(everything(), as.numeric)) %>%
 # filter(meta_resolution_width >800) %>%
  mutate(resolution_ratio = meta_resolution_width/meta_resolution_height) %>%
  filter(resolution_ratio >= 1.5) %>%
  rename_with(.cols = starts_with("event_"), ~str_replace_all(., "(.*)_(.*)_(.*)", "\\1 \\2 \\3")) %>%
  rename_with(.cols = starts_with("rt_"), ~str_replace_all(., "rt_", "")) %>%
  rename(`STAI-S` = stai_s_mean) %>%
  corr_table(numbered = T, 
             stats = "",
             c.names = names(.) %>% str_replace("_"," ") %>% str_to_title() %>% str_replace("Ses","SES") %>% str_replace("Agg","Composite") %>% str_replace("Neighborhood","Neigh."),
             change = T
  ) 



# Primary analyses --------------------------------------------------------

## Violence exposure on raw performance

### Change Detection Task ----

primary_raw_vio_change_rt <- primary_vio_raw[[1]]$model_tidy$rt %>%
  tidy_lm_table(model_tidy = ., std_effects = primary_vio_raw[[1]]$model_standardized$rt, iv = "violence_composite_z", iv_rename = "Violence composite")

primary_raw_vio_change_acc <- primary_vio_raw[[1]]$model_tidy$acc %>%
  tidy_lm_table(model_tidy = ., std_effects = primary_vio_raw[[1]]$model_standardized$acc, iv = "violence_composite_z", iv_rename = "Violence composite") 

### Cued Attention Task ----

primary_raw_vio_cueing_rt <- primary_vio_raw[[2]]$model_tidy %>%
  tidy_lmer_table(model_tidy = ., std_effects = primary_vio_raw[[2]]$model_standardized, iv = "violence_composite_z", iv_rename = "Violence composite")

### Flanker Task ----

primary_raw_vio_flanker_rt <- primary_vio_raw[[3]]$model_tidy %>%
  tidy_lmer_table(model_tidy = ., std_effects = primary_vio_raw[[3]]$model_standardized, iv = "violence_composite_z", iv_rename = "Violence composite")


## Unpredictability on raw performance ----

### Change Detection Task ----
secondary_raw_unp_change_rt <- secondary_unp_raw[[1]]$model_tidy$rt %>%
  tidy_lm_table(model_tidy = ., std_effects = secondary_unp_raw[[1]]$model_standardized$rt, iv = "unpredictability_composite_z", iv_rename = "Unpredictability composite")

secondary_raw_unp_change_acc <- secondary_unp_raw[[1]]$model_tidy$acc %>%
  tidy_lm_table(model_tidy = ., std_effects = secondary_unp_raw[[1]]$model_standardized$acc, iv = "unpredictability_composite_z", iv_rename = "Unpredictability composite")

### Cued Attention Task ----
secondary_raw_unp_cueing_rt <- secondary_unp_raw[[2]]$model_tidy %>%
  tidy_lmer_table(model_tidy = ., std_effects = secondary_unp_raw[[2]]$model_standardized, iv = "unpredictability_composite_z", iv_rename = "Unpredictability composite")


### Flanker Task ----
secondary_raw_unp_flanker_rt <- secondary_unp_raw[[3]]$model_tidy %>%
  tidy_lmer_table(model_tidy = ., std_effects = secondary_unp_raw[[3]]$model_standardized, iv = "unpredictability_composite_z", iv_rename = "Unpredictability composite")



## Poverty on raw performance

### Change Detection Task ----
secondary_raw_pov_change_rt <- secondary_pov_raw[[1]]$model_tidy$rt %>%
  tidy_lm_table(model_tidy = ., std_effects = secondary_pov_raw[[1]]$model_standardized$rt, iv = "poverty_composite_z", iv_rename = "Poverty composite")

secondary_raw_pov_change_acc <- secondary_pov_raw[[1]]$model_tidy$acc %>%
  tidy_lm_table(model_tidy = ., std_effects = secondary_pov_raw[[1]]$model_standardized$acc, iv = "poverty_composite_z", iv_rename = "Poverty composite")

### Cued Attention Task ----
secondary_raw_pov_cueing_rt <- secondary_pov_raw[[2]]$model_tidy %>%
  tidy_lmer_table(model_tidy = ., std_effects = secondary_pov_raw[[2]]$model_standardized, iv = "poverty_composite_z", iv_rename = "Poverty composite")


### Flanker Task ----
secondary_raw_pov_flanker_rt <- secondary_pov_raw[[3]]$model_tidy %>%
  tidy_lmer_table(model_tidy = ., std_effects = secondary_pov_raw[[3]]$model_standardized, iv = "poverty_composite_z", iv_rename = "Poverty composite")



# Interaction figures -----------------------------------------------------

fig_primary_vio_cueing_interaction <- primary_vio_raw[[2]]$mod_effects %>%
  rename(violence = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(violence), color = factor(violence))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Neutral", "Cued")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Violence exposure\n",
    x = "\nCondition",
    y = "RT (log)"
  )
  
fig_primary_vio_flanker_interaction <- primary_vio_raw[[3]]$mod_effects %>%
  rename(violence = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(violence), color = factor(violence))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Congruent", "Incongruent")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Violence exposure\n",
    x = "\nCondition",
    y = "RT (log)"
  )


fig_secondary_unp_cueing_interaction <- secondary_unp_raw[[2]]$mod_effects %>%
  rename(unpredictability = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(unpredictability), color = factor(unpredictability))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Neutral", "Cued")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Unpredictability\n",
    x = "\nCondition",
    y = "RT (log)"
  )

fig_secondary_unp_flanker_interaction <- secondary_unp_raw[[3]]$mod_effects %>%
  rename(unpredictability = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(unpredictability), color = factor(unpredictability))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Congruent", "Incongruent")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Unpredictability\n",
    x = "\nCondition",
    y = "RT (log)"
  )



fig_secondary_pov_cueing_interaction <- secondary_pov_raw[[2]]$mod_effects %>%
  rename(poverty = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(poverty), color = factor(poverty))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Neutral", "Cued")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Poverty\n",
    x = "\nCondition",
    y = "RT (log)"
  )

fig_secondary_pov_flanker_interaction <- secondary_pov_raw[[3]]$mod_effects %>%
  rename(poverty = x, condition = group) %>%
  ggplot(aes(condition, predicted, group = factor(poverty), color = factor(poverty))) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(size=1) +
  scale_x_discrete(labels = c("Congruent", "Incongruent")) +
  scale_color_manual(
    values = c("#345995", "#AD343E"),
    labels = c("-1 SD", "+1 SD")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    color = "Poverty\n",
    x = "\nCondition",
    y = "RT (log)"
  )
