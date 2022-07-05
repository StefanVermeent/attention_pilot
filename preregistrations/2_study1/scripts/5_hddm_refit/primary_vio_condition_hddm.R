library(tidyverse)


read_csv("data/2_study1/hddm_gelrub_free_std.csv") %>% summarise(max = max(`1`))

load("data/2_study1/2_cleaned_data.Rdata")


hddm_data <- list.files("data/2_study1", pattern = "hddm.*mod", full.names = T) %>%
  map_dfr(function(x) {
    
    read_csv(x) %>%
      rename(id = `...1`) %>%
      filter(str_detect(id, "subj")) %>%
      separate(id, into = c("parm", "id"), sep = "_") %>%
      separate(id, into = c("condition", "id"), sep = "\\.") %>%
      mutate(condition = str_replace_all(condition, "subj|\\(|\\)", "")) %>%
      unite("parm", c(parm, condition), sep = "_") %>%
      mutate(parm = str_replace_all(parm, "_$", "")) %>%
      select(parm, id, mean) %>%
      mutate(source = x)
  }) %>%
  group_by(parm, id) %>%
  summarise(value = mean(mean, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = "parm", values_from = "value", names_prefix = "hddm_") %>%
  mutate(id = as.numeric(id))


cleaned_data %>%
  select(id, matches("^(p|a|t0|rd|sda|interference)_flanker_std")) %>%
  rename_with(.cols = matches("flanker"), ~str_replace_all(., "_flanker_std", "")) %>%
  rename(
    ssp_a = a,
    ssp_t0 = t0,
    ssp_p = p,
    ssp_rd = rd,
    ssp_sda = sda,
    ssp_interference = interference
  ) %>%
  left_join(hddm_data) %>%
  select(-id) %>%
  cor() %>%
  corrplot::corrplot(method = "number")
  

