standardize_parameters <- function(model) {
  
  if(!class(model) %in% c("lmerModLmerTest", "rlmerMod", "lm")) {
    
    warning("This custom function can only handle objects of class 'lm' or 'lmer'")
  }
  
  if(class(model) == "lm") {
    data_std = model$model %>% 
      as_tibble() %>%
      mutate(across(everything(), scale))
    
    refit <- lm(formula(model), data = data_std) %>%
      broom::tidy()
  }
  
  
  if(class(model) %in% c("lmerModLmerTest", "rlmerMod")) {
    data_std = model@frame %>%
      as_tibble() %>%
      mutate(across(!id, ~scale(.) %>% as.numeric))
    
    refit <- lmer(formula(model), data = data_std) %>%
      broom.mixed::tidy()
   

  }
  
  refit <- refit %>%
    filter(!str_detect(term, "^sd__")) %>%
    rename(
      Parameter = term,
      Std_Coefficient = estimate
    ) %>%
    mutate(
      CI = 0.95,
      CI_low = Std_Coefficient - 1.96 * std.error,
      CI_high = Std_Coefficient + 1.96 * std.error
    ) %>%
    select(Parameter, Std_Coefficient, CI, CI_low, CI_high)
  
  return(refit)
}


parse_hddm_stats <- function(stats_object) {
  
  stats_object %>%
    mutate(id = stats_object %>% rownames) %>%
    as_tibble() %>%
    unnest(everything()) %>%
    rename(
      sd = std,
      mc_error = `mc err`,
      quantile25 = `25q`,
      quantile50 = `50q`,
      quantile75 = `75q`
    ) %>%
    select(id, mean, sd, mc_error, starts_with("quantile")) %>%
    filter(str_detect(id, ".*subj.*")) %>% 
    separate(id, into = c("parameter", "id"), sep = "(\\.)") %>%
    mutate(
      parameter = str_replace_all(parameter, "_subj", ""),
      parameter = str_replace_all(parameter, "\\(", "_"),
      parameter = str_replace_all(parameter, "\\)", "")
    ) %>%
    arrange(id)
}


parse_hddm_traces <- function(stats_object, parms = c('v', 'v_std', 'a', 'a_std', 't', 't_std')){

    parms %>%
      map_dfr(function(x) {
        trace <- stats_object[[x]][[1]] %>%
          as_tibble() %>%
          mutate(parm = x)
      })
  }
  

