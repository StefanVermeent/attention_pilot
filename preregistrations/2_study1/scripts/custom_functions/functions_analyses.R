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
  
  
  names(stats_object) %>%
    str_subset(".*subj.*") %>%
    map_df(function(x){
      
      tibble(
        id   = x,
        mean =  stats_object[[x]]$mean,
        sd   =  stats_object[[x]]$sd,
        mc_error = stats_object[[x]]$`mc error`,
        quantile25 = stats_object[[x]]$quantiles$`25`,
        quantile50 = stats_object[[x]]$quantiles$`50`,
        quantile75 = stats_object[[x]]$quantiles$`75`,
        hpd_95 =  stats_object[[x]]$`95% HPD interval`,
      )
    }) %>%
    separate(id, into = c("parameter", "id", "rm"), sep = "\\.") %>%
    select(-rm) %>%
    mutate(parameter = str_replace_all(parameter, "_subj", ""))
}


parse_hddm_traces <- function(stats_object, parms = c('v', 'v_std', 'a', 'a_std', 't', 't_std')){

    parms %>%
      map(function(x) {
        py_run_string(paste0("trace = ", stats_object, ".trace('", x, "')[:]"))
        
        
          py$trace %>%
          as_tibble() %>%
          mutate(parm = x)
        
      })
  }
  

