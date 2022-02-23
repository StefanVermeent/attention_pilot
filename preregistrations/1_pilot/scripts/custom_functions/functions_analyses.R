standardize_parameters <- function(model) {
  
  if(!class(model) %in% c("lmerModLmerTest", "lm")) {
    
    warning("This custom function can only handle objects of class 'lm' or 'lmer'")
  }
  
  if(class(model) == "lm") {
    data_std = model$model %>% 
      as_tibble() %>%
      mutate(across(everything(), scale))
    
    refit <- lm(formula(model), data = data_std) %>%
      broom::tidy()
  }
  
  
  if(class(model) == "lmerModLmerTest") {
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
