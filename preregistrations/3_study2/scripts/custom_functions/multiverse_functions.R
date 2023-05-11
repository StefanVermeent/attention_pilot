multiverse_grid <- function(specs) {
  
  if(!all(c("spec_var", "spec_expr") %in% names(specs))) {
    stop("you don't have the correct variables in the specs dataframe. Make sure specs consists of the variables 'spec_var', 'spec_expr'.\n\n")
  }
  
  # If descriptive labels are not provided, generate automatic labels
  if(!"name" %in% names(specs)) {
    specs <- specs %>%
      mutate(name = str_c(spec_var, spec_value, sep="_"))
  }
  
  specs <- specs %>%
    group_by(spec_var) %>%
    mutate(spec_value = seq(0,n()-1,1))
  
  combinations <- specs %>%
    select(-spec_value) %>%
    group_by(spec_var) %>%
    summarise(n = n()) %>%
    mutate(n = n-1)
  
  varnames = combinations$spec_var
  
  combinations <- combinations %>%
    glue_data("{spec_var} = 0:{n}") %>%
    str_replace_all(" ", "") %>%
    map(function(x) {
      eval(parse(text=x))
    }) %>% 
    expand.grid %>%
    as_tibble() %>%
    rename_all(~varnames)
  
  
  spec_grid <- combinations %>%
    rownames_to_column("spec_number") %>%
    mutate(spec_number = as.numeric(spec_number)) %>%
    pivot_longer(!spec_number, names_to = "spec_var", values_to = "spec_value") %>%
    left_join(specs, by = c("spec_var", "spec_value"))
  
  return(spec_grid)
}



multiverse_apply_grid <- function(data, grid) {
  
  data_list <- 
    grid %>%
    split(.$spec_number) %>%
    map(function(x) {
      
      spec_number <- unique(x$spec_number)
      spec_expressions <- 
        x %>% 
        select(spec_var, spec_expr) %>% 
        pivot_wider(names_from = "spec_var", values_from = "spec_expr")
      
      
      if(exists("dv_type", where = spec_expressions)) {
        dv <- spec_expressions$dv_type
      } else {
        dv <- ""
      }
      
      if(exists("iv_type", where = spec_expressions)) {
        iv <- spec_expressions$iv_type
      } else {
        iv <- ""
      }
      
      
      # Create a list of filtered dataframes
      filtered_data <- spec_expressions %>% select(-contains("dv_type"), -contains("iv_type")) %>%
        map(function(f) {
          dv_type <- dv

          data %>%
            filter(
              eval(parse(text = paste(glue(f))))
            )
        })
 
      
      # Find participants that are excluded under one or more exclusion criteria
      exclusions <- map(filtered_data, function(x) {
        
        all_ids <- data$id %>% unique
        sub_ids <- unique(x$id)
        
        exclude <- all_ids[!all_ids %in% sub_ids]
      }) %>%
        unlist %>% unique
      
      
      # Apply exclusions
      data %<>%
        filter(!id %in% exclusions)
      
      results <- list(
        n    = nrow(data),
        data_analysis = data,
        specifications = x %>% select(-spec_value)
      )
      
      message("dataset ", spec_number, " created")
      results
      
    }) 
}



multiverse_run_lmer <- function(data_list, predictors = c("condition"), levels = list(c(-1, 1), c(0, 1)), covariates = NULL, dv = NULL, random = "(1|id)", parallel = FALSE, cores = 1, ...) {
  
 if(parallel) {
   plan("multisession", workers = cores)
 } else {
   plan("sequential")
 }
  
  results <- 
    data_list %>%
    future_map(function(x){
      
      dv <- ifelse(is.null(dv), x$specifications %>% filter(spec_var == "dv_type") %>% pull(spec_expr), dv)
      
      
      iv <- ifelse("iv_type" %in% x$specifications$spec_var, 
                   x$specifications %>% filter(spec_var == "iv_type") %>% pull(spec_expr), 
                   str_subset(predictors, "_c$"))
      
      if("iv_type" %in% x$specifications$spec_var) {
        predictors <- c(iv, predictors)
      } else {
        predictors <- predictors
      }
      
      
     # iv <- str_subset(predictors, "_c$")
      scale_iv <- iv %>%
        map_chr(function(x) {
          base_iv <- str_replace(x, "_c$", "")
          
          mutate_str <- str_c("scale(", base_iv, ",scale = FALSE) %>% as.numeric()")
        })
      
      
      spec_number <- unique(x$specifications$spec_number)
      data = x$data_analysis %>%
        mutate("{iv}" := eval(parse(text = scale_iv))) 
      
    
      if(!is.null(covariates)) {covariates <- str_c("+ ", covariates)}
      
      # Construct full model
      mod_formula <<- as.formula(paste0(glue("{dv}"), " ~ ", str_c(predictors, collapse = "*"), covariates, " + ", random))
      
      # Fit full model
      mod <- lmer(formula = mod_formula, data = data)
     
      # Tidy model dataframe
      mod_tidy <- broom.mixed::tidy(mod) %>% rename_all(~paste0("mod_",.))
     
      singular <- isSingular(mod)
      #Standardized coefficients
      mod_std <- standardize_parameters(mod)
      
      mod_effect_terms <- 1:length(c(iv, predictors) %>% unique) %>% 
        map_chr(function(x) {
          str_c((c(iv, predictors) %>% unique)[[x]], "[", str_c(levels[[x]], collapse = ","), "]", collapse = "")
        })

      
      # Create a data.frame with predicted effects at high and low predictor value for each task type
      mod_effects <- ggpredict(mod, terms = mod_effect_terms)
      
     
      ## Simple slopes
      
      ss1 <- sim_slopes(mod, pred = !! predictors[1],  modx = !! predictors[2],  modx.values = levels[[2]], data = data)
      ss2 <- sim_slopes(mod, pred = !! predictors[2],  modx = !! predictors[1],  modx.values = levels[[1]], data = data)
      
   #
   # if(robust) {
   #   mod_robust <- rlmer(formula = mod_formula, data = data)
   #   coefs_robust <- coef(summary(mod_robust))
   #   pvalues_robust <- 2*pt(abs(coefs_robust[,3]), coefs$df, lower=FALSE)
   #   
   #   mod_tidy_robust <- broom.mixed::tidy(mod_robust) %>% rename_all(~paste0("mod_",.)) %>%
   #     mutate(mod_p.value = c(pvalues_robust, NA, NA))
   #   
   #   mod_std_robust <- standardize_parameters(mod_robust)
   #   
   #   mod_effects_robust <- ggpredict(mod_robust, terms = mod_effect_terms)
   #   
   #   emmeans(mod_robust, specs = "condition", by = "vio_comp_c")
        
        
   #   }
      
      results <- list(
        n                 = x$n/2,
        n_model           = nrow(mod@frame),
        n_model_obs       = mod@frame %>% distinct(id) %>% nrow,
        data_analysis     = data,
        data_model        = mod@frame,
        specifications    = x$specifications,
        model             = mod,
        model_tidy        = mod_tidy,
        model_std_effects = mod_std,
        model_effects     = mod_effects,
        simple_slopes     = list(task = ss1, unp = ss2),
        singular          = singular
      )
     
      results
      
    }, .options = furrr_options(seed = TRUE))
}


multiverse_run_lm <- function(data_list, predictors = c("vio_comp_c"), covariates = NULL, dv = NULL, parallel = FALSE, cores = 1, ...) {
  
  if(parallel) {
    plan("multisession", workers = cores)
  } else {
    plan("sequential")
  }
  
  results <- 
    data_list %>%
    future_map(function(x){
      
      dv <- ifelse(is.null(dv), x$specifications %>% filter(spec_var == "dv_type") %>% pull(spec_expr), dv)
      
      
      iv <- ifelse("iv_type" %in% x$specifications$spec_var, 
                   x$specifications %>% filter(spec_var == "iv_type") %>% pull(spec_expr), 
                   str_subset(predictors, "_c$"))
      
      if("iv_type" %in% x$specifications$spec_var) {
        predictors <- c(iv, predictors)
      } else {
        predictors <- predictors
      }
      
      
      # iv <- str_subset(predictors, "_c$")
      scale_iv <- iv %>%
        map_chr(function(x) {
          base_iv <- str_replace(x, "_c$", "")
          
          mutate_str <- str_c("scale(", base_iv, ",scale = FALSE) %>% as.numeric()")
        })
      
      
      spec_number <- unique(x$specifications$spec_number)
      data = x$data_analysis %>%
        mutate("{iv}" := eval(parse(text = scale_iv)))  # TODO: Make this dynamic
      
      
      if(!is.null(covariates)) {covariates <- str_c("+ ", covariates)}
      
      # Construct full model
      mod_formula <<- as.formula(paste0(glue("{dv}"), " ~ ", str_c(predictors, collapse = "*"), covariates))
      
      # Fit full model
      mod <- lm(formula = mod_formula, data = data)
      
      
      # Tidy model dataframe
      mod_tidy <- broom::tidy(mod) %>% rename_all(~paste0("mod_",.))
      
      #Standardized coefficients
      mod_std <- standardize_parameters(mod)
      
    
      results <- list(
        n                 = x$n,
        n_model           = nrow(mod$model),
        n_model_obs       = mod$model %>% nrow,
        data_analysis     = data,
        data_model        = mod$model,
        specifications    = x$specifications,
        model             = mod,
        model_tidy        = mod_tidy,
        model_std_effects = mod_std
      )

      results
      
    })
}




multiverse_extract_effects <- function(model_list, grid) {
  
  n_multiverse <- length(model_list)
  

  n_dv <- grid %>%
    filter(spec_var == "dv_type") %>%
    select(spec_expr) %>%
    distinct() %>% 
    pull %>%
    length

  
  
  effects <- 
    model_list %>%
    map_df(function(multiverse){
      
      # All parameters from multiverse
      params <-
        bind_cols(
          n = multiverse$n_model,
          left_join(
            multiverse$model_tidy %>% select(mod_term:mod_p.value), 
            multiverse$model_std_effects %>% rename_with(~paste0("mod_",.x)), 
            by = c("mod_term" = "mod_Parameter")
          ),
          multiverse$specifications %>% 
            select(-spec_expr) %>% 
            pivot_wider(names_from = "spec_var", values_from = "name") %>% 
            rename_with(.cols = !spec_number, ~paste0("spec_",.x))
        )}) %>% 
    rename_with(tolower) %>% 
    filter(!str_detect(mod_term, "^sd__")) %>%
    group_by(mod_term, spec_dv_type) %>% 
    mutate(
      dv         = spec_dv_type,
      spec_rank  = as.numeric(fct_reorder(as_factor(spec_number), mod_std_coefficient)),
      median_dbl = median(mod_std_coefficient),
      median_chr = median_dbl %>% round(2) %>% as.character,
      pval_prop  = (sum(mod_p.value < .05)/(n_multiverse/n_dv)) * 100
    ) %>% 
    ungroup() %>% 
    mutate(
      mod_sig        = case_when(mod_p.value <  .05 & mod_std_coefficient > 0 ~ "pos-sig",
                                 mod_p.value <  .05 & mod_std_coefficient < 0 ~ "neg-sig",
                                 mod_p.value >= .05 ~ "non"),
      mod_term_num   = case_when(str_detect(mod_term, "Intercept")  ~ -1,
                                 !str_detect(mod_term, "Intercept|:") ~ 0,
                                 str_detect(mod_term, ":")          ~ 1
      ),
      #  mod_term_label = case_when(str_detect(mod_term, "Intercept")  ~ "Intercept",
      #                             mod_term == "vio_comp_c" ~ "Violence exposure",
      #                             mod_term == "condition"            ~ "Condition",
      #                             str_detect(mod_term, ":")          ~ "Interaction",
      #  ),
      mod_term_group = factor(mod_term_num,
                              levels = c(-1,0,1), 
                              labels = c("Intercept","Main Effect","Interaction")),
      #  mod_term_unique = case_when(mod_term == "vio_comp_c" ~ "Vio",
      #                              mod_term == "vio_comp_c:condition" ~ "Vio~symbol('\\264')~Task-Condition",
      #                              T ~ mod_term_label),
     # mod_term_fct   = factor(mod_term_label) %>% fct_reorder(mod_term_num),
      pval_prop      = paste0(round(pval_prop,2),"% of ps"," < .05"),
      pval_prop      = fct_reorder(pval_prop, mod_term_num))
}

multiverse_medians <- function(effects) {
  
  medians <- 
   effects %>% 
    select(dv, contains("_term"), contains("median_")) %>% 
    distinct()
}
  


multiverse_interaction_points <- function(multiverse, effects) {
  effects_points <- 
    multiverse %>% 
    map_df(function(m){
      specs <- m$specifications %>% 
        select(-spec_expr) %>% 
        pivot_wider(names_from = "spec_var", values_from = "name") %>% 
        rename_with(.cols = !spec_number, ~paste0("spec_",.x))
      
      bind_cols(m$model_effects, n = m$n, specs) %>% tibble()
    }) %>% 
    left_join(
      effects %>% 
        filter(str_detect(mod_term_label, "Interaction")) %>% 
        select(spec_number, mod_sig)
    ) %>%
    mutate(
      dv         = spec_dv_type
    )
}



multiverse_median_effects_table <- function(effects) {
  
 ## Bootstrapped medians ----------------------------------------------------
 #boot_medians <- 
 #  boot_effects %>% 
 #  filter((null_term == "main" & mod_term == "iv") | (null_term == "int" & mod_term == "iv:type_z")) %>% 
 #  rename(term = mod_term) %>% 
 #  mutate(
 #    across(
 #      .cols = c(mod_estimate, mod_std.error, mod_statistic, mod_std_coefficient, mod_ci, mod_ci_low, mod_ci_high),
 #      .fns = ~ifelse(str_detect(iv,"ses"), .x * -1, .x)
 #    )
 #  ) %>%
 #  group_by(boot_num, iv, dv, null_term) %>% 
 #  summarize(median_null = median(mod_std_coefficient))
 #
  # Primary Analysis Medians ------------------------------------------------
  medians <- 
    effects %>% 
    mutate(
      iv = "Violence exposure",
      dv = spec_dv_type
    ) %>%
    filter(mod_term_label != "Condition") %>%
    select(spec_number,iv,dv,mod_term_group, mod_std_coefficient, mod_ci_high, mod_ci_low, n, mod_p.value) %>% 
    filter(mod_term_group %in% c("Main Effect","Interaction")) %>% 
    group_by(iv, dv, mod_term_group) %>% 
    mutate(p_percent = (sum(mod_p.value < .05)/n())) %>% 
    summarize(across(where(is.numeric), median)) %>% 
    rename(null_term = mod_term_group) %>% 
    transmute(
     # iv          = case_when(iv == "Unpredictability" ~ "unp", iv == "Violence" ~ "vio", iv == "SES" ~ "ses"),
     # dv          = ifelse(dv == "Attention-Shifting","shifting","updating"),
      null_term   = ifelse(null_term == "Main Effect", "main", "int"),
      median_beta = mod_std_coefficient,
      median_lo   = mod_ci_low,
      median_hi   = mod_ci_high,
      median_n    = n,
      p_percent   = p_percent
    )
  
  # Table - Base ------------------------------------------------------------
  table_base <- 
    medians %>%
    #left_join(boot_medians, primary_medians, by = c("iv","dv", "null_term")) %>% 
    arrange(dv, iv) %>%#, boot_num) %>% 
    group_by(iv, dv, null_term) %>% 
   # mutate(
   #   is_sig = if_else(median_beta < 0, median_null < median_beta, median_null > median_beta)
   # ) %>% 
    summarize(
   #   p_overall   = sum(is_sig)/n(),
      p_percent   = unique(p_percent),
      median_beta = unique(median_beta),
      median_lo   = unique(median_lo),
      median_hi   = unique(median_hi),
      median_n    = unique(median_n)
    ) %>% 
    ungroup() %>%
    arrange(dv, iv, desc(null_term)) %>% 
    mutate(
      median_beta = formatC(round(median_beta,2), digits = 2, width = 3, flag = '0', format = 'f'),
      median_n    = formatC(round(median_n), digits = 0, format = 'f'),
      p_percent   = formatC(round(p_percent*100, 2), digits = 2, width = 3, flag = '0', format = 'f') %>% paste0(.,"%"),
     # p_overall   = formatC(round(p_overall,3), digits = 3, width = 4, flag = '0', format = 'f'),
      ci_lo          = formatC(round(median_lo,2), digits = 2, width = 3, flag = '0', format = 'f'),
      ci_hi          = formatC(round(median_hi,2), digits = 2, width = 3, flag = '0', format = 'f'),
      ci             = glue::glue("[{ci_lo}, {ci_hi}]")
    ) %>% 
    select(iv,dv,null_term, p_percent, median_beta, ci, median_n) %>% 
    pivot_wider(names_from = null_term, values_from = c(p_percent, median_beta, ci, median_n)) %>% 
   # mutate(
   #   dv          = ifelse(dv == "shifting", "Attention Shifting","WM Updating"),
   #   iv          = case_when(iv == "ses" ~ "Poverty",
   #                           iv == "unp" ~ "Unpredictability",
   #                           iv == "vio" ~ "Violence")
   # ) %>% 
    select(
      iv,dv, 
      matches("(beta|ci)_main"), p_percent_main, #p_overall_main, 
      matches("(beta|ci)_int"), p_percent_int# p_overall_int
    )
  
  # Table 2 - Mixed Models --------------------------------------------------
  table.02 <- 
    table_base %>% 
    gt(groupname_col = "dv") %>% 
    cols_label(
      iv = "", 
      median_beta_main = "\\Beta", 
      ci_main          = md("95\\% CI"),
      p_percent_main   = md("*p* (\\%)"),
     # p_overall_main   = md("*p*"), 
      median_beta_int  = "\\Beta", 
      ci_int           = md("95\\% CI"),
      p_percent_int    = md("*p* (\\%)"),
    #  p_overall_int    = md("*p*")
    ) %>% 
    tab_spanner(c("Main Effect"), columns = 3:5) %>% 
    tab_spanner(c("Interaction"), columns = 6:8) %>% 
    tab_options(
      row_group.font.weight = "bold"
    )
  
  table.02_export <- 
    table_base %>% 
    select(-iv) %>%
    filter(!dv %in% c("Initial width (sda)", "Shrinking rate (rd)")) %>%
   # mutate(iv = factor(iv, levels =  c("Unpredictability","Violence","Poverty"))) %>% 
    arrange(dv) %>% 
    #mutate(iv = as.character(iv)) %>% 
    gt() %>% 
    cols_label(
      dv               = "",
      median_beta_main = "\\Beta", 
      ci_main          = md("95% CI"),
      p_percent_main   = md("*p* (%)"),
    #  p_overall_main   = md("*p*"), 
      median_beta_int  = "&Beta;", 
      ci_int           = md("95% CI"),
      p_percent_int    = md("*p* (%)"),
     # p_overall_int    = md("*p*")
    ) %>% 
    tab_style(
      style = cell_borders("all",weight = "0px"),
      locations = cells_body(columns = 1:7, rows = 1:4)
    ) %>% 
    tab_spanner(c("Main Effect"), columns = 2:4) %>% 
    tab_spanner(c("Interaction"), columns = 5:7) %>% 
    tab_options(
      row_group.font.weight = "bold"
    )
  
  return(table.02_export)
}



multiverse_simple_slopes_table <- function(model_list, label_names = list(c("std", "deg"), c("low", "high"))) {
  
  simple_slopes <- 
    model_list %>%
    map_df(function(multiverse){
      
          specs <- 
            multiverse$specifications %>% 
            select(-spec_expr) %>% 
            pivot_wider(names_from = "spec_var", values_from = "name") %>% 
            rename_with(.cols = !spec_number, ~paste0("spec_",.x))
          
          bind_cols(
            specs,
            bind_rows(
              multiverse$simple_slopes[[1]][[3]] %>% 
                rename_with(~c("level","beta","beta_se","beta_lo","beta_hi","t_value","p_value")) %>% 
                mutate(level = ifelse(level == 0, label_names[[1]][[1]], label_names[[1]][[2]])), 
              multiverse$simple_slopes[[2]][[3]] %>% 
                rename_with(~c("level","beta","beta_se","beta_lo","beta_hi","t_value","p_value")) %>% 
                mutate(level = ifelse(level == -1, label_names[[2]][[1]], label_names[[2]][[2]])),
            )
          )
        })
  

  row_length <- nrow(simple_slopes) / unique(simple_slopes$spec_dv_type) %>% length
  # Simple Slope medians ----------------------------------------------------
  simple_effects <- 
    simple_slopes %>% 
    group_by(spec_dv_type, level) %>% 
    mutate(p_percent = (sum(p_value < .05)/row_length)) %>% 
    filter(spec_dv_type != "RT difference") %>%
    summarize(across(where(is.numeric), median)) %>% 
    select(dv = spec_dv_type, level, beta, p_percent) %>% 
    mutate(
      beta      = formatC(round(beta,2), digits = 2, width = 3, flag = '0', format = 'f'),
      p_percent = formatC(round(p_percent*100, 2), digits = 2, width = 3, flag = '0', format = 'f') %>% paste0(.,"%"),
    ) %>% 
    pivot_wider(names_from = level, values_from = c(beta, p_percent))
    
    
  
  # Table 2 - Simple Slopes -------------------------------------------------
  table.03.0 <- 
    simple_effects %>% 
    select(
      dv,
      matches(str_c("(beta|p_percent)_", label_names[[1]][[1]])),
      matches(str_c("(beta|p_percent)_", label_names[[1]][[2]])),
      matches(str_c("(beta|p_percent)_", label_names[[2]][[1]])),
      matches(str_c("(beta|p_percent)_", label_names[[2]][[2]]))
    ) %>%
    ungroup()
  
  table.03.1 <- 
    table.03.0 %>% 
    filter(!dv %in% c("Initial width (sda)", "Shrinking rate (rd)")) %>%
    mutate(dv = factor(dv, levels = c("Perceptual input (p)", "sda / rd (interference)", "Non-decision time (t0)", "Boundary separation (a)"))) %>%
    gt() %>% 
    cols_label(
      dv             = "",
      beta_std       = md("*b* (std)"),
      beta_enh       = md("*b* (enh)"),
      beta_low       = md("*b* (low)"),
      beta_high      = md("*b* (high)"),
      p_percent_std  = md("*p* (\\%)"),
      p_percent_enh  = md("*p* (\\%)"),
      p_percent_low  = md("*p* (\\%)"),
      p_percent_high = md("*p* (\\%)")
    ) %>% 
    tab_spanner(c("Task Version"), columns = 2:5) %>% 
    tab_spanner(c("Adversity"), columns = 6:9) %>% 
    tab_options(
      row_group.font.weight = "bold"
    )
}
