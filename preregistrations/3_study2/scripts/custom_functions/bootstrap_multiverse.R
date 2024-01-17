bootstrap_multiverse <- function(data, mult, effect, grouping_id = NULL, n_bootstrap = 2, cores = 1) {
  # Step 1: Already done in the multiverse_tbl
  
  if(str_detect(effect, "\\:")) {
    interaction <- TRUE
  } else {
    interaction <- FALSE
  }
  
  #  print(interaction)
  
  # Step 2: Generate K dependent variables under the null
  
  if(interaction){
    effects_split <- str_split(effect, pattern = "\\:") |> unlist()
    effect_var <- str_replace(effect, pattern = "\\:", "X")
  } else {
    effect_var = NULL
    effects_split = NULL
  }
  
  mult_unpacked <- mult %>%
    reveal(.what = "model_fitted", .which = matches("tidy"), .unpack_specs = 'wide') |> 
    filter(term == {{effect}}) |> 
    mutate(
      null_y = pmap(
        list(
          estimate = estimate,
          dv       = dv
        ),
        .f = function(estimate, dv) {
          
          if(interaction) {
            data_modified <- data |> 
              mutate(!!effect_var := .data[[effects_split[[1]]]] * .data[[effects_split[[2]]]]) |>
              mutate(null_y = data[[dv]] - estimate * .data[[effect_var]])
          } else {
            data_modified <- data |> 
              mutate(null_y = data[[dv]] - estimate * data[[effect]])
          }
          
          return(data_modified)
        }
      )
    )
  
  # Steps 3-5: Bootstrap
  
  ## Initiate Results tibble
  result <- tibble(
    boot_n = numeric(), 
    iv = character(), 
    dv = character(), 
    boot_est = numeric() 
  )
  
  future::plan("multisession", workers = cores)
  
  result <- 1:n_bootstrap |> 
    furrr::future_map_dfr(function(x){
      
      if(!is.null(grouping_id)) {
        
        sampled_ids <- data %>% 
          distinct(across(matches(grouping_id))) %>% 
          sample_n(size = length(unique(data[[grouping_id]])), replace = TRUE) %>% 
          pull(id) %>%
          tibble(id = .)  # convert it into a tibble for joining
        
        
        boot_sample <- sampled_ids #%>%
        # left_join(data, by = "id", relationship = "many-to-many")
        
      } else {
        boot_sample <- data |> 
          sample_n(n(), replace = TRUE) |> 
          select(id)
      }
      
      
      result_i <- mult_unpacked %>%  
        mutate(boot_code = str_remove(string = lm_code, pattern = "^([a-zA-Z]*|[0-9]*|_*)*\\s\\|\\>")) |>
        mutate(
          boot_data = pmap(
            .l = list(
              null_y = null_y,
              dv     = dv
            ),
            .f = function(null_y, dv) {
              
              df <- null_y |> 
                right_join(boot_sample, by = "id", relationship = "many-to-many") |> 
                select(-matches(dv)) 
              
              names(df)[names(df) == "null_y"] <- dv
              
              df
              
            }
          )
        )
      
      result_i <- result_i |> 
        #  rowwise() |> 
        mutate(
          boot_est = pmap_dbl(
            list(
              boot_data = boot_data,
              boot_code = boot_code
            ),
            function(boot_data, boot_code) {
              library(lmerTest)
              rlang::eval_tidy(rlang::parse_expr(str_c("boot_data", "|>", boot_code))) |> 
                broom.mixed::tidy() |> 
                filter(term == {{effect}}) |> 
                pull(estimate)
            }
          )     
        ) |> 
        ungroup() |> 
        group_by(iv, dv) |> 
        summarise(boot_est = median(boot_est)) |> 
        mutate(boot_n = x)
      
      result <- result |> 
        drop_na(boot_n) |> 
        bind_rows(result_i)
    }, .options = furrr_options(seed = TRUE))
  
  plan("sequential")
  
  original_effect <- mult  |>
    reveal(.what = 'model_fitted', .which = matches("tidy"), .unpack_specs = "wide")  |>
    filter(term == {{effect}}) |>
    group_by(iv, dv) |>
    summarise(med_effect = median(estimate))
  
  
  result_sum <- result |>
    left_join(original_effect) |>
    mutate(
      larger = ifelse(abs(med_effect) > abs(boot_est), FALSE, TRUE)
    ) |>
    group_by(iv, dv) |>
    summarise(boot_p = sum(larger)/n())
  
  return(list(result = result, result_sum = result_sum))
}
