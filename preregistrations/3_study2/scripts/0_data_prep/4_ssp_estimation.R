
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(furrr)
library(here)
library(glue)
# devtools::install_github("JimGrange/flankr", ref = "development")
library(flankr)
library(tictoc)

load(here("data", "3_study2", "1_task_data_clean.Rdata"))



# Tidy Flanker Data -------------------------------------------------------

flanker_ssp_setup <- flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x) {
      
      x %>%
        mutate(rt=rt/1000) %>%
        rename(subject = id, accuracy = correct) %>%
        select(subject, congruency, accuracy, rt)
    }) 
  )

# Global Fit Parameters ---------------------------------------------------

# Number of cores for parallel processing
cores <- parallel::detectCores()

# set random seed so user can reproduce simulation outcome
set.seed(42)

# during the fit, how many sets of starting parameters should be used?
n_start_parms <- 50

# what should the variance across starting parameters be?
var_start_parms <- 20

# how many trials to simulate during each iteration of the fit routine whilst
# exploring multiple starting parameters?
n_first_pass <- 1000

# how many trials to simulate during the final fit routine?
n_final_pass <- 50000



# Model Fit: Standard Condition -------------------------------------------

# In case processing was interrupted, we check which subjects were already processed and skip them
processed_files <- list.files(here("data", "3_study2")) %>%
  str_subset("ssp_fit") %>%
  str_replace_all("ssp_fit|.csv", "") %>%
  as.numeric()

ssp_results <- flanker_ssp_setup %>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x){

      if(unique(x$subject) %in% processed_files) {
        return(NA)
      } else {
        return(x)
      }
    })
  ) %>%
  filter(!is.na(flanker_data_long))
  
# Fit SSP Model 

# Initiate parallel processing
plan(multisession, workers = cores - 2)
 
# Find best starting parameters for each subject in the Standard Condition
ssp_results  %<>%
  mutate(
    results = future_map(flanker_data_long,
                  function(x) {
                    
                    tic()
                    # Find best starting parameters
                    best_starting_parms <-
                      fitMultipleSSP(x, var = var_start_parms,
                                     nParms = n_start_parms,
                                     nTrials = n_first_pass,
                                     multipleSubjects = FALSE)

                    
                    # Perform final fit using best starting parameters
                    final_fit <-
                      fitSSP(x, 
                             parms = best_starting_parms$bestParameters,
                             nTrials = n_final_pass, multipleSubjects = FALSE)
                    time <- toc()
                    
                    final_fit_results <-
                      tibble(
                        subject    = unique(x$subject),
                        start_a    = best_starting_parms$bestParameters[1],
                        start_t0   = best_starting_parms$bestParameters[2],
                        start_p    = best_starting_parms$bestParameters[3],
                        start_rd   = best_starting_parms$bestParameters[4],
                        start_sda  = best_starting_parms$bestParameters[5],
                        start_g2   = best_starting_parms$g2,
                        start_bbic = best_starting_parms$bBIC,
                        
                        a          = final_fit$bestParameters[1],
                        t0         = final_fit$bestParameters[2],
                        p          = final_fit$bestParameters[3],
                        rd         = final_fit$bestParameters[4],
                        sda        = final_fit$bestParameters[5],
                        g2         = final_fit$g2,
                        bbic       = final_fit$bBIC
                      )
                    
                    # Backup data
                    write_csv(final_fit_results, here("data", "3_study2/ssp", str_c("ssp_fit", unique(x$subject), ".csv")))
                    
                    message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                    
                    return(final_fit_results)
                    
                  },
                  .options = furrr_options(seed = TRUE)
    ))





# Read Results ------------------------------------------------------------
    
ssp_results_initial <- c("^ssp_fit") %>%
  map(function(x) {
    list.files(here("data","3_study2", "ssp"), pattern = x, full.names = TRUE) %>%
      map_df(function(y) read_csv(y)) %>%
      rename(id = subject) %>%
      rename_with(.cols = !matches("id"), ~str_replace_all(.x, ., str_c(., "_flanker")))
  }) %>%
  reduce(left_join) %>%
  select(-c(starts_with(c("start")))) %>%
  filter(rd_flanker > 0) |> 
  mutate(
    interference_flanker = sda_flanker / rd_flanker,

  )



# Investigate extreme DDM parameter estimates -----------------------------

ssp_outliers <- ssp_results_initial %>%
  mutate(across(matches("^(a|t0|p|interference)"), ~scale(.) %>% as.numeric, .names = "{.col}_z")) %>%
  drop_na() %>%
  filter(if_any(ends_with("_z"), ~ . > 3.2))

flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  unnest(flanker_data_long) %>%
  filter(id %in% c(ssp_outliers %>% pull(id) %>% unique)) %>%
  split(.$id) %>%
  map(function(x) {
    ggplot(x, aes(scale(rt))) +
      geom_histogram(bins = 64) +
      facet_wrap(~congruency) + 
      scale_x_continuous(breaks = seq(-12,12, 0.5)) +
      labs(
        title = str_c("id = ", x$id %>% unique)
      ) %>%
      print
  })

# Apply trial filtering and nest data
refit_data <- flanker_ssp_setup %>%
  unnest(flanker_data_long) %>%
  filter(subject %in% c(ssp_outliers %>% pull(id) %>% unique)) %>%
  group_by(subject, congruency) %>%
  mutate(rt_z = scale(rt) %>% as.numeric) %>%
  filter(rt_z < 3.2) %>%
  group_by(subject, condition) %>%
  mutate(n_trials = n()) %>%
  filter(n_trials > 54) %>%
  ungroup() %>%
  select(-c(rt_z, n_trials))


# Refit SSP model for participants with extreme values --------------------

ssp_refit <- tibble(subject = ssp_outliers %>% pull(id) %>% unique) %>%
  mutate(flanker_data_long = map(subject, function(x) {refit_data %>% filter(subject == x)})) %>%
  select(flanker_data_long)


# Find best starting parameters for each subject in the Standard Condition
ssp_refit  %>%
  mutate(
    results = future_map(flanker_data_long,
                         function(x) {
                           
                           tic()
                           # Find best starting parameters
                           best_starting_parms <-
                             fitMultipleSSP(x, var = var_start_parms,
                                            nParms = n_start_parms,
                                            nTrials = n_first_pass,
                                            multipleSubjects = FALSE)
                           
                           
                           # Perform final fit using best starting parameters
                           final_fit <-
                             fitSSP(x, 
                                    parms = best_starting_parms$bestParameters,
                                    nTrials = n_final_pass, multipleSubjects = FALSE)
                           time <- toc()
                           
                           final_fit_results <-
                             tibble(
                               subject    = unique(x$subject),
                               start_a    = best_starting_parms$bestParameters[1],
                               start_t0   = best_starting_parms$bestParameters[2],
                               start_p    = best_starting_parms$bestParameters[3],
                               start_rd   = best_starting_parms$bestParameters[4],
                               start_sda  = best_starting_parms$bestParameters[5],
                               start_g2   = best_starting_parms$g2,
                               start_bbic = best_starting_parms$bBIC,
                               
                               a          = final_fit$bestParameters[1],
                               t0         = final_fit$bestParameters[2],
                               p          = final_fit$bestParameters[3],
                               rd         = final_fit$bestParameters[4],
                               sda        = final_fit$bestParameters[5],
                               g2         = final_fit$g2,
                               bbic       = final_fit$bBIC
                             )
                           
                           # Backup data
                           write_csv(final_fit_results, here("data", "3_study2", str_c("ssp_refit", unique(x$subject), ".csv")))
                           
                           message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                           
                           return(final_fit_results)
                           
                         },
                         .options = furrr_options(seed = TRUE)
    ))


ssp_results_refit <- c("^ssp_refit") %>%
  map(function(x) {
    list.files(here("data", "3_study2", "ssp"), pattern = x, full.names = TRUE) %>%
      map_df(function(y) read_csv(y)) %>%
      rename(id = subject) %>%
      rename_with(.cols = !matches("id"), ~str_replace_all(.x, ., str_c(., "_flanker")))
  }) %>%
  reduce(left_join) %>%
  select(-c(starts_with(c("start")))) %>%
  rename_with(~gsub("standard", "std", .x)) %>%
  rename_with(~gsub("enhanced", "enh", .x)) %>%
  rename_with(~gsub("degraded", "deg", .x)) %>%
  mutate(
    interference_flanker = sda_flanker / rd_flanker
  ) 
  

# Bind results from initial fit and refit and replace initial values for refitted values.
ssp_results_refit %<>%
  mutate(ssp_refit = TRUE) %>%
  bind_rows(
    ssp_results_initial %>%
      filter(!id %in% c(ssp_results_refit$id %>% unique))
    ) %>%
  mutate(ssp_refit = ifelse(is.na(ssp_refit), FALSE, ssp_refit))




save(ssp_results_initial, ssp_results_refit, file = here("data", "3_study2", "1_SSP_objects.Rdata"))
