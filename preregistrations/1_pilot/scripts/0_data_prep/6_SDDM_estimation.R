
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(furrr)
library(here)
library(glue)
# devtools::install_github("JimGrange/flankr", ref = "development")
library(flankr)
library(tictoc)

load(here("data", "1_pilot", "1_task_data_clean.Rdata"))



# Tidy Flanker Data -------------------------------------------------------

flanker_SDDM_setup <- flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x) {
      
      x %>%
        mutate(rt=rt/1000) %>%
        rename(subject = id, accuracy = correct) %>%
        select(subject, congruency, accuracy, rt)
    }) 
  )


# In case processing was interrupted, we check which subjects were already processed and skip them
processed_files <- list.files(here("data", "1_pilot")) %>%
  str_subset("ssp_fit") %>%
  str_replace_all("ssp_fit_|.csv", "") %>%
  as.numeric()

flanker_SDDM_setup %<>%
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
  
# Fit SSP Model -----------------------------------------------------------

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
  

# Initiate parallel processing
plan(multisession, workers = cores - 2)
 
# Find best starting parameters for each subject

ssp_results <- 
  flanker_SDDM_setup %>%
  mutate(
    results = future_map(flanker_data_long,
                  function(x) {
                    
                    message("we are getting past the start")
                    tic()
                    # Find best starting parameters
                    best_starting_parms <-
                      fitMultipleSSP(x, var = var_start_parms,
                                     nParms = n_start_parms,
                                     nTrials = n_first_pass,
                                     multipleSubjects = FALSE)
                    
                    message("We got pas the best starting parms function")
                    
                    
                    # Perform final fit using best starting parameters
                    final_fit <-
                      fitSSP(x, parms = best_starting_parms$bestParameters,
                             nTrials = n_final_pass, multipleSubjects = FALSE)
                    time <- toc()
                    
                    message("We got pas the final fit")
                    
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
                    write_csv(final_fit_results, here("data", "1_pilot", str_c("ssp_fit_", unique(x$subject), ".csv")))
                    
                    message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                    
                    return(final_fit_results)
                    
                  },
                  .options = furrr_options(seed = TRUE)
    ))
  
 
    
    
 flanker_ssp_results <- list.files(here("data", "1_pilot"), pattern = "^ssp_fit", full.names = TRUE) %>%
   map_df(function(x) read_csv(x)) %>%
   rename(id = subject) %>%
   rename_with(.cols = !matches("id"), ~str_replace_all(.x, ., str_c(., "_flanker")))
 
save(flanker_ssp_results, file = here("data", "1_pilot", "1_SSP_objects.Rdata"))
