
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(furrr)
library(here)
library(glue)
# devtools::install_github("JimGrange/flankr", ref = "development")
library(flankr)
library(tictoc)

load(here("data", "2_study1", "1_task_data_clean.Rdata"))



# Tidy Flanker Data -------------------------------------------------------

flanker_ssp_setup <- flanker_data_clean_average %>%
  select(flanker_data_long) %>%
  mutate(
    flanker_data_long = map(flanker_data_long, function(x) {
      
      x %>%
        mutate(rt=rt/1000) %>%
        rename(subject = id, accuracy = correct) %>%
        select(subject, condition, congruency, accuracy, rt)
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
processed_files <- list.files(here("data", "2_study1")) %>%
  str_subset("ssp_fit_standard") %>%
  str_replace_all("ssp_fit_standard|.csv", "") %>%
  as.numeric()

ssp_results_standard <- flanker_ssp_setup %>%
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
ssp_results_standard  %<>%
  mutate(
    results = future_map(flanker_data_long,
                  function(x) {
                    
                    tic()
                    # Find best starting parameters
                    best_starting_parms <-
                      fitMultipleSSP(x, var = var_start_parms,
                                     conditionName = "standard",
                                     nParms = n_start_parms,
                                     nTrials = n_first_pass,
                                     multipleSubjects = FALSE)

                    
                    # Perform final fit using best starting parameters
                    final_fit <-
                      fitSSP(x, conditionName = "standard", 
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
                    write_csv(final_fit_results, here("data", "2_study1", str_c("ssp_fit_standard", unique(x$subject), ".csv")))
                    
                    message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                    
                    return(final_fit_results)
                    
                  },
                  .options = furrr_options(seed = TRUE)
    ))




# Model Fit: Enhanced condition -------------------------------------------

# In case processing was interrupted, we check which subjects were already processed and skip them
processed_files <- list.files(here("data", "2_study1")) %>%
  str_subset("ssp_fit_enhanced") %>%
  str_replace_all("ssp_fit_enhanced|.csv", "") %>%
  as.numeric()

ssp_results_enhanced <- flanker_ssp_setup %>%
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
ssp_results_enhanced <- 
  flanker_ssp_setup %>%
  mutate(
    results = future_map(flanker_data_long,
                         function(x) {
                           
                           tic()
                           # Find best starting parameters
                           best_starting_parms <-
                             fitMultipleSSP(x, var = var_start_parms,
                                            conditionName = "enhanced",
                                            nParms = n_start_parms,
                                            nTrials = n_first_pass,
                                            multipleSubjects = FALSE)
                           
                           
                           # Perform final fit using best starting parameters
                           final_fit <-
                             fitSSP(x, conditionName = "enhanced", 
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
                           write_csv(final_fit_results, here("data", "2_study1", str_c("ssp_fit_enhanced", unique(x$subject), ".csv")))
                           
                           message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                           
                           return(final_fit_results)
                           
                         },
                         .options = furrr_options(seed = TRUE)
    ))



# Model Fit: Degraded condition -------------------------------------------

# In case processing was interrupted, we check which subjects were already processed and skip them
processed_files <- list.files(here("data", "2_study1")) %>%
  str_subset("ssp_fit_degraded") %>%
  str_replace_all("ssp_fit_degraded|.csv", "") %>%
  as.numeric()

flanker_ssp_setup %<>%
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

# Initiate parallel processing
plan(multisession, workers = cores - 2)

# Find best starting parameters for each subject in the Standard Condition
ssp_results_standard <- 
  flanker_ssp_setup %>%
  mutate(
    results = future_map(flanker_data_long,
                         function(x) {
                           
                           tic()
                           # Find best starting parameters
                           best_starting_parms <-
                             fitMultipleSSP(x, var = var_start_parms,
                                            conditionName = "standard",
                                            nParms = n_start_parms,
                                            nTrials = n_first_pass,
                                            multipleSubjects = FALSE)
                           
                           
                           # Perform final fit using best starting parameters
                           final_fit <-
                             fitSSP(x, conditionName = "standard", 
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
                           write_csv(final_fit_results, here("data", "2_study1", str_c("ssp_fit_standard", unique(x$subject), ".csv")))
                           
                           message(cat("Subject", unique(x$subject), "was processed in", time$toc %>% as.numeric, "seconds."))
                           
                           return(final_fit_results)
                           
                         },
                         .options = furrr_options(seed = TRUE)
    ))



# Read Results ------------------------------------------------------------
    
 flanker_ssp_results <- list.files(here("data", "1_pilot"), pattern = "^ssp_fit", full.names = TRUE) %>%
   map_df(function(x) read_csv(x)) %>%
   rename(id = subject) %>%
   rename_with(.cols = !matches("id"), ~str_replace_all(.x, ., str_c(., "_flanker")))
 
 
# Plot model fit
 
plot_flanker <- flanker_ssp_results %>%
  
   select(a_flanker,t0_flanker,p_flanker,rd_flanker,sda_flanker,g2_flanker,bbic_flanker, id) %>%
   pmap(function(a_flanker,t0_flanker,p_flanker,rd_flanker,sda_flanker,g2_flanker,bbic_flanker, id) {
     
     list(
       parameters = list(
         bestParameters = c(a_flanker, t0_flanker, p_flanker, rd_flanker, sda_flanker),
         g2 = g2_flanker,
         bBIC = bbic_flanker),
       data = flanker_ssp_setup %>% unnest() %>% filter(subject == id)
     )
   }) 
 
 
 map(plot_flanker, function(x) {
   plotFitSSP(modelFit = x$parameters, data = x$data)
 })
 
save(flanker_ssp_results, file = here("data", "1_pilot", "1_SSP_objects.Rdata"))
