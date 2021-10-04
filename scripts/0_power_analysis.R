
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(faux)



# DDM parameter generation ------------------------------------------------




# Data parameters ---------------------------------------------------------

generate_design <- function(n_participants, n_conditions, n_trials, condition_labels = list("neutral", "cued")){
  
  design_matrix <- expand.grid(
    participant = 1:n_participants, 
    condition = 1:n_conditions, 
    trials = 1:n_trials) # here we create the data-frame
  
  design_matrix$condition <- ifelse(design_matrix$condition == 1, condition_labels[[1]], condition_labels[[2]])
  design_matrix$trials <- paste0(design_matrix$condition, "_", design_matrix$trials) 

    return(design_matrix) # return the data-frame
}

# Cueing

### Adversity * Condition (congruent vs incongruent)

df_cueing <- generate_design(n_participants = 10, n_condition = 2, n_trials = 32) 

df_cueing$condition_f <- factor(df_cueing$condition)
contr.sum(df_cueing$condition_f)

