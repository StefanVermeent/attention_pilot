generate_RTs <- function(trials, boundary_sep, tau, beta, drift_rate, id) { # alpha = boundary separation, tau = non-decision time, beta = starting point, delta = drift rate
  
  set.seed(42)
  
  RT_data <- rwiener(n     = trials,
                     alpha = boundary_sep,
                     tau   = tau,
                     beta  = beta,
                     delta = drift_rate) %>%
    mutate(
      resp = ifelse(resp == "upper", 1, 0))
  
  
  utils::write.table(RT_data, str_c(here("data", "0_simulation", id), ".dat"), row.names = FALSE, col.names = FALSE, sep = " ")

}
