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


# Calculate correlated variables
getBiCop <- function(n, rho, mar.fun=rnorm, x = NULL, ...) {
  if (!is.null(x)) {X1 <- x} else {X1 <- mar.fun(n, ...)}
  if (!is.null(x) & length(x) != n) warning("Variable x does not have the same length as n!")
  
  C <- matrix(rho, nrow = 2, ncol = 2)
  diag(C) <- 1
  
  C <- chol(C)
  
  X2 <- mar.fun(n)
  X <- cbind(X1,X2)
  
  # induce correlation (does not change X1)
  df <- X %*% C
  
  return(df)
}
