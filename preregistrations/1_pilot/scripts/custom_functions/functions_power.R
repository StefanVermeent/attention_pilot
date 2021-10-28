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
  
  ## if desired: check results
  #all.equal(X1,X[,1])
  #cor(X)
  
  return(df)
}


calculate_sigma = function(low_adversity_sd, high_adversity_sd, correlation) {
  
  sigma <- matrix(c(low_adversity_sd^2, low_adversity_sd*high_adversity_sd*correlation, low_adversity_sd*high_adversity_sd*correlation, high_adversity_sd^2), ncol = 2) # define variance-covariance matrix
  
}

sigma_grid <- expand_grid(
  low_adversity_sd = c(0.5, 1, 1.5, 2),
  high_adversity_sd = c(0.5, 1, 1.5, 2),
  correlation = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
)

values_of_sigma <- sigma_grid %>%
  pmap(calculate_sigma)

