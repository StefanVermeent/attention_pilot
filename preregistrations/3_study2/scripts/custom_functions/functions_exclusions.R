# Calculate Standard Deviation in response patterns on survey items

response_sd <- function(df, prefix_vars) {
  
 df %>%
   rowwise() %>%
   mutate("sd_{prefix_vars}" := sd(c_across(matches(prefix_vars)), na.rm = TRUE)) %>%
   ungroup() %>%
   var_labels(
     "sd_{prefix_vars}" := "Standard Deviation of item responses on the questionnaire. If (close to) 0, the participant tended to respond with the same response option regardless of reverse coding."
   )
}

gbinom <- function(n, p, low=0, high=n,scale = F) {
   
   sd <- sqrt(n * p * (1 - p))
   if(scale && (n > 10)) {
      low <- max(0, round(n * p - 4 * sd))
      high <- min(n, round(n * p + 4 * sd))
   }
   values <- low:high
   probs <- dbinom(values, n, p)
   
   max_chance_performance <- max(values[which(probs > 0.025)])
   
   
   
   return((max_chance_performance/n)*100)
}
