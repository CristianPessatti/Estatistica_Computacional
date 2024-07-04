get_coefficients <- function(coef_estimados_df) {
  metrics <- coef_estimados_df %>%
    group_by(delim) %>%
    summarise(b0_mean = mean(beta0),
              b0_sd = sd(beta0),
              b1_mean = mean(beta1),
              b1_sd = sd(beta1))
  return(list(
    coefficients = coef_estimados_df,
    metrics = metrics
  ))
}
