get_coefficients_nl <- function(coef_estimados_df) {
  metrics <- coef_estimados_df %>%
    group_by(delim) %>%
    summarise(b1_mean = mean(beta1),
              b1_sd = sd(beta1),
              b2_mean = mean(beta2),
              b2_sd = sd(beta2))
  return(metrics)
}
