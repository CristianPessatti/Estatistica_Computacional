get_coefficients_nl <- function(coef_estimados_df) {
  metrics <- coef_estimados_df %>%
    group_by(delim) %>%
    summarise(b1_mean = mean(beta1),
              b1_sd = sd(beta1),
              b2_mean = mean(beta2),
              b2_sd = sd(beta2))
  return(metrics)
}

get_pred_int <- function(dados, mods) {
  betas = c(10,2)
  new_x_values <- data.frame(x = seq(from = 0, to = 12, by = 0.1))
  nomes <- names(dados)

  values <- map(1:length(dados), function(i) {
    map(mods[[i]], function(ajuste) {
      predictions <- predict(ajuste, newdata = new_x_values)

      model_summary <- summary(ajuste)

      coefficients <- coef(ajuste)
      se_coefficients <- coef(summary(ajuste))[, "Std. Error"]

      new_data <- data.frame(x = new_x_values)
      J <- model.matrix(~ x, data = new_data)
      sigma <- model_summary$sigma

      vcov_matrix <- vcov(ajuste)

      se_fit <- sqrt(diag(J %*% vcov_matrix %*% t(J)))

      critical_value <- qt(0.975, df = df.residual(ajuste))
      lower_bound <- predictions - critical_value * se_fit
      upper_bound <- predictions + critical_value * se_fit

      final <- data.frame(
        x = new_x_values,
        fit = predictions,
        lwr = lower_bound,
        upr = upper_bound
      )

      max_int <- max(final$upr - final$lwr)
      mean_int <- mean(final$upr - final$lwr)

      return(data.frame(delim = nomes[i], max_int = max_int, mean_int = mean_int))
    })
  })
  return(values)
}
