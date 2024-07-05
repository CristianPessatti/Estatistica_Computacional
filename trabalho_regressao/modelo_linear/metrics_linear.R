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


get_pred_int_linear <- function(dados, mods) {
  betas = c(1,1)
  new_x_values <- data.frame(x = seq(from = -10, to = 10, by = 0.2))
  nomes <- names(dados)

  values <- map(1:length(dados), function(i) {
    map(mods[[i]], function(ajuste) {
      predictions <- predict(ajuste, newdata = new_x_values, interval = "prediction") %>% as.data.frame()

      max_int <- max(predictions$upr - predictions$lwr)
      mean_int <- mean(predictions$upr - predictions$lwr)

      return(data.frame(delim = nomes[i], max_int = max_int, mean_int = mean_int))
    })
  })
  return(values)
}
