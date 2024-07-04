plot_all_nl <- function(dfs, betas) {
  random_dfs <- map(dfs, ~ .x[[sample(length(.x), 1)]])

  plts <- map(1:18, function(i) {
    plt <- random_dfs[[i]] %>%
      ggplot(aes(x = x, y = y)) +
      lims(y = c(-1,15), x = c(0,12)) +
      geom_point() +
      stat_function(fun = function(x) (betas[1]*x)/(betas[2] + x), colour = 'tomato', size = 0.8) +
      theme_minimal() +
      ggtitle(names(dfs)[i]) +
      theme(legend.position = 'none',
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5))
    return(plt)
  })

  wrap_plots(plts, nrow = 3)
}

boxplot_beta1_nl <- function(dados, sigmas) {
  plts_beta0 <- map(sigmas, function(s) {
    plt <- dados %>%
      filter(str_sub(delim, -4) == s) %>%
      ggplot(aes(x = delim, y = beta1)) +
      geom_boxplot(colour = 'darkblue') +
      geom_hline(yintercept = 10, colour ='tomato', linewidth = 0.8) +
      ylim(8, 12) +
      theme_bw() +
      labs(x = 'Delineamento', y='Beta1')
    return(plt)
  })
  wrap_plots(plts_beta0, nrow=3)
}


boxplot_beta2_nl <- function(dados, sigmas) {
  plts_beta1 <- map(sigmas, function(s) {
    plt <- dados %>%
      filter(str_sub(delim, -4) == s) %>%
      ggplot(aes(x = delim, y = beta2)) +
      geom_boxplot(colour = 'darkblue') +
      geom_hline(yintercept = 2, colour ='tomato', linewidth = 0.8) +
      ylim(0, 4) +
      theme_bw() +
      labs(x = 'Delineamento', y='Beta2')
    return(plt)
  })
  wrap_plots(plts_beta1, nrow=3)
}

plot_intervalo_pred <- function(dados, ajustes) {

  betas = c(10,2)
  random_dfs <- sample(1:100, 18, replace = T)
  new_x_values <- data.frame(x = seq(from = 0, to = 12, by = 0.5))

  plts <- map(1:18, function(i) {
    df <- dados[[i]][[random_dfs[i]]]
    ajuste <- ajustes[[i]][[random_dfs[i]]]

    predictions <- predict(ajuste, newdata = data.frame(x = new_x_values))

    # Get model summary
    model_summary <- summary(ajuste)

    # Extract coefficients and their standard errors
    coefficients <- coef(ajuste)
    se_coefficients <- coef(summary(ajuste))[, "Std. Error"]

    # Calculate standard error of predictions (assuming a simple model for example)
    new_data <- data.frame(x = new_x_values)
    J <- model.matrix(~ x, data = new_data)  # Jacobian matrix
    sigma <- model_summary$sigma  # Residual standard error

    # Variance-covariance matrix of the coefficients
    vcov_matrix <- vcov(ajuste)

    # Standard error of the fitted values
    se_fit <- sqrt(diag(J %*% vcov_matrix %*% t(J)))

    # Prediction intervals
    critical_value <- qt(0.975, df = df.residual(ajuste))  # 95% CI
    lower_bound <- predictions - critical_value * se_fit
    upper_bound <- predictions + critical_value * se_fit

    # Combine the results
    final <- data.frame(
      x = new_x_values,
      fit = predictions,
      lwr = lower_bound,
      upr = upper_bound
    )

    plt <- ggplot(final, aes(x = x, y = fit)) +
      geom_line(colour = 'tomato', linewidth = 0.8) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = 'steelblue', fill = 'steelblue') +
      stat_function(fun = function(x) (betas[1]*x)/(betas[2] + x), colour = 'gold', size = 0.8) +
      geom_point(data = df, aes(x = x, y = y)) +
      theme_minimal() +
      lims(y = c(-1,15), x = c(0,12)) +
      ggtitle(names(dados)[i]) +
      theme(legend.position = 'none',
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5))
    return(plt)
  })

  return(wrap_plots(plts, nrow = 3))
}

