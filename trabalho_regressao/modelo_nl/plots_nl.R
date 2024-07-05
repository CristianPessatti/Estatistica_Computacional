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
  ndf <- data.frame('x' = seq(0, 12, by = 0.1))

  plts <- map(1:18, function(i) {
    df <- dados[[i]][[random_dfs[i]]]
    fit <- ajustes[[i]][[random_dfs[i]]]

    # Prever os valores pontuais
    ndf$fit <- predict(fit, newdata = ndf)

    # Calcular a matriz de covariância dos parâmetros ajustados
    cov_matrix <- vcov(fit)

    # Função para calcular os erros padrão das previsões
    predict_se <- function(x, beta1, beta2, cov_matrix) {
      grad <- c(x / (beta2 + x), -beta1 * x / (beta2 + x)^2)
      sqrt(t(grad) %*% cov_matrix %*% grad)
    }

    # Calcular os intervalos de confiança manualmente
    alpha <- 0.05
    t_value <- qt(1 - alpha / 2, df.residual(fit))

    # Calcular os erros padrão das previsões
    ndf$se <- apply(ndf, 1, function(row) predict_se(row['x'], coef(fit)[1], coef(fit)[2], cov_matrix))

    # Calcular os intervalos de confiança
    ndf$lwr_conf <- ndf$fit - t_value * ndf$se
    ndf$upr_conf <- ndf$fit + t_value * ndf$se

    # Calcular os erros residuais padrão
    residual_se <- sqrt(sum(residuals(fit)^2) / df.residual(fit))

    # Calcular os intervalos de predição
    ndf$lwr_pred <- ndf$fit - t_value * sqrt(ndf$se^2 + residual_se^2)
    ndf$upr_pred <- ndf$fit + t_value * sqrt(ndf$se^2 + residual_se^2)

    plt <- ggplot(ndf, aes(x = x, y = fit)) +
      geom_line(colour = 'tomato', linewidth = 0.8) +
      geom_ribbon(aes(ymin = lwr_pred, ymax = upr_pred), alpha = 0.2, colour = 'steelblue', fill = 'steelblue') +
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

