require(patchwork)

plot_all <- function(dfs) {
  random_dfs <- map(dfs, ~ .x[[sample(length(.x), 1)]])

  plts <- map(1:18, function(i) {
    plt <- random_dfs[[i]] %>%
      ggplot(aes(x = x, y = y)) +
      lims(y = c(-20,20), x = c(-10,10)) +
      geom_smooth(method = 'lm', se=F, colour = 'steelblue') +
      geom_abline(slope = 1, intercept = 1, colour = 'tomato', linewidth = 0.8) +
      geom_point() +
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

boxplot_beta0 <- function(dados, sigmas) {
  plts_beta0 <- map(sigmas, function(s) {
    plt <- dados %>%
      filter(str_detect(delim, paste0(s,'$'))) %>%
      ggplot(aes(x = delim, y = beta0)) +
      geom_boxplot(colour = 'darkblue') +
      geom_hline(yintercept = 1, colour ='tomato', linewidth = 0.8) +
      ylim(-0.5, 2.5) +
      theme_bw() +
      labs(x = 'Delineamento', y='Beta0')
    return(plt)
  })
  wrap_plots(plts_beta0, nrow=3)
}


boxplot_beta1 <- function(dados, sigmas) {
  plts_beta1 <- map(sigmas, function(s) {
    plt <- dados %>%
      filter(str_detect(delim, paste0(s,'$'))) %>%
      ggplot(aes(x = delim, y = beta1)) +
      geom_boxplot(colour = 'darkblue') +
      geom_hline(yintercept = 1, colour ='tomato', linewidth = 0.8) +
      ylim(0, 2) +
      theme_bw() +
      labs(x = 'Delineamento', y='Beta1')
    return(plt)
  })
  wrap_plots(plts_beta1, nrow=3)
}

plot_intervalo_pred <- function(dados, ajustes) {

  random_dfs <- sample(1:100, 18, replace = T)
  dados_new <- data.frame(x = seq(from = -10, to = 10, by = 0.5))

  plts <- map(1:18, function(i) {
    df <- dados[[i]][[random_dfs[i]]]
    ajuste <- ajustes[[i]][[random_dfs[i]]]

    y = predict(ajuste, newdata = dados_new, interval = 'prediction') %>% as.data.frame()
    final <- bind_cols(dados_new, y)

    plt <- ggplot(final, aes(x = x, y = fit)) +
      geom_line(colour = 'tomato', linewidth = 0.8) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = 'steelblue', fill = 'steelblue') +
      geom_point(data = df, aes(x = x, y = y)) +
      theme_minimal() +
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
