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


