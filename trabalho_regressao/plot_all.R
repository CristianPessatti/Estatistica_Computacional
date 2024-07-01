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
