get_coefficients <- function(dados) {
  nomes <- names(dados)

  coef_estimados <- map(dados, function(item) {
    map(item, function(df) {
      ajuste <- lm(y~x, df)
      return(ajuste$coefficients %>% as.numeric())
    })
  })

  metrics <- map(1:18, function(i){
    map_df(coef_estimados[[i]], ~ as.data.frame(t(.))) %>% mutate(delim = nomes[i])
  }) %>% bind_rows()

  names(metrics) <- c('beta0', 'beta1', 'delim')

  metrics %>%
    group_by(delim) %>%
    summarise(b0_mean = mean(beta0),
              b0_sd = sd(beta0),
              b1_mean = mean(beta1),
              b1_sd = sd(beta1)) %>%
    return()
}
