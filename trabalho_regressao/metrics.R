get_coefficients <- function(dados) {
  nomes <- names(dados)

  coef_estimados <- map(dados, function(item) {
    map(item, function(df) {
      ajuste <- lm(y~x, df)
      return(ajuste$coefficients %>% as.numeric())
    })
  })

  coef_estimados_df <- map(1:18, function(i){
    map_df(coef_estimados[[i]], ~ as.data.frame(t(.))) %>% mutate(delim = nomes[i])
  }) %>% bind_rows()

  names(coef_estimados_df) <- c('beta0', 'beta1', 'delim')

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

get_coefficients_nl <- function(dados) {
  nomes <- names(dados)

  coef_estimados <- map(dados, function(item) {
    map(item, function(df) {
      ajuste <- nls(y ~ (beta1*x)/(beta2 + x), data = df, start = list(beta1 = 8, beta2 = 4), control = nls.control(maxiter = 1000))
      return(coef(ajuste))
    })
  })

  coef_estimados_df <- map(1:18, function(i){
    map_df(coef_estimados[[i]], ~ as.data.frame(t(.))) %>% mutate(delim = nomes[i])
  }) %>% bind_rows()

  names(coef_estimados_df) <- c('beta1', 'beta2', 'delim')

  metrics <- coef_estimados_df %>%
    group_by(delim) %>%
    summarise(b1_mean = mean(beta1),
              b1_sd = sd(beta1),
              b2_mean = mean(beta2),
              b2_sd = sd(beta2))
  return(list(
    coefficients = coef_estimados_df,
    metrics = metrics
  ))
}
