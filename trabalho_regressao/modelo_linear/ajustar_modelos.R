ajustar_modelos <- function(dados) {
  nomes <- names(dados)

  ajustes <- map(dados, function(item) {
    map(item, function(df) {
      ajuste <- lm(y~x, df)
      return(ajuste)
    })
  })

  coef_estimados <- map(ajustes, function(item) {
    map(item, function(ajuste) {
      return(ajuste$coefficients %>% as.numeric())
    })
  })

  coef_estimados_df <- map(1:length(dados), function(i){
    map_df(coef_estimados[[i]], ~ as.data.frame(t(.))) %>% mutate(delim = nomes[i])
  }) %>% bind_rows()

  names(coef_estimados_df) <- c('beta0', 'beta1', 'delim')

  return(list(coefs = coef_estimados_df,
              ajustes = ajustes))
}
