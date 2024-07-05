ajustar_mods_nl <- function(dados) {
  nomes <- names(dados)

  ajustes <- map(dados, function(item) {
    map(item, function(df) {
      ajuste <- nls(y ~ (beta1*x)/(beta2 + x), data = df, start = list(beta1 = 10, beta2 = 2), control = nls.control(maxiter = 5000))
      return(ajuste)
    })
  })

  coef_estimados <- map(ajustes, function(item) {
    map(item, function(ajuste) {
      coef(ajuste)
    })
  })

  coef_estimados_df <- map(1:length(dados), function(i){
    map_df(coef_estimados[[i]], ~ as.data.frame(t(.))) %>% mutate(delim = nomes[i])
  }) %>% bind_rows()

  names(coef_estimados_df) <- c('beta1', 'beta2', 'delim')

  return(list(coefs = coef_estimados_df, mods = ajustes))
}
