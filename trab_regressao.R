require(tidyverse)
require(patchwork)
# ==============================================================================
# GERAR AMOSTRAS ASSUMINDO ERRO ~ N(0,4)
gen_amostras <- function(x, beta0 = 1, beta1 = 1) {
  y <-  beta0 + beta1 * x + rnorm(length(x), mean = 0, sd = 2)
  return(y)
}

# GERAR DATAFRAMES PARA TODO TIPO DE DELINEAMENTO (24 amostras)

n <- 8
dfs <- list()

# Tomar pontos ao acaso entre -10 e 10.
dfs[[1]] <- data.frame(x = runif(n, -10, 10))
dfs[[1]]$y <- gen_amostras(dfs[[1]]$x)


# Tomar pontos igualmente espaçados no intervalo.
dfs[[2]] <- data.frame(x = seq(from=-10, to=10, length.out=n))
dfs[[2]]$y <- gen_amostras(dfs[[2]]$x)

# Tomar dois valores em cada ponto escolhido ao acaso.
dfs[[3]] <- data.frame(x = rep(runif(n/2,-10,10), 2)) %>% arrange(x)
dfs[[3]]$y <- gen_amostras(dfs[[3]]$x)

# Tomar dois valores em cada ponto dentre pontos igualmente espaçados.
dfs[[4]] <- data.frame(x = rep(rep(seq(from=-10,to=10,length.out=n/2),2))) %>% arrange(x)
dfs[[4]]$y <- gen_amostras(dfs[[4]]$x)

# Tomar 1/4 de pontos em cada posição.
dfs[[5]] <- data.frame(x = rep(seq(from=-10,to=10,length.out=4), n*(1/4))) %>% arrange(x)
dfs[[5]]$y <- gen_amostras(dfs[[5]]$x)

# Tomar metade dos valores em x=−10 e a outra metade em x=10.
dfs[[6]] <- data.frame(x = c(rep(-10, n/2), rep(10, n/2)))
dfs[[6]]$y <- gen_amostras(dfs[[6]]$x)
# PS: AINDA FALTA FAZER MAIS 2 FORMAS QUE A GENTE DEFINE

# ==============================================================================
# FUNÇÃO QUE FAZ O AJUSTE E RETORNA: A BASE DE DADOS (dfs[[i]]) UTILIZADA,
# O AJUSTE, E AS MÉTRICAS OBTIDAS

#PS: AINDA FALTA FAZER OUTRAS MÉTRICAS QUE O PROFESSOR PEDE PARA SUGERIR
fazer_ajuste <- function(x) {
  ajuste <- lm(y~., data=x)
  sumario <- summary(ajuste)

  ic_pred <- predict(ajuste, interval='predict')

  medidas <- data.frame(beta0             = as.numeric(ajuste$coefficients[1]),
                        beta1             = as.numeric(ajuste$coefficients[2]),
                        erro_padrao_beta0 = as.numeric(sumario$coefficients[,2][1]),
                        erro_padrao_beta1 = as.numeric(sumario$coefficients[,2][2]),
                        erro_padrao_medio = mean(ic_pred[,3] - ic_pred[,1]),
                        erro_padrao_max   = max(ic_pred[,3] - ic_pred[,1]),
                        determinante      = det(sumario$coefficients[,1] %*% t(sumario$coefficients[,1])))
  return(list(x, ajuste, medidas))
}


# APLICANDO OS AJUSTES NOS DIFERENTES DELINEAMENTOS DE X
ajustes <- map(dfs, fazer_ajuste)

# APENAS JUNTANDO UM ÚNICO DATAFRAME COM AS MÉTRICAS DE TODOS OS MODELOS
medidas <- ajustes %>%
  map(3) %>%
  bind_rows()

ajustes[[c(1,1)]]

par(mfrow=c(2,3))

plts <- map(1:6, function(i){
  ajustes[[c(i,1)]] %>%
    ggplot(aes(x=x,y=y)) +
      geom_point() +
      lims(x = c(-10,10), y=c(-20,20)) +
      geom_abline(slope = ajustes[[c(i,3)]]$beta1,
                  intercept = ajustes[[c(i,3)]]$beta0,
                  colour = 'red', linewidth = 0.6) +
      geom_abline(slope = 1, intercept = 1,
                  colour = 'blue', linewidth = 0.6) +
      theme_bw()
})
wrap_plots(plts, nrow = 2)

# AINDA FALTA:
# REALIZAR ESTUDO DE SIMULAÇÃO PARA MAIS AMOSTRAS
# REALIZAR MAIS DE UMA REPETIÇÃO DE CADA DELINEAMENTO
# FAZER AS DESCRITIVAS E CONCLUSÕES
