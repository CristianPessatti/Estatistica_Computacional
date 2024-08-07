require(tidyverse)
require(patchwork)

# GERAR AMOSTRAS ASSUMINDO ERRO ~ N(0,sigma2)
gen_amostras <- function(x, beta0 = 1, beta1 = 1, sigma2) {
  y <-  beta0 + beta1 * x + rnorm(length(x), mean = 0, sd = sigma2)
  return(y)
}

gen_data <- function(names = c('random', 'eq.space', 'two.each.rand', 'two.each.space', 'quarter.space', 'min.max'),
                     sigmas = 1:3,
                     n = 12,
                     A = 100){


  # Nomes para cada delineamento -----------------------------------------------
  names_deli <- names

  # Criando Nomes para as listas combinando os delineamentos com os sigmas --------
  names_listas2 <- apply(expand.grid(names_deli, sigmas), MARGIN=1,
                         FUN = function(x) paste0(x, collapse = '.')) %>% sort()

  # Declarando lista ----------------------------------------------------------
  dfs <- list()

  # Criando os dataframes -----------------------------------------------------
  for(i in 1:length(sigmas)) {
    dfs[[i]] <- map(1:A, function(r) {
      df <- data.frame(x = seq(from=-10, to=10, length.out=n))
      df$y <- gen_amostras(df$x, sigma2 = sigmas[i])
      return(df)
    })
  }

  for(i in (length(sigmas)+1):(length(sigmas)*2)) {
    dfs[[i]] <- map(1:A, function(r) {
      df <- data.frame(x = c(rep(-10, n/2), rep(10, n/2)))
      df$y <- gen_amostras(df$x, sigma2 = sigmas[i-(length(sigmas))])
      return(df)
    })
  }

  for(i in (length(sigmas)*2+1):(length(sigmas)*3)) {
    dfs[[i]] <- map(1:A, function(r) {
      df <- data.frame(x = rep(seq(from=-10,to=10,length.out=4), n*(1/4))) %>% arrange(x)
      df$y <- gen_amostras(df$x, sigma2 = sigmas[i-(length(sigmas)*2)])
      return(df)
    })
  }

  for(i in (length(sigmas)*3+1):(length(sigmas)*4)) {
    dfs[[i]] <- map(1:A, function(r) {
      df <- data.frame(x = runif(n, -10, 10))
      df$y <- gen_amostras(df$x, sigma2 = sigmas[i-(length(sigmas)*3)])
      return(df)
    })
  }

  for(i in (length(sigmas)*4+1):(length(sigmas)*5)) {
    dfs[[i]] <- map(1:A, function(r) {
      df <- data.frame(x = rep(runif(n/2,-10,10), 2)) %>% arrange(x)
      df$y <- gen_amostras(df$x, sigma2 = sigmas[i-(length(sigmas)*4)])
      return(df)
    })
  }

  for(i in (length(sigmas)*5+1):(length(sigmas)*6)) {
    dfs[[i]] <- map(1:A, function(r) {
      df <- data.frame(x = rep(rep(seq(from=-10,to=10,length.out=n/2),2))) %>% arrange(x)
      df$y <- gen_amostras(df$x, sigma2 = sigmas[i-(length(sigmas)*5)])
      return(df)
    })
  }

  # Atribuindo nomes as listas ------------------------------------------------
  return(setNames(dfs, names_listas2))
}
