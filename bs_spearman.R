reamostrar <- function(amostra) {
  idx <- sample(1:nrow(amostra), nrow(amostra), replace = TRUE)

  return(amostra[idx,])
}

calcular_spearman <- function(df) {
  return(cor(dados[,1], dados[,2], method='spearman'))
}

x <- rnorm(100,1.74, 2)
y <- 40 + 20*x + rnorm(100, sd=5)

dados <- data.frame(X = x, Y = y)
reamostrar(dados)
