n <- 80
p <- 0.12
m <- 1000

gerar_un_amostral <- function(n,p) {
  y <- rbinom(1,size = n,prob = p)

  p_hat <- y/n
  sd_hat <- sqrt(p_hat*(1-p_hat)/n)

  ic <- p_hat + c(-1,1)*1.96*sd_hat
  in_bet <- prod(sign(p-ic)) < 0

  ic_conserv <- p_hat + c(-1,1)*1.96*(sqrt((1/4)/n))
  in_bet_conserv <- prod(sign(p-ic_conserv)) < 0

  return(c(p_hat, in_bet, in_bet_conserv))
}

dados <- data.frame(t(replicate(100,gerar_un_amostral(80,0.12))))
names(dados) <- c('p_estimado', 'in_ic', 'in_ic_conserv')

colMeans(dados)
