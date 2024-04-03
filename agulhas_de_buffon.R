agulha <- function(params) {
  n = params[3]
  d = params[1]
  l = params[2]
  p=numeric(n)
  for(i in 1:n) {
    y <- runif(n*10, min = 0, max = d/2)
    th <- runif(n*10, min = 0, max = pi/2)
    p[i] <- (2 * (l/d))/mean(y < (l/2)*sin(th))
  }
  testet <- t.test(p, mu=pi)
  return(c(mean(p), sd(p), round(testet[[3]],6), testet[[4]][1], testet[[4]][2]))
}

valores <- data.frame(d = rep(1,100),
                      l = seq(0.05,1,by=0.05),
                      n = rep(x=1000, times=100))


c <- apply(valores, MARGIN=1, FUN=agulha)
valores$est <- c[1,]
valores$var_est <- c[2,]
valores$pvalue <- c[3,]
valores$li <- c[4,]
valores$ls <- c[5,]

valores %>%
  arrange(l)


valores %>%
  ggplot(aes(x=l, y=var_est)) +
  theme_bw() +
  geom_point()

valores %>%
  ggplot(aes(x=l, y=est)) +
    theme_bw() +
    ylim(pi-0.01, pi+0.01) +
    geom_point() +
    geom_abline(intercept = pi, slope=0, col='red')
