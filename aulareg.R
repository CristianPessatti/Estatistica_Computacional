x <- runif(100,0,10)
e <- rnorm(100, mean=0, sd=3)

y <- 5 + 3*x + e

data <- data.frame(X = x,
                   Y = y)

fit <- lm(Y~., data = data)
fit
abline(a=4.328,b=3.081, col='red')

data %>%
  ggplot(aes(x=X, y=Y)) +
    geom_point() +
    geom_smooth(method='lm', col='red', se=F)
