df <- data.frame(x = c(rep(1, 25), rep(50, 25)))
df <- df %>%
  mutate(y = sin(0.2 * x)/(4 + x) + rnorm(length(x), mean = 0, sd = 0.01))

par(mfrow = c(1, 1))

df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  stat_function(fun = function(x) sin(0.2*x)/(2 + x), colour = 'tomato', size = 0.8) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

fit <- nls(y ~ sin(beta1*x)/(beta2 + x), data = df, start = list(beta1 = 0.18, beta2 = 3.5), control = nls.control(maxiter = 10000))

ndf <- data.frame('x' = seq(0, 50, by = 0.1))
betas <- c(0.5,4)

ndf$fit <- predict(fit, newdata = ndf)

# Calcular a matriz de covariância dos parâmetros ajustados
cov_matrix <- vcov(fit)

# Função para calcular os erros padrão das previsões
predict_se <- function(x, beta1, beta2, cov_matrix) {
  grad <- c(x / (beta2 + x), -beta1 * x / (beta2 + x)^2)
  sqrt(t(grad) %*% cov_matrix %*% grad)
}

# Calcular os intervalos de confiança manualmente
alpha <- 0.05
t_value <- qt(1 - alpha / 2, df.residual(fit))

# Calcular os erros padrão das previsões
ndf$se <- apply(ndf, 1, function(row) predict_se(row['x'], coef(fit)[1], coef(fit)[2], cov_matrix))

# Calcular os intervalos de confiança
ndf$lwr_conf <- ndf$fit - t_value * ndf$se
ndf$upr_conf <- ndf$fit + t_value * ndf$se

# Calcular os erros residuais padrão
residual_se <- sqrt(sum(residuals(fit)^2) / df.residual(fit))

# Calcular os intervalos de predição
ndf$lwr_pred <- ndf$fit - t_value * sqrt(ndf$se^2 + residual_se^2)
ndf$upr_pred <- ndf$fit + t_value * sqrt(ndf$se^2 + residual_se^2)


plt1 <- ggplot(ndf, aes(x = x, y = fit)) +
  geom_line(colour = 'tomato', linewidth = 0.8) +
  geom_ribbon(aes(ymin = lwr_pred, ymax = upr_pred), alpha = 0.2, colour = 'steelblue', fill = 'steelblue') +
  stat_function(fun = function(x) sin(betas[1]*x)/(betas[2] + x), colour = 'gold', size = 0.8) +
  geom_point(data = df, aes(x = x, y = y)) +
  theme_minimal()

df <- data.frame(x = runif(50, 0, 50))
df <- df %>%
  mutate(y = sin(0.2 * x)/(4 + x) + rnorm(length(x), mean = 0, sd = 0.01))

par(mfrow = c(1, 1))

df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  stat_function(fun = function(x) sin(0.2*x)/(4 + x), colour = 'tomato', size = 0.8) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

fit <- nls(y ~ sin(beta1*x)/(beta2 + x), data = df, start = list(beta1 = 0.18, beta2 = 3.5), control = nls.control(maxiter = 1000))

ndf <- data.frame('x' = seq(0, 50, by = 0.1))
betas <- c(0.2,4)

ndf$fit <- predict(fit, newdata = ndf)

# Calcular a matriz de covariância dos parâmetros ajustados
cov_matrix <- vcov(fit)

# Função para calcular os erros padrão das previsões
predict_se <- function(x, beta1, beta2, cov_matrix) {
  grad <- c(x / (beta2 + x), -beta1 * x / (beta2 + x)^2)
  sqrt(t(grad) %*% cov_matrix %*% grad)
}

# Calcular os intervalos de confiança manualmente
alpha <- 0.05
t_value <- qt(1 - alpha / 2, df.residual(fit))

# Calcular os erros padrão das previsões
ndf$se <- apply(ndf, 1, function(row) predict_se(row['x'], coef(fit)[1], coef(fit)[2], cov_matrix))

# Calcular os intervalos de confiança
ndf$lwr_conf <- ndf$fit - t_value * ndf$se
ndf$upr_conf <- ndf$fit + t_value * ndf$se

# Calcular os erros residuais padrão
residual_se <- sqrt(sum(residuals(fit)^2) / df.residual(fit))

# Calcular os intervalos de predição
ndf$lwr_pred <- ndf$fit - t_value * sqrt(ndf$se^2 + residual_se^2)
ndf$upr_pred <- ndf$fit + t_value * sqrt(ndf$se^2 + residual_se^2)


plt2 <- ggplot(ndf, aes(x = x, y = fit)) +
  geom_line(colour = 'tomato', linewidth = 0.8) +
  geom_ribbon(aes(ymin = lwr_pred, ymax = upr_pred), alpha = 0.2, colour = 'steelblue', fill = 'steelblue') +
  stat_function(fun = function(x) sin(betas[1]*x)/(betas[2] + x), colour = 'gold', size = 0.8) +
  geom_point(data = df, aes(x = x, y = y)) +
  theme_minimal()

wrap_plots(list(plt1, plt2), nrow = 2)
