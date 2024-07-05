dataset <- dados$eq.space.1[[1]]

model <- resultado_ajustes$ajustes$eq.space.1[[1]]

new_x_values <- data.frame(x = seq(from = -10, to = 10, by = 0.2))

preds <- predict(model, newdata = new_x_values, interval = "prediction") %>% as.data.frame()

max_int <- max(preds$upr - preds$lwr)
mean_int <- mean(preds$upr - preds$lwr)
