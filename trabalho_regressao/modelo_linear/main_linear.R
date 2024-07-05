# Limpando Environment
rm(list = ls())

# Bibliotecas
require(tidyverse)

# Fontes
source('trabalho_regressao/modelo_linear/gen_data_linear.R')
source('trabalho_regressao/modelo_linear/plots_linear.R')
source('trabalho_regressao/modelo_linear/metrics_linear.R')
source('trabalho_regressao/modelo_linear/ajustar_modelos.R')

# Gerando dados
dados <- gen_data(sigmas = 1:3)

plot_all(dados)

# Realizando ajustes
resultado_ajustes <- ajustar_modelos(dados)

# Pegando os coeficientes
coeficientes <- get_coefficients(resultado_ajustes$coefs)

plot_intervalo_pred(dados, resultado_ajustes$ajustes)

resultados <- get_pred_int_linear(dados, resultado_ajustes$ajustes) %>%
  flatten() %>%
  bind_rows() %>%
  group_by(delim) %>%
  summarise(max_int = mean(max_int),
            mean_int = mean(mean_int)) %>%
  left_join(coeficientes$metrics, . ,by = 'delim') %>%
  mutate(sigma = str_extract(delim, "\\d$") %>% as.numeric(),
         delim = substr(delim, 1, nchar(delim) - 2) %>% as.factor())

# Modelo de interação
obj <- plot_anova_resultados(resultados)
obj$grafico

# SEPARANDO ESSA PORRA -----------------------------------------

dados2 <- gen_data(sigmas = 1:9)

resultado_ajustes2 <- ajustar_modelos(dados2)

# Pegando os coeficientes
coeficientes2 <- get_coefficients(resultado_ajustes2$coefs)

resultados2 <- get_pred_int_linear(dados2, resultado_ajustes2$ajustes) %>%
  flatten() %>%
  bind_rows() %>%
  group_by(delim) %>%
  summarise(max_int = mean(max_int),
            mean_int = mean(mean_int)) %>%
  left_join(coeficientes2$metrics, . ,by = 'delim') %>%
  mutate(sigma = str_extract(delim, "\\d$") %>% as.numeric(),
         delim = substr(delim, 1, nchar(delim) - 2) %>% as.factor())

obj2 <- plot_anova_resultados(resultados2)
obj2$grafico
