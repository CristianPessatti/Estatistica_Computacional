# Limpando Environment
rm(list = ls())

# Bibliotecas
require(tidyverse)

# Fontes
source('trabalho_regressao/gen_data.R')
source('trabalho_regressao/plot_all.R')
source('trabalho_regressao/metrics.R')

# Gerando dados
dados <- gen_data()

# Plotando com um dataframe aleatório de cada delineamento
# plot_all(dados)

# Pegando as métricas
resultado_ajustes <- get_coefficients(dados)
resultado_ajustes$metrics

# Plotando boxplots dos coeficientes
resultado_ajustes$coefficients %>%
  boxplot_beta0(sigmas = 1:3)

resultado_ajustes$coefficients %>%
  boxplot_beta1(sigmas = 1:3)
