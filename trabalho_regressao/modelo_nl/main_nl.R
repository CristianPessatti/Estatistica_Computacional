# Limpando Environment
rm(list = ls())

# Bibliotecas
require(tidyverse)
require(patchwork)

# Fontes
source('trabalho_regressao/modelo_nl/gen_data_nl.R')
source('trabalho_regressao/modelo_nl/plots_nl.R')
source('trabalho_regressao/modelo_nl/metrics_nl.R')
source('trabalho_regressao/modelo_nl/ajustar_modelos.R')

dados <- gen_data_nl()

resultado_ajustes <- ajustar_mods_nl(dados)

#plot_all_nl(dados, c(10,2))

coeficientes <- get_coefficients_nl(resultado_ajustes$coefs)

plot_intervalo_pred(dados, resultado_ajustes$mods)

intervalos <- get_pred_int(dados, resultado_ajustes$mods) %>%
  flatten() %>%
  bind_rows() %>%
  group_by(delim) %>%
  summarise(max_int = max(max_int), mean_int = mean(mean_int))

completo <- left_join(coeficientes, intervalos, by = "delim")

