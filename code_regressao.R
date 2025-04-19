################################################################################
# Artigo: Eficiência na Educação - Municípios
# Autore: Kelly Arruda Gomes & Yuri Cesar de Lima e Silva
# Produto: Rodando a regressão 
################################################################################

# Fechar dados & gráficos
rm(list=ls())
graphics.off()

# Pacotes utilizados:
library(AER)

# Baixar dados DEA
dados2019 <- read_excel("dados/dados_full_2019.xlsx") 
dados2021 <- read_excel("dados/dados_full_2019.xlsx") 

dados2019 <- dados2019 %>%
  mutate(pibpc_2019 = pib_2019/pop_2019,
         pibpc_2021 = pib_2021/pop_2021)

dados2021 <- dados2021 %>%
  mutate(pibpc_2019 = pib_2019/pop_2019,
         pibpc_2021 = pib_2021/pop_2021)


# Regressão Linear Simples
modelo_ols_2019 <- lm(eficiencia ~ pibpc_2019, data = dados2019)
summary(modelo_ols_2019)

modelo_ols_2021 <- lm(eficiencia ~ pibpc_2021, data = dados2021)
summary(modelo_ols_2021)

# Modelo Tobit
modelo_tobit_2019 <- tobit(eficiencia ~ pibpc_2019, left = 0, right = 100, data = dados2019)
summary(modelo_tobit_2019)

modelo_tobit_2021 <- tobit(eficiencia ~ pibpc_2021, left = 0, right = 100, data = dados2021)
summary(modelo_tobit_2021)
