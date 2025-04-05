################################################################################
# Artigo: Eficiência na Educação - Municípios
# Autore: Kelly Arruda Gomes & Yuri Cesar de Lima e Silva
# Produto: Rodando o DEA
################################################################################

# Fechar dados & gráficos
rm(list=ls())
graphics.off()

# Pacotes utilizados:
library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(deaR)

# I) importando dados:

dados <- read_excel("dados/dados_finais.xlsx") 

# II) Organizando os dados:

dados <- dados %>%
  mutate(desp_2019 = desp_ef_2019/mat_efai_2019,
         desp_2021 = desp_ef_2021/mat_efai_2021,
         desp_2023 = desp_ef_2023/mat_efai_2023)

# II) Rodando o DEA:
x2019 <- as.matrix(with(dados, desp_2019))

y2019 <- as.matrix(with(dados, ideb_2019))


dea.plot.frontier(x2019,y2019,txt=TRUE)

#modelos

modelo2019 <- dea(x2019,y2019,RTS="vrs",ORIENTATION="out")

efficiencies(modelo2019)

eff <- (efficiencies(modelo2019))

1/eff

summary(modelo2019)

escores2019 <- data.frame(dados$Capitais,eficiencia=1/(modelo2019=modelo2019$eff)*100)

escores2019 %>%
  arrange(desc(eficiencia))

modelo2019 <- dea(x2019,y2019,RTS="vrs",ORIENTATION="out")

benchmarks2019 <- data.frame(dados$Capitais,modelo2019=modelo2019$lambda)

benchmarks2019
