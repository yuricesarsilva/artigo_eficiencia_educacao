################################################################################
# Artigo: Eficiência na Educação - Municípios
# Autore: Kelly Arruda Gomes & Yuri Cesar de Lima e Silva
# Produto: Ajuste da base de dados
################################################################################

# Fechar dados & gráficos
rm(list=ls())
graphics.off()

# Pacotes utilizados:
library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)

# I) importando dados:

desp_ef_2019 <- read_excel("dados/desp_ef_2019.xlsx") 
desp_ef_2021 <- read_excel("dados/desp_ef_2021.xlsx") 
desp_ef_2023 <- read_excel("dados/desp_ef_2023.xlsx") 

ideb <- read_excel("dados/ideb.xlsx") 

matriculas_2019 <- read_excel("dados/matriculas_2019.xlsx") 
matriculas_2021 <- read_excel("dados/matriculas_2021.xlsx") 
matriculas_2023 <- read_excel("dados/matriculas_2023.xlsx") 

pib_2019 <- read_excel("dados/pib2019.xlsx") 
pib_2021 <- read_excel("dados/pib2021.xlsx") 

populacao <- read_excel("dados/populacao2019_2021.xlsx") 

# Juntando as bases:
dados <- ideb %>%
  left_join(desp_ef_2019, by = c("cod_ibge")) %>%
  left_join(desp_ef_2021, by = c("cod_ibge")) %>%
  left_join(desp_ef_2023, by = c("cod_ibge")) %>%
  left_join(matriculas_2019, by = c("cod_ibge")) %>%
  left_join(matriculas_2021, by = c("cod_ibge")) %>%
  left_join(matriculas_2023, by = c("cod_ibge")) %>%
  left_join(pib_2019, by = c("cod_ibge")) %>%
  left_join(pib_2021, by = c("cod_ibge")) %>%
  left_join(populacao, by = c("cod_ibge"))

# Estatísticas descritivas
summary(dados)

linhas_sem_na <- sum(complete.cases(dados))
print(linhas_sem_na)

# Separando dados por ano:
dados_2019 <- dados %>% 
  select(-ideb_2021, -ideb_2023, -desp_ef_2021, -desp_ef_2023, -mat_ef_2021, -mat_ef_2023, -mat_efai_2021, -mat_efai_2023, -prop_efai_2021, -prop_efai_2023, -pib_2021, -pop_2021)

dados_2021 <- dados %>% 
  select(-ideb_2019, -ideb_2023, -desp_ef_2019, -desp_ef_2023, -mat_ef_2019, -mat_ef_2023, -mat_efai_2019, -mat_efai_2023, -prop_efai_2019, -prop_efai_2023, -pib_2019, -pop_2019)

dados_2023 <- dados %>% 
  select(-ideb_2019, -ideb_2021, -desp_ef_2019, -desp_ef_2021, -mat_ef_2019, -mat_ef_2021, -mat_efai_2019, -mat_efai_2021, -prop_efai_2019, -prop_efai_2021, -pib_2019, -pop_2019, -pib_2021, -pop_2021)

# Salvar bases

write.xlsx(dados, "dados/dados_finais.xlsx")
write.xlsx(dados_2019, "dados/dados_finais_2019.xlsx")
write.xlsx(dados_2021, "dados/dados_finais_2021.xlsx")
write.xlsx(dados_2023, "dados/dados_finais_2023.xlsx")
