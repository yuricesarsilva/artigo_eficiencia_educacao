################################################################################
# Artigo: Eficiência na Educação - Municípios
# Autore: Kelly Arruda Gomes & Yuri Cesar de Lima e Silva
# Produto: Rodando o DEA
################################################################################

# Fechar dados & gráficos
rm(list=ls())
graphics.off()

# Pacotes utilizados:
library(ggplot2)
library(dplyr)
library(readxl)
library(plyr)
library(deaR)
library(Benchmarking)
library(ggridges)
library(ggrepel)
library(patchwork)
library(magrittr)
library(grid)
library(lpSolveAPI)
library(ucminf)
library(quadprog)

# I) importando dados:

dados <- read_excel("dados/dados_finais.xlsx") 

# II) Organizando os dados:

dados <- dados %>%
  mutate(desp_2019 = (desp_ef_2019/mat_ef_2019)*prop_efai_2019,
         desp_2021 = (desp_ef_2021/mat_ef_2021)*prop_efai_2021,
         desp_2023 = (desp_ef_2023/mat_ef_2023)*prop_efai_2023)


dados <- dados %>%
  mutate(
    regiao = case_when(
      sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      sigla %in% c("PR", "RS", "SC") ~ "Sul",
      sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      TRUE ~ NA_character_))

# Excluindo linhas com NA

dados_full <- dados[complete.cases(dados), ]


# Ajustando a base para rodar o Índice de Malmquist
dados_mq <- dados_full %>%
  select(cod_ibge,
         desp_2019, ideb_2019,
         desp_2021, ideb_2021,
         desp_2023, ideb_2023)

data_mq <- make_malmquist(datadea = dados_mq,
                          nper = 3,
                          arrangement = "horizontal",
                          inputs = 2,
                          outputs = 3)

# Rodando o Índice de Malmquist
result <- malmquist_index(data_mq,
                          orientation = "oo",
                          rts = "vrs",
                          type1 = "seq",
                          tc_vrs = TRUE)

# Resultados

mi <- data.frame(t(result$mi))
mi <- rownames_to_column(mi, var = "nome")
colnames(mi)[colnames(mi) == "Period.2"] <- "mi_19.21"
colnames(mi)[colnames(mi) == "Period.3"] <- "mi_21.23"
colnames(mi)[colnames(mi) == "nome"] <- "cod_ibge"
mi$cod_ibge <- sub("^DMU", "", mi$cod_ibge)
mi$cod_ibge <- as.numeric(mi$cod_ibge)


pech <- data.frame(t(result$pech))
pech <- rownames_to_column(pech, var = "nome")
colnames(pech)[colnames(pech) == "Period.2"] <- "pech_19.21"
colnames(pech)[colnames(pech) == "Period.3"] <- "pech_21.23"
colnames(pech)[colnames(pech) == "nome"] <- "cod_ibge"
pech$cod_ibge <- sub("^DMU", "", pech$cod_ibge)
pech$cod_ibge <- as.numeric(pech$cod_ibge)

sech <- data.frame(t(result$sech))
sech <- rownames_to_column(sech, var = "nome")
colnames(sech)[colnames(sech) == "Period.2"] <- "sech_19.21"
colnames(sech)[colnames(sech) == "Period.3"] <- "sech_21.23"
colnames(sech)[colnames(sech) == "nome"] <- "cod_ibge"
sech$cod_ibge <- sub("^DMU", "", sech$cod_ibge)
sech$cod_ibge <- as.numeric(sech$cod_ibge)

mq_final <- dados_full %>%
  left_join(mi, by = c("cod_ibge")) %>%
  left_join(pech, by = c("cod_ibge")) %>%
  left_join(sech, by = c("cod_ibge"))

################################################################################
# BoxPlot dos resultados:

# mi
mi_19.21 <- mq_final %>%
  filter(mi_19.21 < 2) %>%
  ggplot(aes(x = regiao, y = mi_19.21, fill = regiao)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", 
               aes(label = round(..y.., 1)),
               vjust = -0.5,
               color = "black", 
               size = 3) +
  stat_summary(fun = function(x) quantile(x, probs = 0.25), geom = "text", 
               aes(label = round(..y.., 1)),
               hjust = -1.8, vjust = 1.2, color = "black", size = 2) +
  stat_summary(fun = function(x) quantile(x, probs = 0.75), geom = "text", 
               aes(label = round(..y.., 1)),
               hjust = -1.8, vjust = -0.5, color = "black", size = 2) +
  labs(x = "Região", y = "Índice de Malmquist", title = "Boxplot do Índice de Mamlquist por Região - 2019/2021") +
  theme_classic() + guides(fill = FALSE)

mi_19.21

mi_21.23 <- mq_final %>%
  filter(mi_21.23 < 2) %>%
  ggplot(aes(x = regiao, y = mi_21.23, fill = regiao)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", 
               aes(label = round(..y.., 1)),
               vjust = -0.5,
               color = "black", 
               size = 3) +
  stat_summary(fun = function(x) quantile(x, probs = 0.25), geom = "text", 
               aes(label = round(..y.., 1)),
               hjust = -1.8, vjust = 1.2, color = "black", size = 2) +
  stat_summary(fun = function(x) quantile(x, probs = 0.75), geom = "text", 
               aes(label = round(..y.., 1)),
               hjust = -1.8, vjust = -0.5, color = "black", size = 2) +
  labs(x = "Região", y = "Índice de Malmquist", title = "Boxplot do Índice de Mamlquist por Região - 2021/2023") +
  theme_classic() + guides(fill = FALSE)

mi_21.23

################################################################################

# Calcular a média da eficiência por estado
mq_estado_19.21 <- mq_final %>%
  group_by(sigla) %>%
  dplyr::summarise(media_mi_19.21 = mean(mi_19.21, na.rm = TRUE))

# Criar o gráfico de barras com a média de eficiência
ggplot(mq_estado_19.21, aes(x = reorder(sigla, media_mi_19.21), y = media_mi_19.21)) +
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +  # Inverte os eixos para melhor visualização dos rótulos
  labs(title = "Média do Índice de Malmquist por Estado - 2019/2021",
       x = "Estado",
       y = "Índice de Malmquist") +
  theme_minimal()


# Calcular a média da eficiência por estado
mq_estado_21.23 <- mq_final %>%
  group_by(sigla) %>%
  dplyr::summarise(media_mi_21.23 = mean(mi_21.23, na.rm = TRUE))

# Criar o gráfico de barras com a média de eficiência
ggplot(mq_estado_21.23, aes(x = reorder(sigla, media_mi_21.23), y = media_mi_21.23)) +
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +  # Inverte os eixos para melhor visualização dos rótulos
  labs(title = "Média do Índice de Malmquist por Estado - 2021/2023",
       x = "Estado",
       y = "Índice de Malmquist") +
  theme_minimal()

################################################################################
# Salvar base
write.xlsx(mq_final, "results/malmquist_full.xlsx")
write.xlsx(mq_estado_19.21, "results/malmquist_estado_19.21.xlsx")
write.xlsx(mq_estado_21.23, "results/malmquist_estado_21.23.xlsx")
