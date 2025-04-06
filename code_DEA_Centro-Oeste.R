################################################################################
# Artigo: Eficiência na Educação - Municípios
# Autore: Kelly Arruda Gomes & Yuri Cesar de Lima e Silva
# Produto: Rodando o DEA sul
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
      sigla %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "sul",
      sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      sigla %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      sigla %in% c("PR", "RS", "SC") ~ "Sul",
      sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      TRUE ~ NA_character_))

# Excluindo linhas com NA

dados_full <- dados[complete.cases(dados), ]

dados_full <- dados_full %>%
  filter(regiao == "Centro-Oeste")

################################################################################
## DEA 2019
################################################################################

# Rodando o DEA:
x2019 <- as.matrix(with(dados_full, desp_2019))

y2019 <- as.matrix(with(dados_full, ideb_2019))

dea.plot.frontier(x2019,y2019,txt=TRUE)

# Modelo

modelo2019 <- dea(x2019,y2019,RTS="vrs",ORIENTATION="out")

summary(modelo2019)

escores2019 <- data.frame(dados_full$nome,dados_full$sigla, eficiencia=1/(modelo2019=modelo2019$eff)*100)

escores2019 %>%
  arrange(desc(eficiencia))

modelo2019 <- dea(x2019,y2019,RTS="vrs",ORIENTATION="in")

benchmarks2019 <- data.frame(dados_full$nome,dados_full$sigla,modelo2019=modelo2019$lambda)

benchmarks2019

## Gráficos

colnames(escores2019)[colnames(escores2019) == "dados_full.nome"] <- "nome"
colnames(escores2019)[colnames(escores2019) == "dados_full.sigla"] <- "sigla"

dados_full <- dados_full %>%
  left_join(escores2019, by = c("sigla","nome"))

frontier <- dados_full %>%
  filter(eficiencia == 100) %>%   # substitua 'eficiencia' pelo nome da sua coluna
  arrange(desp_2019)


# Obtém os extremos:
min_x_base <- dados_full %>%
  filter(eficiencia == 100)                     # menor valor de x2019 dentre os eficientes
min_x <- min(min_x_base$desp_2019, na.rm = TRUE)

max_x_total <- max(dados_full$desp_2019, na.rm = TRUE)  # valor máximo de x2019 (pode ser da base completa)

min_y_base <- dados_full %>%
  filter(eficiencia == 100) %>%                # menor valor de x2019 dentre os eficientes
  filter(desp_2019 == min(desp_2019))
first_y <- min_y_base$ideb_2019                   # y do primeiro ponto eficiente

last_y_base <- dados_full %>%
  filter(eficiencia == 100) %>%
  filter(ideb_2019 == max(ideb_2019))

last_y <- last_y_base$ideb_2019

# Cria um novo dataframe com os pontos adicionais
frontier2 <- bind_rows(
  tibble(desp_2019 = min_x, ideb_2019 = 0),    # ponto que liga ao eixo x (mínimo)
  frontier,                          # pontos eficientes
  tibble(desp_2019 = max_x_total, ideb_2019 = last_y)  # extensão horizontal até o x máximo
)

# Gráfico
g2019 <- ggplot(dados_full, aes(x = desp_2019, y = ideb_2019)) +
  geom_point(aes(colour = sigla), size = 2) +
  geom_line(data = frontier2, aes(x = desp_2019, y = ideb_2019),
            color = "red", size = 1) +
  geom_label_repel(data = filter(dados_full, eficiencia == 100),
                   aes(label = paste(nome, sigla, sep = " - ")),
                   fill = alpha("white", 0.5),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'red',
                   size = 3) +
  labs(
    x = "Despesa média por aluno (R$)",
    y = "IDEB",
    title = "DEA 2019",
    colour = "Estado"
  ) +
  theme_classic()

g2019

# BoxPlot
bp2019 <- ggplot(dados_full, aes(x = sigla, y = eficiencia, fill = sigla)) +
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
  labs(x = "Estado", y = "Eficiência", title = "Boxplot da Eficiência por Estado - 2019") +
  theme_classic() + guides(fill = FALSE)

bp2019
################################################################################
## DEA 2021
################################################################################

# Cuidando de um possível over point:
dados_full <- dados_full[dados_full$cod_ibge != 5212253, ]

# Rodando o DEA:
x2021 <- as.matrix(with(dados_full, desp_2021))

y2021 <- as.matrix(with(dados_full, ideb_2021))

dea.plot.frontier(x2021,y2021,txt=TRUE)

# Modelo

modelo2021 <- dea(x2021,y2021,RTS="vrs",ORIENTATION="out")

summary(modelo2021)

escores2021 <- data.frame(dados_full$nome,dados_full$sigla, eficiencia=1/(modelo2021=modelo2021$eff)*100)

escores2021 %>%
  arrange(desc(eficiencia))

modelo2021 <- dea(x2021,y2021,RTS="vrs",ORIENTATION="in")

benchmarks2021 <- data.frame(dados_full$nome,dados_full$sigla,modelo2021=modelo2021$lambda)

benchmarks2021

## Gráficos

colnames(escores2021)[colnames(escores2021) == "dados_full.nome"] <- "nome"
colnames(escores2021)[colnames(escores2021) == "dados_full.sigla"] <- "sigla"

dados_full <- dados_full %>%
  left_join(escores2021, by = c("sigla","nome"))

frontier <- dados_full %>%
  filter(eficiencia == 100) %>%   # substitua 'eficiencia' pelo nome da sua coluna
  arrange(desp_2021)


# Obtém os extremos:
min_x_base <- dados_full %>%
  filter(eficiencia == 100)                     # menor valor de x2019 dentre os eficientes
min_x <- min(min_x_base$desp_2021, na.rm = TRUE)

max_x_total <- max(dados_full$desp_2021, na.rm = TRUE)  # valor máximo de x2019 (pode ser da base completa)

min_y_base <- dados_full %>%
  filter(eficiencia == 100) %>%                # menor valor de x2019 dentre os eficientes
  filter(desp_2021 == min(desp_2021))
first_y <- min_y_base$ideb_2021                   # y do primeiro ponto eficiente

last_y_base <- dados_full %>%
  filter(eficiencia == 100) %>%
  filter(ideb_2021 == max(ideb_2021))

last_y <- last_y_base$ideb_2021

# Cria um novo dataframe com os pontos adicionais
frontier2 <- bind_rows(
  tibble(desp_2021 = min_x, ideb_2021 = 0),    # ponto que liga ao eixo x (mínimo)
  frontier,                          # pontos eficientes
  tibble(desp_2021 = max_x_total, ideb_2021 = last_y)  # extensão horizontal até o x máximo
)

# Gráfico
g2021 <- ggplot(dados_full, aes(x = desp_2021, y = ideb_2021)) +
  geom_point(aes(colour = sigla), size = 2) +
  geom_line(data = frontier2, aes(x = desp_2021, y = ideb_2021),
            color = "red", size = 1) +
  geom_label_repel(data = filter(dados_full, eficiencia == 100),
                   aes(label = paste(nome, sigla, sep = " - ")),
                   fill = alpha("white", 0.5),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'red',
                   size = 3) +
  labs(
    x = "Despesa média por aluno (R$)",
    y = "IDEB",
    title = "DEA 2021",
    colour = "Estado"
  ) +
  theme_classic()

g2021

# BoxPlot
bp2021 <- ggplot(dados_full, aes(x = sigla, y = eficiencia, fill = sigla)) +
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
  labs(x = "Estado", y = "Eficiência", title = "Boxplot da Eficiência por Estado - 2021") +
  theme_classic() + guides(fill = FALSE)

bp2021
################################################################################
## DEA 2023
################################################################################

# Cuidando de um possível over point:
dados_full <- dados_full[dados_full$cod_ibge != 3170701, ]

# Rodando o DEA:
x2023 <- as.matrix(with(dados_full, desp_2023))

y2023 <- as.matrix(with(dados_full, ideb_2023))

dea.plot.frontier(x2023,y2023,txt=TRUE)

# Modelo

modelo2023 <- dea(x2023,y2023,RTS="vrs",ORIENTATION="out")

summary(modelo2023)

escores2023 <- data.frame(dados_full$nome,dados_full$sigla, eficiencia=1/(modelo2023=modelo2023$eff)*100)

escores2023 %>%
  arrange(desc(eficiencia))

modelo2023 <- dea(x2023,y2023,RTS="vrs",ORIENTATION="in")

benchmarks2023 <- data.frame(dados_full$nome,dados_full$sigla,modelo2023=modelo2023$lambda)

benchmarks2023

## Gráficos

colnames(escores2023)[colnames(escores2023) == "dados_full.nome"] <- "nome"
colnames(escores2023)[colnames(escores2023) == "dados_full.sigla"] <- "sigla"

dados_full <- dados_full %>%
  left_join(escores2023, by = c("sigla","nome"))

frontier <- dados_full %>%
  filter(eficiencia == 100) %>%   # substitua 'eficiencia' pelo nome da sua coluna
  arrange(desp_2023)


# Obtém os extremos:
min_x_base <- dados_full %>%
  filter(eficiencia == 100)                     # menor valor de x2019 dentre os eficientes
min_x <- min(min_x_base$desp_2023, na.rm = TRUE)

max_x_total <- max(dados_full$desp_2023, na.rm = TRUE)  # valor máximo de x2019 (pode ser da base completa)

min_y_base <- dados_full %>%
  filter(eficiencia == 100) %>%                # menor valor de x2019 dentre os eficientes
  filter(desp_2023 == min(desp_2023))
first_y <- min_y_base$ideb_2023                   # y do primeiro ponto eficiente

last_y_base <- dados_full %>%
  filter(eficiencia == 100) %>%
  filter(ideb_2023 == max(ideb_2023))

last_y <- last_y_base$ideb_2023

# Cria um novo dataframe com os pontos adicionais
frontier2 <- bind_rows(
  tibble(desp_2023 = min_x, ideb_2023 = 0),    # ponto que liga ao eixo x (mínimo)
  frontier,                          # pontos eficientes
  tibble(desp_2023 = max_x_total, ideb_2023 = last_y)  # extensão horizontal até o x máximo
)

# Gráfico
g2023 <- ggplot(dados_full, aes(x = desp_2023, y = ideb_2023)) +
  geom_point(aes(colour = sigla), size = 2) +
  geom_line(data = frontier2, aes(x = desp_2023, y = ideb_2023),
            color = "red", size = 1) +
  geom_label_repel(data = filter(dados_full, eficiencia == 100),
                   aes(label = paste(nome, sigla, sep = " - ")),
                   fill = alpha("white", 0.5),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'red',
                   size = 3) +
  labs(
    x = "Despesa média por aluno (R$)",
    y = "IDEB",
    title = "DEA 2023",
    colour = "Estado"
  ) +
  theme_classic()

g2023

# BoxPlot
bp2023 <- ggplot(dados_full, aes(x = sigla, y = eficiencia, fill = sigla)) +
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
  labs(x = "Estado", y = "Eficiência", title = "Boxplot da Eficiência por Estado - 2023") +
  theme_classic() + guides(fill = FALSE)

bp2023
################################################################################
# Salvar bases
################################################################################

write.xlsx(escores2019, "results/escores_centro-oeste_2019.xlsx")
write.xlsx(benchmarks2019, "results/benchmarks_centro-oeste_2019.xlsx")

write.xlsx(escores2021, "results/escores_centro-oeste_2021.xlsx")
write.xlsx(benchmarks2021, "results/benchmarks_centro-oeste_2021.xlsx")

write.xlsx(escores2023, "results/escores_centro-oeste_2023.xlsx")
write.xlsx(benchmarks2023, "results/benchmarks_centro-oeste_2023.xlsx")

