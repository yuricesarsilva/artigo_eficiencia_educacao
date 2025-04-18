################################################################################
# Artigo: Eficiência na Educação - Municípios
# Autore: Kelly Arruda Gomes & Yuri Cesar de Lima e Silva
# Produto: Estatísticas Descritivas
################################################################################

# Fechar dados & gráficos
rm(list=ls())
graphics.off()

library(readxl)
library(dplyr)
library(tidyr)


# I) Importar e preparar
dados_full <- read_excel("dados/dados_finais.xlsx") %>%
  mutate(
    desp_2019 = (desp_ef_2019 / mat_ef_2019) * prop_efai_2019,
    desp_2021 = (desp_ef_2021 / mat_ef_2021) * prop_efai_2021,
    desp_2023 = (desp_ef_2023 / mat_ef_2023) * prop_efai_2023,
    regiao = case_when(
      sigla %in% c("AC","AP","AM","PA","RO","RR","TO")         ~ "Norte",
      sigla %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
      sigla %in% c("ES","MG","RJ","SP")                        ~ "Sudeste",
      sigla %in% c("PR","RS","SC")                            ~ "Sul",
      sigla %in% c("DF","GO","MT","MS")                       ~ "Centro-Oeste",
      TRUE                                                    ~ NA_character_
    )
  ) %>%
  drop_na(desp_2019, desp_2021, desp_2023,
          ideb_2019, ideb_2021, ideb_2023)

# II) Estatística descritiva por região e variável
desc_stats <- dados_full %>%
  pivot_longer(
    cols      = matches("^(desp|ideb)_\\d{4}$"),
    names_to  = c("variavel", "ano"),
    names_sep = "_",
    values_to = "valor"
  ) %>%
  group_by(regiao, variavel, ano) %>%
  summarise(
    n       = length(valor),
    media   = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    dp      = sd(valor, na.rm = TRUE),
    minimo  = min(valor, na.rm = TRUE),
    maximo  = max(valor, na.rm = TRUE),
    IQR     = IQR(valor, na.rm = TRUE),
    .groups = "drop"
  )

# Visualizar
print(desc_stats)

############################

library(dplyr)
library(tidyr)

# Supondo que 'dados_full' já contém a coluna regiao e as variáveis desp_2019, desp_2021, desp_2023, ideb_2019, ideb_2021, ideb_2023

df_resultados <- dados_full %>%
  # 1) "empilha" os anos e variáveis em colunas
  pivot_longer(
    cols      = matches("^(desp|ideb)_\\d{4}$"),
    names_to  = c("variavel", "ano"),
    names_sep = "_",
    values_to = "valor"
  ) %>%
  # 2) ajusta os nomes para apresentação
  mutate(
    variavel = dplyr::recode(variavel,
                             desp = "Despesa por aluno",
                             ideb = "IDEB"),
    ano = as.integer(ano)
  ) %>%
  # 3) calcula estatísticas por região × ano × variável
  dplyr::group_by(regiao, ano, variavel) %>%
  dplyr::summarise(
    Média          = mean(valor, na.rm = TRUE),
    Mediana        = median(valor, na.rm = TRUE),
    Mínimo         = min(valor, na.rm = TRUE),
    Máximo         = max(valor, na.rm = TRUE),
    `Desvio-Padrão` = sd(valor, na.rm = TRUE),
    .groups = "drop"
  )

# Veja o resultado
print(df_resultados)

# 1) Converte para data.frame
summary_regiao_df <- as.data.frame(df_resultados)

# 2) Salva em arquivo .xls
library(xlsx)
write.xlsx(summary_regiao_df, "results/estatisticas_descritivas.xlsx")
