library(tidyverse)
library(readr)

df <- data.table::fread("perfil_comparecimento_abstencao_2022.csv", sep = ";",
                        encoding = "Latin-1", showProgress = TRUE)
municipios_tse <- read_csv("municipios_brasileiros_tse.csv")
passe_livre <- read_csv("passe_livre.csv")

df <- df %>% 
  select(ano = ANO_ELEICAO, turno = NR_TURNO, municipio = CD_MUNICIPIO, 
         faixa_etaria = DS_FAIXA_ETARIA, aptos = QT_APTOS, abstencoes = QT_ABSTENCAO) %>% 
  group_by(ano, turno, municipio, faixa_etaria) %>% 
  summarise(aptos = sum(aptos),
            abstencoes = sum(abstencoes))

df <- df %>% 
  mutate(tx_abstencao = abstencoes / aptos) %>% 
  select(-aptos, -abstencoes) %>% 
  pivot_wider(names_from = faixa_etaria, values_from = tx_abstencao) %>% 
  mutate(across(everything(), ~ . - `65 a 69 anos`))

df <- df %>% 
  left_join(municipios_tse %>% 
              select(codigo_tse, nome_municipio, codigo_ibge), 
            by = c("municipio" = "codigo_tse")) %>% 
  left_join(passe_livre %>% select(-ano),
            by = c("codigo_ibge" = "id_municipio", "turno"))

df <- df %>% 
  mutate(uf = substr(codigo_ibge, 1, 2))

write.csv(df, file = "df.csv")

lm(`60 a 64 anos` ~ passe_livre + factor(uf), data = df) %>% 
  summary(.)

lm(`55 a 59 anos` ~ passe_livre + factor(uf), data = df) %>% 
  summary(.)
