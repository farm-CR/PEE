library(tidyverse)
library(readxl)
library(readr)

comparecimento <- bind_rows(list(
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2022.csv"),
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2018.csv"),
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2014.csv"))) %>% 
  select(ano = ANO_ELEICAO, turno = NR_TURNO, id_municipio_tse = CD_MUNICIPIO, 
         faixa_etaria = DS_FAIXA_ETARIA, aptos = QT_APTOS, comparecimento = QT_COMPARECIMENTO) %>% 
  mutate(faixa_etaria = fct_collapse(faixa_etaria,
                                     "18 a 20 anos" = c("20 anos", "19 anos", "18 anos"),
                                     "16 e 17 anos" = c("17 anos", "16 anos")),
         faixa_etaria = fct_relevel(faixa_etaria, "100 anos ou mais", after = Inf)) %>% 
  group_by(ano, turno, id_municipio_tse, faixa_etaria) %>% 
  summarize(aptos = sum(aptos),
            comparecimento = sum(comparecimento)) %>% 
  mutate(tx_comparecimento = comparecimento / aptos)

resultado <- bind_rows(list(
  read_csv("dados/resultado/votacao_candidato_munzona_2014_BRASIL.csv"),
  read_csv("dados/resultado/votacao_candidato_munzona_2018_BRASIL.csv"),
  read_csv("dados/resultado/votacao_candidato_munzona_2022_BRASIL.csv"))) %>% 
  mutate(DS_SIT_TOT_TURNO = ifelse(DS_SIT_TOT_TURNO == "ELEITO", "eleito", "naoeleito")) %>% 
  pivot_wider(names_from = DS_SIT_TOT_TURNO,
              values_from = QT_VOTOS_NOMINAIS,
              values_fn = {mean}) %>% 
  mutate(k = 1 / abs(log(eleito / naoeleito))) %>% 
  select(ano = ANO_ELEICAO, turno = NR_TURNO, id_municipio_tse = CD_MUNICIPIO, 
         competitividade = k)

municipios_tse <- read_csv("dados/municipios_brasileiros_tse.csv") %>% 
  select(id_municipio_tse = codigo_tse, id_municipio = codigo_ibge)

passe_livre <- read_csv("dados/passe_livre.csv")

df <- comparecimento  %>% 
  left_join(resultado) %>% 
  left_join(municipios_tse) %>% 
  left_join(
    bind_rows(list(
      passe_livre,
      passe_livre %>% mutate(ano = 2018, passe_livre = 0),
      passe_livre %>% mutate(ano = 2014, passe_livre = 0)))) %>% 
  mutate(across(id_municipio, as.character)) %>% 
  drop_na()

# Salvar df final
save(df, file = "dados/comparecimento.RData")

