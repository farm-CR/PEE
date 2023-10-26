library(tidyverse)
library(readr)

df <- data.table::fread("dados/ignore/perfil_comparecimento_abstencao_2022.csv", sep = ";",
                        encoding = "Latin-1", showProgress = TRUE)
municipios_tse <- read_csv("dados/municipios_brasileiros_tse.csv")
passe_livre <- read_csv("dados/passe_livre.csv")

df <- df_original %>% 
  select(ano = ANO_ELEICAO, turno = NR_TURNO, municipio = CD_MUNICIPIO, 
         faixa_etaria = DS_FAIXA_ETARIA, aptos = QT_APTOS, abstencoes = QT_ABSTENCAO) %>% 
  group_by(ano, turno, municipio, faixa_etaria) %>% 
  summarize(aptos = sum(aptos),
            abstencoes = sum(abstencoes)) %>%
  mutate(
    faixa_etaria = fct_collapse(
      faixa_etaria,
      "18 a 20 anos" = c("20 anos", "19 anos", "18 anos"),
      "16 e 17 anos" = c("17 anos", "16 anos")),
    faixa_etaria = fct_relevel(faixa_etaria, "100 anos ou mais", after = Inf))

#Análise descritiva
df %>% 
  filter(faixa_etaria != "Inválido") %>% 
  group_by(turno, faixa_etaria) %>% 
  summarize(aptos = sum(aptos), abstencoes = sum(abstencoes)) %>% 
  mutate(comparecimento = 1- abstencoes/aptos,
         comparecimento = ifelse(turno == 1, -comparecimento, comparecimento)) %>% 
  ggplot(aes(x = faixa_etaria, y = comparecimento, fill = factor(turno))) +
  geom_col() +
  # geom_hline(yintercept = -0.8371519, linetype = "dashed") +
  # geom_hline(yintercept = 0.8470657, linetype = "dashed") +
  coord_flip() +
  labs(x = "", y = "Comparecimento", fill = "Turno") 

ggsave("output/piramide.png", dpi = 600, height = 4, width = 5)

write.csv(df, "output/data-2022.csv")

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


  
