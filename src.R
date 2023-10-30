library(tidyverse)
library(readr)
library(MatchIt)

#Dados ----

df_original <- data.table::fread("dados/ignore/perfil_comparecimento_abstencao_2022.csv", sep = ";",
                        encoding = "Latin-1", showProgress = TRUE)

municipios_tse <- read_csv("dados/municipios_brasileiros_tse.csv") %>% 
  select(id_municipio_tse = codigo_tse, id_municipio = codigo_ibge)

passe_livre <- read_csv("dados/passe_livre.csv")

df <- df_original %>% 
  select(ano = ANO_ELEICAO, turno = NR_TURNO, id_municipio_tse = CD_MUNICIPIO, 
         faixa_etaria = DS_FAIXA_ETARIA, aptos = QT_APTOS, abstencoes = QT_ABSTENCAO) %>% 
  mutate(
    faixa_etaria = fct_collapse(
      faixa_etaria,
      "18 a 20 anos" = c("20 anos", "19 anos", "18 anos"),
      "16 e 17 anos" = c("17 anos", "16 anos")),
    faixa_etaria = fct_relevel(faixa_etaria, "100 anos ou mais", after = Inf)) %>% 
  group_by(ano, turno, id_municipio_tse, faixa_etaria) %>% 
  summarize(aptos = sum(aptos),
            abstencoes = sum(abstencoes)) %>% 
  left_join(municipios_tse) %>% 
  left_join(passe_livre)

#Análise descritiva ----

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

ggsave("output/piramide-2018.png", dpi = 600, height = 4, width = 5)

write.csv(df, "output/data-2022.csv")

#Propensity ----

df.psm <- read_csv("dados/psm.csv") %>% 
  left_join(passe_livre) %>% 
  filter(turno == 2)

modelo.psm <- passe_livre ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + populacao_urbana

#Balanceamento antes do PSM
df.psm %>% 
  mutate(ps = predict(glm(modelo.psm, data = ., family = binomial(link = "logit")), type = "response")) %>%  
  ggplot() +
  geom_density(aes(ps, fill = factor(passe_livre)), alpha = 1) +
  labs(x = "Propensity Score", y = "Densidade") + 
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0,1))

df.match <- matchit(modelo.psm, data=df.psm, link="probit", replace = T) %>% 
  get_matches(distance = "propscore", data=df.psm) %>%
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  select(id_municipio, id_municipio_unico) %>% 
  left_join(df %>% filter(turno == 2))

#Regressão ----

df.reg <- df.match %>% 
  filter(faixa_etaria != "Inválido", faixa_etaria != "100 anos ou mais") %>% 
  mutate(tx_abstencao = abstencoes / aptos) %>% 
  select(id_municipio_unico, passe_livre, faixa_etaria, tx_abstencao) %>% 
  pivot_wider(names_from = faixa_etaria, values_from = tx_abstencao) %>% 
  mutate(across(!c(id_municipio_unico, passe_livre), ~ . - `65 a 69 anos`))

df <- df %>% 
  left_join(municipios_tse %>% 
              select(codigo_tse, nome_municipio, codigo_ibge), 
            by = c("municipio" = "codigo_tse")) %>% 
  left_join(passe_livre %>% select(-ano),
            by = c("codigo_ibge" = "id_municipio", "turno"))

df <- df %>% 
  mutate(uf = substr(codigo_ibge, 1, 2))

write.csv(df, file = "df.csv")

df

lm(`60 a 64 anos` ~ passe_livre, data = df.reg) %>% 
  summary(.)

lm(`55 a 59 anos` ~ passe_livre, data = df.reg) %>% 
  summary(.)


