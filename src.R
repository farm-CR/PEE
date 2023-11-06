library(tidyverse)
library(readr)
library(MatchIt)
library(fixest)

#Dados ----

df_original <- bind_rows(list(
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2022.csv"),
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2018.csv"),
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2014.csv")
))

municipios_tse <- read_csv("dados/municipios_brasileiros_tse.csv") %>% 
  select(id_municipio_tse = codigo_tse, id_municipio = codigo_ibge)

passe_livre <- read_csv("dados/passe_livre.csv")

df <- df_original %>% 
  select(ano = ANO_ELEICAO, turno = NR_TURNO, id_municipio_tse = CD_MUNICIPIO, 
         faixa_etaria = DS_FAIXA_ETARIA, aptos = QT_APTOS, comparecimento = QT_COMPARECIMENTO) %>% 
  mutate(
    faixa_etaria = fct_collapse(
      faixa_etaria,
      "18 a 20 anos" = c("20 anos", "19 anos", "18 anos"),
      "16 e 17 anos" = c("17 anos", "16 anos")),
    faixa_etaria = fct_relevel(faixa_etaria, "100 anos ou mais", after = Inf)) %>% 
  group_by(ano, turno, id_municipio_tse, faixa_etaria) %>% 
  summarize(aptos = sum(aptos),
            comparecimento = sum(comparecimento)) %>% 
  left_join(municipios_tse) %>% 
  left_join(
    bind_rows(list(
      passe_livre,
      passe_livre %>% mutate(ano = 2018, passe_livre = 0),
      passe_livre %>% mutate(ano = 2014, passe_livre = 0)
    ))
  ) 

#Análise descritiva ----

df %>% 
  filter(faixa_etaria != "Inválido") %>% 
  group_by(ano, turno, faixa_etaria) %>% 
  summarize(aptos = sum(aptos), comparecimento = sum(comparecimento)) %>% 
  mutate(comparecimento = comparecimento/aptos,
         comparecimento = ifelse(turno == 1, -comparecimento, comparecimento)) %>% 
  ggplot(aes(x = faixa_etaria, y = comparecimento, fill = factor(turno))) +
  geom_col() +
  # geom_hline(yintercept = -0.8371519, linetype = "dashed") +
  # geom_hline(yintercept = 0.8470657, linetype = "dashed") +
  coord_flip() +
  facet_wrap(~ano) +
  labs(x = "", y = "Comparecimento", fill = "Turno") 

ggsave("output/piramide.png", dpi = 600, height = 4, width = 5)

write.csv(df, "output/data.csv")

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
  mutate(tx_comparecimento = comparecimento / aptos,
         faixa_etaria = str_replace_all(faixa_etaria, " ", "_"),
         faixa_etaria = str_c("i", faixa_etaria)) %>% 
  select(ano, id_municipio, turno, passe_livre, faixa_etaria, tx_comparecimento) %>% 
  pivot_wider(names_from = faixa_etaria, 
              values_from = tx_comparecimento,
              id_cols = c(ano, turno, id_municipio, passe_livre),
              values_fn = {sum}) %>% 
  mutate(across(!c(id_municipio, passe_livre, turno, ano), ~ . - i65_a_69_anos))

feols(i60_a_64_anos ~ passe_livre | ano + id_municipio, data = df.reg) %>% 
  summary(.)

feols(i55_a_59_anos ~ passe_livre | ano + id_municipio, data = df.reg) %>% 
  summary(.)


