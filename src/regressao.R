library(tidyverse)
library(readr)
library(MatchIt)
library(fixest)
library(betareg)

#Dados ----

df_original <- bind_rows(list(
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2022.csv"),
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2018.csv"),
  read_csv("dados/comparecimento/perfil_comparecimento_abstencao_2014.csv")
))

pnad_idade <- bind_rows(list(
  read_csv("dados/pnad/idade2022_3.csv"),
  read_csv("dados/pnad/idade2018_3.csv"),
  read_csv("dados/pnad/idade2014_3.csv")
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
  ) %>% 
  left_join(read.csv("output/data.csv"))

write.csv(df, "output/data.csv")

df2 <- read.csv("output/__data.csv")

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

# IPW
# N <- nrow(df.psm)
# 
# ps_logit <- glm(modelo.psm, data = df.psm, family = binomial(link = "logit"))
# 
# df.ipw <- df.psm %>% 
#   mutate(ps = ps_logit$fitted.values,
#          d1 = passe_livre / ps,
#          d0 = (1 - passe_livre) / (1 - ps))
# 
# s1 <- sum(df.ipw$d1)
# s0 <- sum(df.ipw$d0)
# 
# df.ipw <- df.psm %>% 
#   mutate(ps = ps_logit$fitted.values) %>% 
#   select(id_municipio, turno, passe_livre, ps) %>% 
#   right_join(df %>% filter(turno == 2)) %>% 
#   mutate(tx_comparecimento = comparecimento / aptos,
#          y1 = (passe_livre * tx_comparecimento / ps) / (s1 / N),
#          y0 = ((1 - passe_livre) * tx_comparecimento / (1 - ps)) / (s0 / N),
#          tx_comparecimento_ipw = y1 + y0,
#          norm = y1 - y0)
# 
# df.ipw %>% filter(passe_livre == 1) %>% distinct(ano)

#Regressao ----

df.reg <- df.match %>% 
  filter(faixa_etaria != "Inv?lido", faixa_etaria != "100 anos ou mais") %>% 
  mutate(tx_comparecimento = comparecimento / aptos,
         faixa_etaria = str_replace_all(faixa_etaria, " ", "_"),
         faixa_etaria = str_c("i", faixa_etaria)) %>% 
  select(ano, id_municipio, turno, passe_livre, faixa_etaria, tx_comparecimento) %>% 
  pivot_wider(names_from = faixa_etaria, 
              values_from = tx_comparecimento,
              id_cols = c(ano, turno, id_municipio, passe_livre),
              values_fn = {mean}) %>% 
  mutate(across(!c(id_municipio, passe_livre, turno, ano), ~ . - i65_a_69_anos))

feols(i60_a_64_anos ~ passe_livre | id_municipio + ano, data = df.reg) %>% 
  summary(.)

feols(i55_a_59_anos ~ passe_livre | ano + id_municipio, data = df.reg) %>% 
  summary(.)

df.reg <- df.match %>% 
  filter(faixa_etaria != "Inválido", faixa_etaria != "100 anos ou mais") %>% 
  mutate(tx_comparecimento = comparecimento / aptos,
         faixa_etaria = as_factor(str_replace_all(faixa_etaria, " ", "_")),
         faixa_etaria = relevel(faixa_etaria, ref = 12)) %>% 
  select(ano, id_municipio, turno, passe_livre, faixa_etaria, tx_comparecimento,
         competitividade, populacao, pib_pc, ideb, beneficiados, pib_governo, 
         eleitores_secao)

feols(log(tx_comparecimento) ~ passe_livre * faixa_etaria + 
        log(competitividade) + log(populacao) + log(pib_pc) + ideb +  
        beneficiados + pib_governo + eleitores_secao | id_municipio + ano, 
      data = df.reg) %>% 
  summary(.)

betareg(log(tx_comparecimento) ~ passe_livre * faixa_etaria + 
          log(competitividade) + log(populacao) + log(pib_pc) + ideb +  
          beneficiados + pib_governo + eleitores_secao + as_factor(ano), 
        data = df.reg %>% 
          filter(tx_comparecimento > 0 & tx_comparecimento < 1)) %>% 
  summary(.)


# df.reg <- df.ipw %>% 
#   filter(faixa_etaria != "Inválido", faixa_etaria != "100 anos ou mais") %>% 
#   mutate(faixa_etaria = as_factor(str_replace_all(faixa_etaria, " ", "_")),
#          faixa_etaria = relevel(faixa_etaria, ref = 12)) %>% 
#   select(ano, id_municipio, turno, passe_livre, faixa_etaria, tx_comparecimento_ipw, norm)
# 
# df.ipw %>% 
#   filter(!is.na(norm)) %>% 
#   pull(norm) %>% 
#   mean()
# 
# feols(norm ~ passe_livre * faixa_etaria | id_municipio + ano, 
#       data = df.reg) %>% 
#   summary(.)

