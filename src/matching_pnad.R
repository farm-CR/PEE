library(tidyverse)
library(MatchIt)
library(cobalt)
library(fixest)

load("dados/df_rm_idd.RData")
load("dados/comparecimento.RData")

pnad_uf <- bind_rows(list(
  read_csv("dados/pnad/uf2022_3.csv"),
  read_csv("dados/pnad/uf2018_3.csv"),
  read_csv("dados/pnad/uf2014_3.csv")
))

df <- df %>% 
  filter(faixa_etaria != "100 anos ou mais" & faixa_etaria != "Inválido",
         turno == 2) %>% 
  droplevels()

passe_livre <- read_csv("dados/passe_livre.csv")

df.ps <- read_csv("dados/psm.csv") %>% 
  left_join(passe_livre) %>% 
  filter(turno == 2) %>% 
  select(-`...1`) %>% 
  mutate(across(id_municipio, as.character))

# Filtrar apenas municipios contemplados pela PNADc
df.ps <- df.ps %>% 
  semi_join(df.rm.idd %>% 
              distinct(id_municipio))

modelo.ps <- passe_livre ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + populacao_urbana

# Nearest neighbour matching
match <- matchit(modelo.ps, method = "nearest", data = df.ps, 
                 replace = T, ratio = 1, discard = "control")

# Optimal matching
match <- matchit(modelo.ps, method = "optimal", data = df.ps, discard = "both")

# Balanceamento
bal.tab(match, stats = c("mean.diffs", "variance.ratios", "ks.statistics"))
love.plot(match, thresholds = c(m = .1, v = 1),
          stats = c("mean.diffs", "variance.ratios", "ks.statistics"))

df.match <- match.data(match) %>% 
  left_join(df) %>% 
  mutate(faixa_etaria = as_factor(str_replace_all(faixa_etaria, " ", "_")),
         faixa_etaria = relevel(faixa_etaria, ref = 12))

df.reg <- df.match %>%
  left_join(df.rm.idd) %>% 
  select(id_municipio, ano, turno, tx_comparecimento, passe_livre, faixa_etaria,
         idade:motivo_vontade)

feols(tx_comparecimento ~ passe_livre * faixa_etaria | id_municipio + ano, 
      data = df.reg) %>% 
  summary(.)
