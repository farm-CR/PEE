library(tidyverse)
library(MatchIt)
library(cobalt)

load("dados/comparecimento.RData")

df <- df %>% 
  filter(faixa_etaria != "100 anos ou mais" & faixa_etaria != "Inválido",
         turno == 2) %>% 
  droplevels()

passe_livre <- read_csv("dados/passe_livre.csv")

df.ps <- read_csv("dados/psm.csv") %>% 
  left_join(passe_livre) %>% 
  filter(turno == 2)

modelo.ps <- passe_livre ~ razao_dependencia + taxa_envelhecimento + expectativa_anos_estudo + 
  taxa_analfabetismo_18_mais + indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais  + taxa_agua_encanada + log(populacao) + populacao_urbana

fit.ps = glm(modelo.ps, family = binomial(link = "logit"), data = df.ps)
summary(fit.ps)

modelo.ps2 <- passe_livre ~ taxa_envelhecimento + taxa_analfabetismo_18_mais + 
  indice_gini + prop_pobreza_extrema + log(renda_pc) + idhm +
  taxa_desocupacao_18_mais + log(populacao)

fit.ps2 = glm(modelo.ps2, family = binomial(link = "logit"), data = df.ps)
summary(fit.ps2)

# Suporte comum
df.ps %>%
  mutate(ps = predict(fit.ps, type = "response"),
         passe_livre = ifelse(passe_livre, "Tratamento", "Controle")) %>% 
  ggplot(aes(x = ps)) +
  geom_histogram(aes(y = after_stat(density)), color = "white") +
  facet_wrap(~passe_livre) +
  labs(x = "", y = "") +
  theme_bw()

# Nearest neighbour com reposicao
match <- matchit(modelo.ps, method = "nearest", data = df.ps, 
                    replace = T, ratio = 1, discard = "both")
summary(match)
plot(summary(match))

bal.tab(match)
love.plot(match, thresholds = c(m = .1, v = 1),
          stats = stats)

df.match <- match %>% 
  get_matches(data = df.ps) %>%
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  left_join(df %>% filter(turno == 2))

# K nearest neighbour
match <- matchit(modelo.ps, method = "nearest", data = df.ps, 
                 replace = T, ratio = 3, caliper = .1, discard = "both")

summary(match)
plot(summary(match))

df.match <- match %>% 
  get_matches(data = df.ps) %>%
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  left_join(df %>% filter(turno == 2))

# Radius matching
match <- matchit(modelo.ps, method = "nearest", data = df.ps, 
                 replace = T, ratio = 8000, caliper = .5, discard = "both")

summary(match)
plot(summary(match))

df.match <- match %>% 
  get_matches(data = df.ps) %>%
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  left_join(df %>% filter(turno == 2))

# Optimal full matching
match <- matchit(modelo.ps, method = "full", discard = "both", data = df.ps)

summary(match)
plot(summary(match))

df.match <- match.data(match) %>% 
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  left_join(df %>% filter(turno == 2))

# Coarsened exact matching
cutpoints <- list(taxa_envelhecimento = "q5",
                  taxa_analfabetismo_18_mais = "q4",
                  taxa_desocupacao_18_mais = "q4",
                  idhm = "q5",
                  indice_gini = "q5")
match <- matchit(modelo.ps2, method = "cem", data = df.ps, cutpoints = cutpoints)
summary(match)
plot(summary(match))

cutpoints <- list(taxa_envelhecimento = "q4",
                  taxa_analfabetismo_18_mais = "q3",
                  taxa_desocupacao_18_mais = "q4",
                  expectativa_anos_estudo = 6,
                  prop_pobreza_extrema = 2,
                  taxa_agua_encanada = "q3",
                  idhm = "q4",
                  indice_gini = "q4")
match <- matchit(modelo.ps, method = "cem", data = df.ps, cutpoints = cutpoints)
summary(match)
plot(summary(match))

df.match <- match.data(nn_match) %>% 
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  left_join(df %>% filter(turno == 2))

# Cardinality matching
match <- matchit(modelo.ps, method = "cardinality", data = df.ps, tols = .2)

summary(match)
plot(summary(match))

df.match <- match.data(match) %>% 
  mutate(id_municipio_unico = paste(id_municipio, subclass, sep = "-")) %>% 
  left_join(df %>% filter(turno == 2))

# Balanceamento
df.match %>% 
  group_by(passe_livre) %>% 
  summarise(across(razao_dependencia:idhm, mean)) %>% 
  View()






