library(tidyverse)
library(sf)
library(geobr)

passe_livre <- read.csv("dados/passe_livre.csv")

#Mapa dos grupos de município ----
df <- passe_livre %>% 
  mutate(id_municipio = as.character(id_municipio)) %>% 
  left_join(df.rm.idd %>% 
              distinct(id_municipio) %>% 
              mutate(area_metrop = 1,
                     id_municipio = as.character(id_municipio))) %>% 
  mutate(area_metrop = ifelse(is.na(area_metrop), 0, 1),
         code_muni = as.double(id_municipio))

data <- left_join(municipios, df) %>% 
  mutate(fill = ifelse((passe_livre == 1 & area_metrop == 1), "Passe Livre", 
                       ifelse(area_metrop == 1, "Área Metropolitana", 
                              ifelse((passe_livre == 1), "Passe Livre Fora da Análise", "Fora da Análise"))))

ggplot() +
  geom_sf(data = data %>% filter(turno == 2), 
          aes(fill = factor(fill)), color = NA) +
  scale_fill_manual(values = c("Área Metropolitana" = "#213A5C",
                      "Passe Livre" = "red",
                      "Passe Livre Fora da Análise" = "#758D93",
                      "Fora da Análise" = "#90A4B0")) +
  labs(fill = "Grupo") +
  theme_void()

ggsave("output/mapa.png", dpi = 1200, width = 10, height = 10)

ggplot() +
  geom_sf(data = data %>% filter(turno == 2, abbrev_state == "RS"), 
          aes(fill = factor(fill)), color = "#8D989F") +
  scale_fill_manual(values = c("Área Metropolitana" = "#213A5C",
                               "Passe Livre" = "#006600",
                               "Passe Livre Fora da Análise" = "#90A4B0",
                               "Fora da Análise" = "#758D93")) +
  labs(fill = "Grupo") +
  theme_void()

ggsave("output/mapa_zoom.png", dpi = 600, width = 10, height = 10)

#Pirâmide Etária ----

df <- read.csv("output/data.csv")

df %>% 
  filter(faixa_etaria != "Inválido", ano == 2022) %>% 
  group_by(ano, turno, faixa_etaria) %>% 
  summarize(aptos = sum(aptos), comparecimento = sum(comparecimento)) %>% 
  mutate(comparecimento = comparecimento/aptos,
         comparecimento = ifelse(turno == 1, -comparecimento, comparecimento),
         faixa_etaria = fct_relevel(faixa_etaria, "100 anos ou mais", after = Inf)) %>% 
  ggplot(aes(x = faixa_etaria, y = comparecimento, fill = factor(turno))) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ano) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0, 0.25, .5, .75, -0.25, -.5, -.75)) +
  labs(x = "", y = "Comparecimento", fill = "Turno") 

ggsave("output/piramide.png", dpi = 600, height = 4, width = 5)

piramide_bonus <- function(bonus){
  df %>% 
    filter(faixa_etaria != "Inválido", ano == 2022, turno == 2) %>% 
    group_by(ano, faixa_etaria) %>% 
    summarize(aptos = sum(aptos), comparecimento = sum(comparecimento)) %>% 
    mutate(comparecimento = comparecimento/aptos,
           faixa_etaria = fct_relevel(faixa_etaria, "100 anos ou mais", after = Inf),
           cor = ifelse(as.integer(faixa_etaria) > 11, "Transporte Gratuito", "Transporte Pago"),
           bonus = ifelse(cor == "Transporte Pago", bonus/15, 0)) %>%
    pivot_longer(cols = c(bonus, comparecimento)) %>%
    ggplot(aes(x = faixa_etaria, y = value, fill = name)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ano) +
    scale_fill_manual(values = c("comparecimento" = "#46417D", "bonus" = "#ACA9D3")) +
    scale_y_continuous(labels = scales::percent_format(),
                       breaks = c(0, 0.25, .5, .75, 1), limits = c(0, 1)) +
    scale_color_manual(values = c("Transporte Gratuito" = "grey",
                                  "Transporte Pago" = "black")) +
    labs(x = "", y = "Comparecimento")
  
  ggsave(sprintf("output/piramide_lateral%s.png", bonus), dpi = 600, height = 4, width = 5)
}

piramide_bonus(0)
piramide_bonus(1)

#Passe livre ----

df.temp <- df %>% 
  as_tibble() %>% 
  left_join(passe_livre) %>%
  filter(ano == 2022, turno == 2) %>% 
  group_by(passe_livre) %>% 
  summarize(comparecimento = sum(comparecimento) / sum(aptos)) %>% 
  drop_na() %>% 
  mutate(passe_livre = ifelse(passe_livre == 1, "Adotou", "Não Adotou")) %>% 
  mutate(bonus = ifelse(passe_livre == "Adotou",max(comparecimento) - min(comparecimento), 0),
         comparecimento = min(comparecimento)) %>% 
  pivot_longer(cols = c(comparecimento, bonus))

line1 <- df.temp %>% 
  filter(passe_livre == "Não Adotou") %>% 
  pull(value) %>% 
  max()

line2 <- df.temp %>% 
  filter(passe_livre == "Adotou") %>% 
  summarize(valor = sum(value)) %>% 
  pull(valor)

df.temp %>% 
  ggplot(aes(x = reorder(passe_livre, value), y = value, fill = name)) +
  geom_col() +
  geom_hline(yintercept = line1, linetype = "dashed") +
  geom_hline(yintercept = line2, linetype = "dashed") +
  coord_cartesian(ylim = c(0.7,0.85)) +
  labs(y = "Comparecimento", x = "Passe Livre") +
  scale_fill_manual(values = c("comparecimento" = "#46417D", "bonus" = "#ACA9D3")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "none")

ggsave("output/passe_livre.png", dpi = 600, height = 4.5, width = 3)

# Mais mapas ----

df %>% 
  filter(ano == 2022, turno == 2) %>% 
  group_by(id_municipio) %>% 
  summarize(comparecimento_tx = sum(comparecimento) / sum(aptos)) %>% 
  mutate(code_muni = as.double(id_municipio)) %>% 
  left_join(municipios) %>% 
  ggplot() +
  geom_sf(aes(fill = comparecimento_tx, geometry = geom), color = NA) +
  theme_void() +
  scale_fill_viridis_c()

ggsave("output/mapa_comparecimento.png", dpi = 1200, width = 10, height = 10)

df %>% 
  mutate(faixa_etaria = 
           fct_collapse(faixa_etaria,
                        idosos = c("100 anos ou mais", "95 a 99 anos", "90 a 94 anos", 
                                          "85 a 89 anos", "80 a 84 anos", "75 a 79 anos", 
                                          "70 a 74 anos", "65 a 69 anos", "60 a 64 anos"),
                        other_level = "nao_idosos")) %>% 
  filter(ano == 2022, turno == 2) %>% 
  group_by(id_municipio, faixa_etaria) %>% 
  summarize(aptos = sum(aptos)) %>% 
  pivot_wider(names_from = faixa_etaria, values_from = aptos) %>%
  mutate(dependencia = idosos / nao_idosos) %>% 
  mutate(code_muni = as.double(id_municipio)) %>% 
  left_join(municipios) %>% 
  ggplot() +
  geom_sf(aes(fill = dependencia, geometry = geom), color = NA) +
  theme_void() +
  scale_fill_viridis_c()

ggsave("output/mapa_dependencia.png", dpi = 1200, width = 10, height = 10)

