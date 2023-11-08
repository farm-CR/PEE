library(tidyverse)
library(sf)

#Mapa dos grupos de munic�pio ----
df <- read.csv("dados/passe_livre.csv") %>% 
  mutate(id_municipio = as.character(id_municipio)) %>% 
  left_join(df.rm.idd %>% 
              distinct(id_municipio) %>% 
              mutate(area_metrop = 1,
                     id_municipio = as.character(id_municipio))) %>% 
  mutate(area_metrop = ifelse(is.na(area_metrop), 0, 1),
         code_muni = as.double(id_municipio))

data <- left_join(municipios, df) %>% 
  mutate(fill = ifelse((passe_livre == 1 & area_metrop == 1), "Passe Livre", 
                       ifelse(area_metrop == 1, "�rea Metropolitana", 
                              ifelse((passe_livre == 1), "Passe Livre Fora da An�lise", "Fora da An�lise"))))

ggplot() +
  geom_sf(data = data %>% filter(turno == 2), 
          aes(fill = factor(fill)), color = NA) +
  scale_fill_manual(values = c("�rea Metropolitana" = "#213A5C",
                      "Passe Livre" = "#006600",
                      "Passe Livre Fora da An�lise" = "#90A4B0",
                      "Fora da An�lise" = "#758D93")) +
  labs(fill = "Grupo") +
  theme_void()

ggsave("output/mapa.png", dpi = 1200, width = 10, height = 10)

#Pir�mide Et�ria ----

df <- read.csv("output/data.csv")

df %>% 
  filter(faixa_etaria != "Inv�lido", ano == 2022) %>% 
  group_by(ano, turno, faixa_etaria) %>% 
  summarize(aptos = sum(aptos), comparecimento = sum(comparecimento)) %>% 
  mutate(comparecimento = comparecimento/aptos,
         comparecimento = ifelse(turno == 1, -comparecimento, comparecimento)) %>% 
  ggplot(aes(x = faixa_etaria, y = comparecimento, fill = factor(turno))) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ano) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0, 0.25, .5, .75, -0.25, -.5, -.75)) +
  labs(x = "", y = "Comparecimento", fill = "Turno") 

ggsave("output/piramide.png", dpi = 600, height = 4, width = 5)