library(tidyverse)
library(readxl)

load("dados/controles_pnad.RData")
df.pnad <- df.pnad %>% 
  mutate(across(uf, as.character))

rms <- read_excel("dados/aglom_urbanas.xlsx") %>% 
  select(uf = COD_UF, id_municipio = COD_MUN, id_rm = COD_CATMETROPOL,
         nome_rm = NOME_CATMETROPOL, nome_municipio = NOME_MUN)

passe_livre = read_csv("dados/passe_livre.csv") %>% 
  mutate(across(id_municipio, as.character))

# Regioes metropolitanas presentes na PNADc
rms_pnad <- c("00201", "00601", "00801",
              "01101", "01301", "01401", "01701", "01801", "03001", "03201", 
              "04101", "04201",
              "04501", "04701", "04801", "04901",
              "05501", "06301", "07401", "07601", "07701")

df.rm.idd <- rms %>% 
  filter(id_rm %in% rms_pnad) %>% 
  left_join(df.pnad) %>% 
  left_join(passe_livre) %>% 
  select(uf, id_municipio, ano, turno, faixa_etaria:motivo_vontade) %>% 
  mutate(faixa_etaria = as_factor(str_replace_all(faixa_etaria, " ", "_")),
         faixa_etaria = relevel(faixa_etaria, ref = "60_a_64_anos"))

save(df.rm.idd, file = "dados/df_rm_idd.RData")
