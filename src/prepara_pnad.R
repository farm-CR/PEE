library(tidyverse)
library(PNADcIBGE)
library(survey)

save_pnad <- function(ano){
  df <- PNADcIBGE::read_pnadc(microdata = sprintf("dados/ignore/pnad/%s_3.txt", ano),
                                       input_txt = "dados/ignore/pnad/input.txt") %>% 
    #Sele��o das perguntas
    select(uf = UF,
           pesos = V1028,
           idade = V2009,
           analfabeto = V3001,
           trabalha = V4001,
           carteira_assinada = V4029,
           responsavel = VD2002,
           ensino_medio = VD3004,
           ocupacao = VD4008,
           salario = VD4019,
           motivo_indisp_pre = VD4023,
           motivo_indisp_pos = VD4030,
           horas_trabalho = VD4031) %>% 
    mutate(across(everything(), as.integer),
           ano = ano,
           #Pergunta do motivo muda em 2015
           motivo_indisp = ifelse(is.na(motivo_indisp_pre), motivo_indisp_pos, motivo_indisp_pre)) %>% 
    #Transforma��o das vari�veis em dummies
    mutate(analfabeto = ifelse(analfabeto == 1, 0, ifelse(analfabeto == 2, 1, NA)),
           trabalha = ifelse(trabalha == 1, 1, ifelse(trabalha == 2, 0, NA)),
           carteira_assinada = ifelse(carteira_assinada == 1, 1, ifelse(carteira_assinada == 2, 0, NA)),
           responsavel = ifelse(responsavel == 1, 1, ifelse(is.na(responsavel), NA, 0)),
           ensino_medio = ifelse(ensino_medio < 5, 0, ifelse(is.na(ensino_medio), NA, 1)),
           setor_privado = ifelse(ocupacao == 1, 1, ifelse(is.na(ocupacao), NA, 0)),
           setor_domestico = ifelse(ocupacao == 2, 1, ifelse(is.na(ocupacao), NA, 0)),
           setor_publico = ifelse(ocupacao == 3, 1, ifelse(is.na(ocupacao), NA, 0)),
           setor_empregador = ifelse(ocupacao == 4, 1, ifelse(is.na(ocupacao), NA, 0)),
           setor_conta_propria = ifelse(ocupacao == 5, 1, ifelse(is.na(ocupacao), NA, 0)),
           setor_auxiliar_familiar = ifelse(ocupacao == 6, 1, ifelse(is.na(ocupacao), NA, 0)),
           motivo_dependente = ifelse(motivo_indisp == 1, 1, ifelse(is.na(motivo_indisp), NA, 0)),
           motivo_estudo = ifelse(motivo_indisp == 2, 1, ifelse(is.na(motivo_indisp), NA, 0)),
           motivo_pcd = ifelse(motivo_indisp == 3, 1, ifelse(is.na(motivo_indisp), NA, 0)),
           motivo_idade = ifelse(motivo_indisp == 4, 1, ifelse(is.na(motivo_indisp), NA, 0)),
           motivo_vontade = ifelse(motivo_indisp == 5, 1, ifelse(is.na(motivo_indisp), NA, 0))) %>% 
    #Separa��o das faixas et�rias
    mutate(faixa_etaria = cut(idade, 
                              breaks=c(-Inf, 16, 18, 21, seq(25, 100, by = 5), Inf), 
                              labels=c('menor',
                                       '16 e 17 anos',
                                       '18 a 20 anos',
                                       '21 a 24 anos',
                                       '25 a 29 anos',
                                       '30 a 34 anos',
                                       '35 a 39 anos',
                                       '40 a 44 anos',
                                       '45 a 49 anos',
                                       '50 a 54 anos',
                                       '55 a 59 anos',
                                       '60 a 64 anos',
                                       '65 a 69 anos',
                                       '70 a 74 anos',
                                       '75 a 79 anos',
                                       '80 a 84 anos',
                                       '85 a 89 anos',
                                       '90 a 94 anos',
                                       '95 a 99 anos',
                                       '100 ou mais')))
  #Dados agrupados por munic�pio (PSM)
  df %>% 
    group_by(uf) %>% 
    summarize(across(!c(pesos, faixa_etaria), ~ weighted.mean(., pesos, na.rm = TRUE))) %>% 
    write.csv(sprintf("dados/pnad/uf%s_3.csv", ano))
  
  #Dados agrupados por faixa et�ria (Regress�o)
  df %>% 
    group_by(faixa_etaria, uf) %>% 
    summarize(across(!c(pesos), ~ weighted.mean(., pesos, na.rm = TRUE))) %>% 
    write.csv(sprintf("dados/pnad/idade%s_3.csv", ano))
}

#Salvar dados para os tr�s anos
save_pnad(2014)
save_pnad(2018)
save_pnad(2022)
