library(tidyverse)
library(readxl)

cidades_regic <- read_excel("REGIC2018_Cidades_v2.xlsx")


cidades_trabalho_regic<-
  cidades_regic %>%
  select(COD_CIDADE,VAR19, VAR23, VAR29, VAR84) %>%
  rename(id_municipio = COD_CIDADE,
         centralidade_gestao_territorio = VAR19,
         intensidade_gestao_empresarial = VAR23,
         centralidade_gestao_publica = VAR29,
         centralidade_atividades_financeiras = VAR84)%>%
  mutate(codigo_do_ap = NA,
         centralidade_gestao_publica = as.numeric(centralidade_gestao_publica))

municipios_ibge <- read_csv("municipios_ibge.csv")

arranjos_populacionais<-  read_excel("REGIC2018_Arranjos_Populacionais_v2.xlsx") 

arranjos_populacionais <-  janitor::clean_names(arranjos_populacionais)

arranjos_populacionais_trabalho<-
  arranjos_populacionais %>%
  filter(codmun != codigo_do_ap ) %>%
  mutate(arranjo_populacional = TRUE) %>%
  rename(id_municipio = codigo_do_ap) %>%
  select(codmun, id_municipio) %>%
  inner_join(
    cidades_trabalho_regic
  ) %>%
  mutate(codigo_do_ap = id_municipio,
         id_municipio = codmun) %>%
  select(-codmun)


cidades_trabalho_regic<-
cidades_trabalho_regic %>%
  bind_rows(arranjos_populacionais_trabalho) %>%
  inner_join(municipios_ibge)
  


notas_capag <- 
  read_excel("TT20240515CAPAG-Municipios.xlsx", 
                                         skip = 2) %>%
  janitor::clean_names()

trata_nd <- function(valor){
  ifelse(is.na(valor),0,as.numeric(valor))
}

notas_capag_trabalho<-
  notas_capag %>%
  rename(id_municipio = codigo_municipio_completo) %>%
  select(id_municipio, indicador_1, indicador_2, indicador_3) %>%
  mutate(across(indicador_1:indicador_3,trata_nd)) %>%
  rename(
    indicador_endividamento = indicador_1,
    indicador_poupanca_corrente = indicador_2,
    indicador_liquidez_relativa = indicador_3
  )


idsc_2024 <- 
  read_excel("Base_de_Dados_IDSC-BR_2024.xlsx", 
                                         sheet = "IDSC-BR 2024") %>%
  janitor::clean_names()


idsc_2024_trabalho<-
  idsc_2024 %>%
  select(1,4) %>%
  rename(id_municipio= cod_mun)



indicadores_municipios<-
  cidades_trabalho_regic %>%
  left_join(notas_capag_trabalho) %>%
  left_join(idsc_2024_trabalho) %>%
  select(c(1,7,8,2:6,9:12))


indicadores_municipios %>%
  writexl::write_xlsx("indicadores_municipios.xlsx")
