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
  mutate(codigo_do_ap = NA)

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
  