library(tidyverse)
library(readxl)
library(sidrar)


###Dados do regic
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
  

####DAdos CAPAG


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


####Dados idsc

idsc_2024 <- 
  read_excel("Base_de_Dados_IDSC-BR_2024.xlsx", 
                                         sheet = "IDSC-BR 2024") %>%
  janitor::clean_names()


idsc_2024_trabalho<-
  idsc_2024 %>%
  select(1,4) %>%
  rename(id_municipio= cod_mun)


####Daddos proporção do PIB dos municípios

info_sidra(5938, wb = TRUE)

participacao_gestao_publica_pib<-  
  get_sidra(x = 5938,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 528,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            #classific = "C544",
            #category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

participacao_gestao_publica_pib_trabalho<-
  participacao_gestao_publica_pib %>%
  select(c(4:5)) %>%
  rename(
    proporcao_gestao_publica_pib = V,
    id_municipio = D1C
  ) %>%
  mutate(id_municipio = as.numeric(id_municipio)) %>%
  filter(proporcao_gestao_publica_pib >0 )



###Dados MUNIC

munic_2019 <- read_excel("Base_MUNIC_2019_20210817.xlsx", 
                                       sheet = "Recursos para gestão")




munic_2019_trabalho<-
  munic_2019 %>%
  select(CodMun, MREG01:MREG0143, MREG02, MREG03:MREG04, MREG05:MREG051, MREG061:MREG066) %>%
  pivot_longer(cols=MREG01:MREG066,
               names_to = "capacidade_gestao",
               values_to = "existe_capacidade") %>%
  mutate(existe_capacidade = case_when(
    existe_capacidade == "Sim" ~ 1,
    existe_capacidade == "Não" ~ 0,
    .default = NA
  )) %>%
  summarise(total_capacidades_gestao = sum(existe_capacidade, na.rm = TRUE),
            .by= CodMun ) %>%
  rename(id_municipio = CodMun)


###Indicador de vulnerabilidade: desastres ambientais reconhecidos

desastres_ambientais_reconhecidos <- readRDS("~/github/indicadores_municipais/desastres_ambientais_reconhecidos.rds")


desastres_ambientais_trabalho<-
  desastres_ambientais_reconhecidos %>%
  summarise( quantidade_desastres_2023 = n(),
            .by = id_municipio) %>%
  filter(!is.na(id_municipio))



#### consolidação

indicadores_municipios<-
  cidades_trabalho_regic %>%
  left_join(munic_2019_trabalho)  %>%
  left_join(notas_capag_trabalho) %>%
  left_join(idsc_2024_trabalho) %>%
  left_join(participacao_gestao_publica_pib_trabalho) %>%
  left_join(desastres_ambientais_trabalho) %>%
  select(c(1,7,8,2:6,9:15)) %>%
  mutate(id_municipio = as.character(id_municipio))




indicadores_municipios %>%
  writexl::write_xlsx("indicadores_municipios.xlsx")


#### Anáises exploratórias iniciais

boxplot(indicadores_municipios$proporcao_gestao_publica_pib)

boxplot(indicadores_municipios$idsc_br_2024)

boxplot(log(indicadores_municipios$intensidade_gestao_empresarial))

indicadores_municipios %>%
  ggplot(aes(x= proporcao_gestao_publica_pib, y= idsc_br_2024)) +
  geom_point()

cor.test(indicadores_municipios$proporcao_gestao_publica_pib, indicadores_municipios$idsc_br_2024)

indicadores_municipios %>%
  ggplot(aes(x= proporcao_gestao_publica_pib, y= intensidade_gestao_empresarial)) +
  geom_point()+
  scale_y_log10()

cor.test(indicadores_municipios$proporcao_gestao_publica_pib, log(indicadores_municipios$intensidade_gestao_empresarial))


indicadores_municipios %>%
  ggplot(aes(x= proporcao_gestao_publica_pib, y= centralidade_gestao_publica)) +
  geom_point()

cor.test(indicadores_municipios$proporcao_gestao_publica_pib, log(indicadores_municipios$intensidade_gestao_empresarial))
