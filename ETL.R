library(tidyverse)
library(readxl)
library(sidrar)
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


####Daddos proporção do PIB dos municípios


pib_municipio<-  
  get_sidra(x = 5938,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 37,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            #classific = "C544",
            #category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

pib_municipio_trabalho<-
  pib_municipio %>%
  select(c(4:5)) %>%
  rename(
    pib_municipio = V,
    id_municipio = D1C
  ) %>%
  mutate(id_municipio = as.numeric(id_municipio)) %>%
  filter(pib_municipio >0 )


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



# Dados RCL

dados_rcl <- read_delim("dados_rcl.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = "."), trim_ws = TRUE)
dados_rcl <- janitor::clean_names(dados_rcl)

rcl_Jacareacanga <- read_csv("rcl_Jacareacanga.csv")
rcl_Jacareacanga <- janitor::clean_names(rcl_Jacareacanga)

dados_rcl<- bind_rows(dados_rcl, rcl_Jacareacanga)

dados_rcl_trabalho<-  
  dados_rcl %>%
  summarise( rcl= sum(value),
             .by = c(id_ente, an_exercicio)) %>%
  arrange(id_ente)

dados_rcl_trabalho<-
  dados_rcl_trabalho %>%
  summarise( rcl = mean(rcl),
             .by = id_ente) %>%
  mutate(id_ente = as.character(id_ente)) %>%
  rename(id_municipio = id_ente)






# Dados número de servidores municipios pela RAIS

rais_servidores_municipios <- read_csv("rais_servidores_municipios.csv", 
                                       col_types = cols(id_municipio = col_character()))


#dados população
info_sidra(4709, wb = TRUE)

populacao_municipios<-  
  get_sidra(x = 4709,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 93,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            #classific = "C544",
            #category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)


populacao_municipios<-
  populacao_municipios %>%
  select(c(5,4)) %>%
  rename(
    populacao = V,
    id_municipio = D1C  ) 



#Dados de idsc associados a indicadores de capacidades
#SDG3_32_UBS - Unidades Básicas de Saúde (mil habitantes)
#SDG4_18_D_SUP_EI - Professores com formação em nível superior - Educação Infantil - rede pública (%) #capacidade administrativa
#SDG4_19_D_SUP_EF - Professores com formação em nível superior - Ensino Fundamental - rede pública (%)
#SDG9_2_EMP_INT - Participação dos empregos formais em atividades intensivas em conhecimento e tecnologia (%)
#SDG17_3_P_RC_TRB - Total de receitas municipais arrecadadas (%)

#FPE(banco mundial) hiato fiscal: receita potencial do estado
#contexto: localização importa (dependência tributária, )


idsc_2024_indicadores_capacidade<-
  idsc_2024 %>%
  select(
    cod_mun,
    sdg3_27_dsp_sau,
    sdg3_32_ubs,
    sdg4_18_d_sup_ei,
    sdg4_19_d_sup_ef,
    sdg9_2_emp_int,
    sdg17_3_p_rc_trb,
    
  ) %>%
  rename(id_municipio = cod_mun) %>%
  mutate(id_municipio = as.character(id_municipio))


#DAdos do ranking do indicador da qualdiade da informação contábil e fiscal no SICONFI - Proxy da capacidade de produzir e tratar dados
# parnamirim- RN estruutra de TI avançada

municipios_bspn <- read_delim("Ranking_municipios_bspn/municipios_bspn.csv", 
                              delim = ";", escape_double = FALSE, col_types = cols(TOTAL = col_double()), 
                              locale = locale(), trim_ws = TRUE)

municipios_bspn <- janitor::clean_names(municipios_bspn)

municipios_bspn_trabalho<- 
  municipios_bspn %>%
  filter(va_exercicio == 2023 ) %>%
  select(id_ente, 
         total) %>%
  rename(id_municipio = id_ente) %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  rename(indice_qualidade_informacao_contabil = total)


#### consolidação

indicadores_municipios<-
  cidades_trabalho_regic %>%
  left_join(munic_2019_trabalho)  %>%
  left_join(notas_capag_trabalho) %>%
  left_join(idsc_2024_trabalho) %>%
  left_join(participacao_gestao_publica_pib_trabalho) %>%
  left_join(pib_municipio_trabalho) %>%
  left_join(desastres_ambientais_trabalho) %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  left_join(dados_rcl_trabalho) %>%
  left_join(rais_servidores_municipios) %>%
  left_join(populacao_municipios) %>%
  left_join(idsc_2024_indicadores_capacidade) %>%
  left_join(municipios_bspn_trabalho) %>%
  select(c(1,7,8,2:6,9:25))





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


