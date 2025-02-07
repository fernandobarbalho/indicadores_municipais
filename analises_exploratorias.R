library(DataExplorer) #a função plot_correlation abaixo foi implementada na biblioteca DataExplorer


##### Anaálise sobre dados do idsc

livro_codigos<-
  read_excel("Base_de_Dados_IDSC-BR_2024.xlsx", 
             sheet = "livro_codigos")

livro_codigos <- janitor::clean_names(livro_codigos)

livro_codigos<- 
  livro_codigos %>%
  rename(codigo = arquivo) %>%
  mutate(codigo = str_to_lower(codigo))

idsc_2024_indicadores_selecionados<-
  idsc_2024 %>%
  select(
    cod_mun,
    sigla_uf,
    normalizado_0_100_sdg3_32_ubs,
    normalizado_0_100_sdg3_15_t_m_inf,
    normalizado_0_100_sdg3_17_t_m_5a,
    normalizado_0_100_sdg4_15_ideb_af,
    normalizado_0_100_sdg4_16_ideb_ai,
    normalizado_0_100_sdg4_17_jv_em,
    normalizado_0_100_sdg4_18_d_sup_ei,
    normalizado_0_100_sdg4_19_d_sup_ef,
    normalizado_0_100_sdg4_5_t_an_15a

  ) %>%
  rename(id_municipio = cod_mun) %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  pivot_longer(cols = normalizado_0_100_sdg3_32_ubs:normalizado_0_100_sdg4_5_t_an_15a, names_to = "codigo", values_to = "valor"  ) %>%
  mutate(codigo = str_remove(codigo, "normalizado_0_100_"),
         codigo = str_to_lower(codigo))


dados_grafico<-
idsc_2024_indicadores_selecionados %>%
  inner_join(
    livro_codigos %>%
      select(codigo, indicador )
  )

dados_grafico %>%
  ggplot() +
  geom_boxplot(aes(x=sigla_uf, y=valor, fill = sigla_uf == "CE"), show.legend = FALSE) +
  facet_wrap(indicador~.) 




dados_financeiros<-
  indicadores_municipios %>%
  mutate(numero_servidores_per_capita = numero_servidores/populacao,
         rcl_per_capita = rcl/populacao,
         pib_per_capita = pib_municipio/populacao) %>%
  select(indicador_endividamento, indicador_poupanca_corrente, indicador_liquidez_relativa, rcl_per_capita,idsc_br_2024, proporcao_gestao_publica_pib,sdg17_3_p_rc_trb,pib_per_capita ) 


dados_financeiros <- na.omit(dados_financeiros)

boxplot(dados_financeiros$rcl_per_capita)

boxplot(log(indicadores_municipios$rcl))

plot_correlation(dados_financeiros)



#### Regressão para cálculo do rcl_per_capita

dados_modelo<-
  indicadores_municipios %>%
  mutate(rcl_per_capita = rcl/populacao,
         pib_per_capita = pib_municipio/populacao) %>%
  select(rcl_per_capita, pib_per_capita, rcl, pib_municipio ) 



modelo_rcl_per_capita<- lm(formula = rcl_per_capita  ~ pib_per_capita, data = dados_modelo)

summary(modelo_rcl_per_capita)

modelo_aplicado_rcl_percapita<- 
  indicadores_municipios %>%
  mutate(rcl_per_capita = rcl/populacao,
         pib_per_capita = pib_municipio/populacao) %>%
  select(id_municipio, nome, sigla_uf, rcl_per_capita, pib_per_capita) %>%
  mutate(rcl_previsto = modelo_rcl_per_capita[["coefficients"]][["(Intercept)"]] + modelo_rcl_per_capita[["coefficients"]][["pib_per_capita"]]* pib_per_capita)




modelo_rcl<- lm(formula = log(rcl)  ~ log(pib_municipio), data = dados_modelo)

summary(modelo_rcl)

modelo_aplicado_rcl<- 
  indicadores_municipios %>%
  mutate(rcl_per_capita = rcl/populacao,
         pib_per_capita = pib_municipio/populacao,
         log_pib = log(pib_municipio),
         log_rcl = log(rcl)) %>%
  select(id_municipio, nome, sigla_uf, rcl, pib_municipio, rcl_per_capita, pib_per_capita, log_pib, log_rcl) %>%
  mutate(log_rcl_previsto = modelo_rcl[["coefficients"]][["(Intercept)"]] + modelo_rcl[["coefficients"]][["log(pib_municipio)"]]*log_pib) %>%
  mutate(rcl_previsto = exp(log_rcl_previsto))


