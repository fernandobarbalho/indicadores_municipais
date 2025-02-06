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
