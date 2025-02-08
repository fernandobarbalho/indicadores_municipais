library(DataExplorer) #a função plot_correlation abaixo foi implementada na biblioteca DataExplorer
library(tidyverse)
library(colorspace)
library(ggrepel)

##### Anaálise sobre dados do idsc

livro_codigos<-
  read_excel("Base_de_Dados_IDSC-BR_2024.xlsx", 
             sheet = "livro_codigos")

livro_codigos <- janitor::clean_names(livro_codigos)

livro_codigos<- 
  livro_codigos %>%
  rename(codigo = arquivo) %>%
  mutate(codigo = str_to_lower(codigo))

###Gráricos idsc com dados normalizados

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



#####Gráficos idsc com dados brutos

idsc_2024_indicadores_selecionados_brutos<-
  idsc_2024 %>%
  select(
    cod_mun,
    municipio,
    sigla_uf,
    sdg3_32_ubs,
    sdg3_15_t_m_inf,
    sdg3_17_t_m_5a,
    sdg4_15_ideb_af,
    sdg4_16_ideb_ai,
    sdg4_17_jv_em,
    sdg4_18_d_sup_ei,
    sdg4_19_d_sup_ef,
    sdg4_5_t_an_15a
    
  ) %>%
  rename(id_municipio = cod_mun) %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  pivot_longer(cols = sdg3_32_ubs:sdg4_5_t_an_15a, names_to = "codigo", values_to = "valor"  ) %>%
  mutate(codigo = str_remove(codigo, "normalizado_0_100_"),
         codigo = str_to_lower(codigo))


dados_grafico<-
  idsc_2024_indicadores_selecionados_brutos %>%
  inner_join(
    livro_codigos %>%
      select(codigo, indicador )
  )


medianas<-
  dados_grafico %>%
  summarise(mediana = median(valor, na.rm = TRUE),
            .by = indicador)

dados_max_ideb<-
  dados_grafico %>%
  filter(codigo %in% c("sdg4_15_ideb_af", "sdg4_16_ideb_ai")) %>%
  summarise(valor = max(valor, na.rm = TRUE),
            .by = codigo)
dados_sel<-
  dados_grafico %>%
  inner_join(dados_max_ideb)


grafico<-
dados_grafico %>%
  ggplot() +
  geom_boxplot(aes(x=sigla_uf, y=valor, fill = sigla_uf == "CE"), 
               color = "lightgray",
               outlier.shape = 21,
               outlier.color = "black",
               outlier.size = 1.6,
               fatten = 1,
               show.legend = FALSE)+ 
  geom_hline(data = medianas, aes(yintercept = mediana), color= "yellow",  size =0.5) +
  # Add curved arrow
  geom_curve(
    data = data.frame(
      sigla_uf = "DF",
      valor = 10,
      indicador = "Analfabetismo na população com 15 anos ou mais (%)",
      xend = "MA",
      yend = 38
    ),
    aes(x = "DF", y = 10, xend = "MA", yend = 36),
    curvature = -0.3,
    arrow = arrow(type = "closed", length = unit(0.1, "inches")),
    color = "yellow",
    size = 0.5
  ) +
  geom_text(data = data.frame(indicador = "Analfabetismo na população com 15 anos ou mais (%)"),
            aes(x="PB", y=37, label = "Mediana da variável"), 
            color="yellow") +
  geom_text_repel(data= dados_sel, 
            aes(x=sigla_uf, y=valor,label= paste0(municipio,": ",valor )),
            color = "#c87a8a",
            hjust = -0.1) +
  facet_wrap(str_wrap(indicador,60)~.,scale = "free_y", ncol= 3 ) +
  scale_fill_discrete_qualitative(palette = "Dark 2", rev= TRUE)+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill =  "black"),
    axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.1),
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,,
      color = "darkblue",
      face = "bold")
  )+
  labs(
    title= "Posição do Ceará em indicadores selecionados",
    x="",
    y="",
    caption = "Fonte: Índice de desenvolvimento sustentável das cidades (IDSC-BR). Elaboração: Fernando Barbalho"
  )

##ggsave(filename = "idsc_ceara.jpeg", plot= grafico, width = 13.3, height = 7.5, dpi=300)

ggsave(filename = "idsc_ceara.png", plot= grafico, width = 13.3, height = 7.5, dpi=300)

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


