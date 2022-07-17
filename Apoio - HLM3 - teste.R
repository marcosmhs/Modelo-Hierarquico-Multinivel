source("01 - Pacotes.R")

#Carregamento da base de dados
vendas = fread("dados_vendas_totais_2021.csv", dec =",", sep=";")

# ---- Explicando o dataset ----
#loja................: número geral da loja
#cidade..............: identificador da cidade
#receita.............: receita em milhões de reais em 2021
#nps.................: NPS geral da loja em 2021
#tempo_medio_contrato: tempo médio de experiência da força de vendas
#perc_fidelidade.....: % de compras realizadas por clientes do clube de fidelidade
#cluster.............: identificador do grupo da franquia (quanto maior o grupo, maior o % de receita da franquia dona da loja)
#ciclo...............: período de vendas do ano

# ---- visualização dos dados ----


#Gráfico de receita x nps (OLS)
ggplotly(
  vendas %>%
    ggplot(aes(x = receita, y = nps)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +    
    scale_colour_viridis_d() +
    labs(x = "Receita",
         y = "NPS") +
    theme_bw()
)

ggplotly(
  vendas %>%
    ggplot(aes(x = receita, y = nps, color = as.character(cidade))) +
    #geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Receita",
         y = "NPS") +
    theme_bw()
)

# ---- modelos ----

#Estimação do modelo nulo

#sem interceptos e inclinação
hlm3_mod_nulo =
  lme(fixed = receita ~ 1, 
      random = list(cidade = ~ 1, loja = ~1),
      data = vendas,
      method = "REML",
      control = lmeControl(opt = "optim"))

ols_mod_nulo =
  lm(formula = receita ~ 1, 
     data = vendas)

#Estimação do modelo com Interceptos Aleatórios
hlm3_mod_intercept =
  lme(fixed = receita ~ nps + tempo_medio_contrato + perc_fidelidade + ciclo,
      random = list(cidade = ~ciclo, loja = ~ciclo),
      data = vendas,
      method = "REML",
      control = lmeControl(opt = "optim"))

#Estimação do modelo com Interceptos e Inclinações Aleatórios
hlm3_mod_intercept_inclin =
  lme(fixed = receita ~ 
        #preditoras
        nps + tempo_medio_contrato + perc_fidelidade + cluster + ciclo,
      random = ~ nps | cidade,
      data = vendas,
      method = "REML")

#Estimação do modelo final
hlm_mod_final =
  lme(fixed = receita ~ 
        #preditoras
        tempo_medio_contrato + perc_fidelidade + ciclo +
        #inclinações dos grupos
        tempo_medio_contrato:cluster + tempo_medio_contrato:cluster +
        perc_fidelidade:cluster + perc_fidelidade:cluster,
      random = list(cidade = ~cluster, loja =~cluster),
      data = vendas,
      method = "REML")

# ---- analises ----

#Parâmetros do modelo
summary(hlm_mod_final)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(hlm_mod_final)

#Elaborando um modelo OLS para fins de comparação
ols_mod_final <- 
  lm(formula = receita ~ nps + tempo_medio_contrato + perc_fidelidade + cluster + inflacao + desemprego + ciclo,
     data = vendas)

export_summs(ols_mod_final, 
             hlm_mod_final,
             model.names = c("OLS Final", "HLM Final"))

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(ols_mod_nulo),
           HLM_Nulo = logLik(hlm3_mod_nulo),
           HLM_Intercept_Aleat = logLik(hlm3_mod_intercept),
           HLM_Intercept_Inclin_Aleat = logLik(hlm3_mod_intercept_inclin),
           OLS = logLik(ols_mod_final),
           HLM_Modelo_Final = logLik(hlm_mod_final)) %>%
  rename(`OLS Nulo` = 1,
         `HLM Nulo` = 2,
         `HLM com Interceptos Aleatórios` = 3,
         `HLM com Interceptos e Inclinações Aleatórios` = 4,
         `OLS Final` = 5,
         `HLM Modelo Final` = 6) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Gerando os fitted values do modelo HLM2 Final
vendas$hlm_fitted =
  predict(hlm_mod_final,
          vendas)

vendas$ols_fitted = 
  ols_mod_final$fitted.values

#Plotagem
vendas %>%
  ggplot() +
  geom_smooth(aes(x = receita, y = ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  
  geom_smooth(aes(x = receita, y= hlm_fitted, color = "HLM"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  
  # normal
  geom_smooth(aes(x = receita, y = receita), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  
  geom_point(aes(x = receita, y = ols_fitted,
                 color = "OLS"), size = 0.7) +
  
  geom_point(aes(x = receita, y = hlm_fitted,
                 color = "HLM"), size = 0.7)  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue2","darkorchid2"))#, "darkorchid2", "darkorchid2")) +
labs(x = "Receita", y = "Fitted Values") +
  theme_bw()


# ---- Transformando cidades em dummies ----


#Procedimento n-1 dummies para o contexto
vendas_cidades_dummies <- 
  dummy_cols(.data = vendas,
             select_columns = "cidade",
             remove_first_dummy = TRUE,
             remove_selected_columns = TRUE)

#Modelo OLS com dummies
ols_mod_final_dummies <- 
  lm(formula = receita ~ nps + tempo_medio_contrato + perc_fidelidade + inflacao + desemprego + cidade_2 +
       cidade_3 + cidade_4 + cidade_5 + cidade_6 +
       cidade_7 + cidade_8 + cidade_9 + cidade_10 + cidade_11 + cidade_12 + 
       cidade_13+ cidade_14+ cidade_15+ cidade_16+ cidade_17,
     data = vendas_cidades_dummies)

#Parâmetros
summary(ols_mod_final_dummies)

#Procedimento stepwise
ols_mod_final_dummies_step_wise <- 
  step(object = ols_mod_final_dummies,
       step = qchisq(p = 0.05, df = 1,lower.tail = FALSE))

#Parâmetros do modelo OLS estimado com dummies por escola a partir do
#procedimento Stepwise
summary(ols_mod_final_dummies_step_wise)

#Comparando os LL dos modelos HLM2 Final, OLs e OLS com Dummies e Stepwise
data.frame(OLS = logLik(ols_mod_final),
           OLS_Dummies_Step = logLik(ols_mod_final_dummies_step_wise),
           HLM2_Modelo_Final = logLik(hlm_mod_final)) %>%
  rename(`OLS` = 1,
         `OLS com Dummies e Stepwise` = 2,
         `HLM Modelo Final` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(ols_mod_final_dummies_step_wise, hlm_mod_final)

#Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
#parâmetros estimados em cada um deles!)
export_summs(ols_mod_final_dummies_step_wise, 
             hlm_mod_final,
             model.names = c("OLS com Dummies", "HLM2 Final"))


#Comparando a aderência dos fitted values dos modelos HLM2 Final, OLS e
#OLS com Dummies e Stepwise
#Gerando os fitted values do modelo OLS com Dummies e Stepwise
vendas$ols_step_fitted <- ols_mod_final_dummies_step_wise$fitted.values

#Gráfico para a comparação entre os fitted values dos modelos HLM2 Final, OLs e
#OLS com Dummies e Procedimento Stepwise
vendas %>%
  ggplot() +
  geom_smooth(aes(x = receita, y = ols_step_fitted, color = "OLS Final com Dummies"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = receita, y= hlm_fitted, color = "HLM Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = receita, y= ols_fitted, color = "OLS Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = receita, y = receita), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1", "maroon1", "darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


#Comparação entre os LLs de todos os modelos estimados neste exemplo
data.frame(OLS_Nulo = logLik(ols_mod_nulo),
           HLM_Nulo = logLik(hlm_mod_nulo),
           OLS = logLik(ols_mod_final),           
           HLM_Intercept_Aleat = logLik(hlm_mod_intercept),
           OLS_Dummies_step = logLik(ols_mod_final_dummies_step_wise),
           HLM_Intercept_Inclin_Aleat = logLik(hlm_mod_intercept_inclin),
           HLM_Modelo_Final = logLik(hlm_mod_final)) %>%
  rename(`OLS Nulo` = 1,
         `HLM Nulo` = 2,
         `OLS Completo` = 3,
         `HLM com Interceptos Aleatórios` = 4,
         `OLS com Dummies e Stepwise` = 5,
         `HLM com Interceptos e Inclinações Aleatórios` = 6,
         `HLM Modelo Final` = 7) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 5) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","darkorchid","bisque4",
                               "maroon1","bisque3","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

