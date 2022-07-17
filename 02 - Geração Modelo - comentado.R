source("01 - Pacotes.R")

#Carregamento da base de dados
vendas = 
  fread("dados_vendas_totais_2021.csv", dec =",", sep=";") %>%
  select(everything(), -cluster, -tempo_medio_contrato, -desemprego) %>%
  mutate(loja = as.factor(loja), cidade = as.factor(cidade))

#Explicando o dataset
#loja................: número geral da loja
#cidade..............: identificador da cidade
#receita.............: receita em milhões de reais em 2021
#nps.................: NPS geral da loja em 2021
#tempo_medio_contrato: tempo médio de experiência da força de vendas
#perc_fidelidade.....: % de compras realizadas por clientes do clube de fidelidade
#cluster.............: identificador do grupo da franquia (quanto maior o grupo, maior o % de receita da franquia dona da loja)

# ---- visualização dos dados ----
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
    ggplot(aes(x = receita, y = nps, color = cidade)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Receita",
         y = "NPS") +
    theme_bw()
)

#Exploração visual do desempenho médio

vendas %>%
  group_by(cidade) %>%
  mutate(receita_media = mean(receita, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = cidade, y = receita),color = "orange", alpha = 0.5, size = 4) +
  geom_line(aes(x = cidade, y = receita_media, 
                group = 1, color = "Receita média por cidade"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Cidade",
       y = "Receita") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma
ggplotly(
  ggplot(vendas, aes(x = receita)) +
    geom_density(aes(x = receita), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "orange",
                   bins = 30) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    labs(x = "Receita",
         y = "Probabilidade") +
    theme_classic()
)


#Kernel density estimation (KDE) - função densidade de probabilidade da
ggplotly(
  ggplot(vendas, aes(x = receita)) +
    geom_density(aes(color = cidade, fill = cidade), 
                 position = "identity", alpha = 0.5) +
    labs(x = "Receita",
         y = "Probabilidade") +    
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)
  


vendas %>% 
  group_by(cidade) %>% 
  mutate(linhas = 1:n()) %>% 
  mutate(x = unlist(density(receita, n = max(linhas))["x"]),
         y = unlist(density(receita, n = max(linhas))["y"])) %>%
  ggplot() +
  geom_area(aes(x = x, y = y, group = cidade, fill = cidade), color = "black", alpha = 0.4) +
  geom_histogram(aes(x = receita, 
                     y = ..density.., 
                     fill = cidade), 
                 color = "black", 
                 position = 'identity', 
                 alpha = 0.3) +
  facet_wrap(~ cidade) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background =  element_rect(colour="black", size = 0.5),
        strip.text = element_text(size = 10, color="black", face = "bold"))

# ---- modelo nulo ----
#sem interceptos e inclinação
hlm2_nulo =
  lme(fixed = receita ~ 1, 
      random = ~ 1 | cidade,
      data = vendas,
      method = "REML", #REML - restricted estimation of maximun likelihood
      control = lmeControl(maxIter = 100, 
                           msMaxIter = 100, 
                           niterEM = 100))

summary(hlm2_nulo)
hlm2_nulo_stderr = stderr_nlme(hlm2_nulo)
hlm2_nulo_stderr
#observar que o p-value do v0j é menor que 0.05 (estatisticamente diferente de zero) 
#o que indica que o modelo HLM é recomendado e que não devemos migrar para OLS

#icc: intraclass correlation
#variancia v0j / (variancia v0j + variancia e)
hlm2_nulo_stderr$Variance.Estimatives[1] / sum(hlm2_nulo_stderr$Variance.Estimatives)
#634211.5 / (634211.5 + 265865.1)
#indica que 70,46% da variação do desempenho da receita é devido ao efeito cidade, 
#os demais 29,54% estão relacionadas a caracteristicas da loja

ols_nulo =
  lm(formula = receita ~ 1, 
     data = vendas)

lrtest(hlm2_nulo, ols_nulo)
#observar que o Loglik do HLM é menor (melhor) que o do modelo nulo

export_summs(
  hlm2_nulo, 
  ols_nulo,
  model.names = c("HLM Nulo", "OLS Nulo"))


# ---- modelo com Interceptos Aleatórios ----
hlm2_intercept =
  lme(fixed = receita ~ nps + perc_fidelidade + tempo_medio_contrato,
      random = ~ 1 | cidade,
      data = vendas,
      method = "REML",
      control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000))

summary(hlm2_intercept)
# vemos que o p-value de todas as variáveis são válidas e estatisticamente diferente de 0
#lembrando que o intercepto (B0 ou alfa) pode ter p-value acima de 0.5 o que não impacta no modelo

hlm2_intercept_stderr = stderr_nlme(hlm2_intercept)
hlm2_intercept_stderr
#seguimos com significancia estatística do intercepto, p-value menor que 0.05

hlm2_intercept_stderr$Variance.Estimatives[1] / sum(hlm2_intercept_stderr$Variance.Estimatives)

lrtest(hlm2_intercept, hlm2_nulo, ols_nulo)
#verificamos que o loglik do modelo com interceptos aleatórios é ainda menor 
#em comparação com os modelos anteriores, o que é um bom sinal


# ---- Estimação do modelo com Interceptos e Inclinações Aleatórios - faltando campo perc_fidelidade----
hlm2_intercept_inclin =
  lme(fixed = receita ~ nps + perc_fidelidade + tempo_medio_contrato,
      # efeitos aleatórios de inclinação
      random = ~ nps + perc_fidelidade | cidade,
      data = vendas,
      method = "REML",
      control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000))

summary(hlm2_intercept_inclin)

hlm2_intercept_inclin_stderr = stderr_nlme(hlm2_intercept_inclin)
hlm2_intercept_inclin_stderr
#se o p-value de var(v1j) fosse maior que 0.05 o loglik deste modelo não traria grandes diferenças
#vs o modelo anterior

#icc: intraclass correlation
#variancia v0j / (variancia v0j + variancia e)
hlm2_intercept_inclin_stderr$Estimatives[1] / sum(hlm2_intercept_inclin_stderr$Estimatives)
#6214617.18712 / (214617.18712 + 81.44528 + 111160.79072)
#indica que 65,88% da variação do desempenho da receita é devido ao efeito cidade, 
#os demais 34,22% estão relacionadas aos demais níveis

#olhando para o NPS no nível loja
hlm2_intercept_inclin_stderr$Estimatives[2] / sum(hlm2_intercept_inclin_stderr$Estimatives)
#81.44528 / (214617.18712 + 81.44528 + 111160.79072)
#?indica que 0.02 da variância está sendo explicada?

lrtest(hlm2_intercept_inclin, hlm2_intercept, hlm2_nulo, ols_nulo)
#

# ---- Estimação do modelo final ----
hlm2_final =
  lme(fixed = receita ~ nps + perc_fidelidade + tempo_medio_contrato + inflacao + desemprego +
      #inclinações dos grupos
      nps:inflacao + nps:desemprego +
      perc_fidelidade:inflacao + perc_fidelidade:desemprego +
      tempo_medio_contrato:inflacao + tempo_medio_contrato:desemprego,
      random = ~ nps + perc_fidelidade | cidade,
      data = vendas,
      method = "REML",
      control = lmeControl(maxIter = 100, 
                           msMaxIter = 100, 
                           niterEM = 100))

summary(hlm2_final)

hlm2_final_stderr = stderr_nlme(hlm2_final)
hlm2_final_stderr

#geral
(hlm2_final_stderr$Estimatives[1] + hlm2_final_stderr$Estimatives[2]) / sum(hlm2_final_stderr$Estimatives)
#interceptos
(hlm2_final_stderr$Estimatives[1] ) / sum(hlm2_final_stderr$Estimatives)
#inclinações
(hlm2_final_stderr$Estimatives[2] ) / sum(hlm2_final_stderr$Estimatives)

lrtest(hlm2_final, hlm2_intercept_inclin, hlm2_intercept, hlm2_nulo, ols_nulo)

# ---- ols final ----


#Elaborando um modelo OLS para fins de comparação
ols_final <- 
  lm(formula = receita ~ nps + perc_fidelidade + cidade,
     data = vendas)

export_summs(hlm2_final,
             ols_final,
             model.names = c("HLM2 Final", "OLS Final"))

lrtest(hlm2_final, ols_final, hlm2_intercept_inclin, hlm2_intercept, hlm2_nulo, ols_nulo)

# ---- analises ----

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(ols_nulo),
           HLM2_Nulo = logLik(hlm2_nulo),
           HLM2_Intercept_Aleat = logLik(hlm2_intercept),
           HLM2_Intercept_Inclin_Aleat = logLik(hlm2_intercept_inclin),
           OLS = logLik(ols_final),
           HLM2_Modelo_Final = logLik(hlm2_final)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM com Interceptos e Inclinações Aleatórios` = 4,
         `OLS Final` = 5,
         `HLM2 Modelo Final` = 6) %>%
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

