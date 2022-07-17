source("01 - Pacotes.R")


# ---- Carregamento da base de dados ----
vendas = 
  fread("dados_vendas_totais_2021.csv", dec =",", sep=";") %>%
  select(everything(), -cluster, -tempo_medio_contrato, -desemprego, perc_fidelidade) %>%
  mutate(loja = as.factor(loja), cidade = as.factor(cidade))

# ---- visualização da distribuição da receita por cidade ----

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
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background =  element_rect(colour="black", size = 0.5),
        strip.text = element_text(size = 10, color="black", face = "bold"))

# ---- modelos ----

# ----  hlm_nulo
hlm_nulo    = lme(fixed = receita ~ 1, 
                   random = ~1 | cidade, 
                   data = vendas, method = "REML")#

std_hlm_nulo = stderr_nlme(hlm_nulo)
std_hlm_nulo %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)
std_hlm_nulo$Variance.Estimatives[1] / sum(std_hlm_nulo$Variance.Estimatives)

BIC(hlm_nulo)

sm_hlm_nulo = summary(hlm_nulo)

sm_hlm_nulo$tTable %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

# ----  hlm_int

hlm_int <- 
  lme(fixed = receita ~ nps,
      random = ~1 | cidade, 
      data = vendas, 
      method = "REML")

std_hlm_int = stderr_nlme(hlm_int)

std_hlm_int %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)
std_hlm_int$Variance.Estimatives[1] / sum(std_hlm_int$Variance.Estimatives)


(1-(BIC(hlm_int) / BIC(hlm_nulo)))*100

sm_hlm_int = summary(hlm_int)

sm_hlm_int$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "~") %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

# ---- hlm_int_inc

hlm_int_inc <-
  lme(fixed = receita ~  nps, 
      random = ~ nps | cidade, 
      data = vendas, 
      method = "REML")

std_hlm_int_inc = stderr_nlme(hlm_int_inc)

std_hlm_int_inc %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)
BIC(hlm_int_inc)
(1-(BIC(hlm_int_inc) / BIC(hlm_int)))*100

sm_hlm_int_inc = summary(hlm_int_inc)
sm_hlm_int_inc$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "~") %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

# ---- hlm_final

hlm_final <- 
  lme(fixed = receita ~ nps + nps:inflacao, 
      random = ~ nps | cidade, 
      data = vendas, 
      method = "REML")

std_hlm_final = stderr_nlme(hlm_final)

std_hlm_final %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

BIC(hlm_final)
(1-(BIC(hlm_final) / BIC(hlm_int_inc)))*100

sm_hlm_final = summary(hlm_final)
sm_hlm_final$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "~") %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

lrtest(hlm_int_inc, hlm_final)

# ---- OLS

vendas_cidades_dummies <-
  dummy_cols(.data = vendas, 
             select_columns = "cidade", 
             remove_first_dummy = T, 
             remove_selected_columns = T)

ols_final_dummies <-
  lm(formula = receita ~ nps + inflacao + cidade_2 + cidade_3 + cidade_4 + cidade_5 + cidade_6 + cidade_7 + cidade_8 + cidade_9 + cidade_10 + cidade_11 + cidade_12 + cidade_13+ cidade_14+ cidade_15+ cidade_16+ cidade_17, 
     data = vendas_cidades_dummies)

ols_final_dummies_step_wise <-
  step(object = ols_final_dummies, 
       step = qchisq(p = 0.05, df = 1, lower.tail = F))

summary(ols_final_dummies_step_wise)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "~") %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

sm_ols_final_dummies_step_wise = summary(ols_final_dummies_step_wise)

sm_ols_final_dummies_step_wise$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "~") %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

# ---- analises ----

BIC(ols_nulo, hlm_nulo, hlm_int, hlm_int_inc, hlm_final, ols_final, ols_final_dummies_step_wise)
BIC(hlm_final, ols_final_dummies_step_wise)
(BIC(hlm_final) / BIC(ols_final_dummies_step_wise)) * 100
#12%

export_summs(hlm_final,
             ols_final_dummies_step_wise,
             model.names = c("HLM Modelo Final", "OLS Final")) %>%
  kable() %>%
  kable_classic(bootstrap_options = "basic", 
                html_font = "Arial",
                position = "center", 
                full_width = F, 
                font_size = 16)

#Comparação entre os LLs do modelos
data.frame(#OLS_Nulo = BIC(ols_nulo),
           #hlm_Nulo = BIC(hlm_nulo),
           #hlm_Intercept_Aleat = BIC(hlm_int),
           #hlm_Intercept_Inclin_Aleat = BIC(hlm_int_inc),
           OLS_Final = BIC(ols_final_dummies_step_wise),
           hlm_Modelo_Final = BIC(hlm_final)) %>%
  rename(#`OLS Nulo` = 1,
         #`hlm Nulo` = 2,
         #`hlm c/ Interceptos Aleatórios` = 3,
         #`hlm c/ Interceptos e Inclinações Aleatórios` = 4,
         `OLS Final` = 1,
         `hlm Modelo Final` = 2) %>%
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do BIC", 
       y = "BIC", 
       x = "Modelo Proposto") +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

# ---- Gerando os fitted values do modelo hlm Final ----
vendas$hlm_final_fitted =
  predict(hlm_final,
          vendas)

vendas$ols_final_fitted = 
  ols_final_dummies_step_wise$fitted.values

#Plotagem

vendas %>%
  ggplot() +
  geom_smooth(aes(x = receita, y = hlm_final_fitted, color = "HLM"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  
  geom_smooth(aes(x = receita, y= ols_final_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  
  # normal
  geom_smooth(aes(x = receita, y = receita), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  
  geom_point(aes(x = receita, y = hlm_final_fitted,
                 color = "HLM"), size = 0.7)  +
  
  geom_point(aes(x = receita, y = ols_final_fitted,
                 color = "OLS"), size = 0.7) +
  
  scale_color_manual("", 
                     values = c("deepskyblue2","darkorchid2"))+ #, "darkorchid2", "darkorchid2")) +
  labs(x = "Receita real", y = "Fitted Values") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12))


# ---- Transformando cidades em dummies ----
#Procedimento n-1 dummies para o contexto

#Comparando os LL dos modelos hlm Final, OLs e OLS com Dummies e Stepwise
data.frame(OLS_Dummies_Step = BIC(ols_final_dummies_step_wise),
           hlm_Modelo_Final = BIC(hlm_final)) %>%
  rename(`OLS com Dummies e Stepwise` = 1,
         `HLM Modelo Final` = 2) %>%
  reshape2::melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do BIC", 
       y = "BIC", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(ols_final_dummies_step_wise, hlm_final)
BIC(hlm_final, ols_final_dummies_step_wise)

#Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
#parâmetros estimados em cada um deles!)
export_summs(hlm_final,
             ols_final_dummies_step_wise, 
             model.names = c("hlm Final", "OLS com Dummies + Step-wise"))


