
source("01 - Pacotes.R")

# ---- Carregamento da base de dados ----
vendas = 
  fread("TESTE.csv", dec =",", sep=";") %>%
  select(everything(), -cluster, -tempo_medio_contrato, -desemprego) %>%
  mutate(loja = as.factor(loja), cidade = as.factor(cidade))
  #filter(cidade %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
  #filter(cidade %in% c(14,15,12,))

# ---- modelos ----
hlm_nulo    = lme(fixed = receita ~ 1, random = ~1 | cidade, data = vendas, method = "REML")

hlm_int     = lme(fixed = receita ~ nps, random = ~1 | cidade, data = vendas, method = "REML")

hlm_int_inc = lme(fixed = receita ~  nps, random = ~ nps | cidade, data = vendas, method = "REML")

hlm_final   = lme(fixed = receita ~ nps + inflacao + nps:inflacao, random = ~ nps | cidade, data = vendas, method = "REML")

vendas_cidades_dummies =  dummy_cols(.data = vendas, select_columns = "cidade", remove_first_dummy = T, remove_selected_columns = T)
ols_final_dummies           = lm(formula = receita ~ nps + inflacao + cidade_2 + cidade_3 + cidade_4 + cidade_5 + cidade_6 + cidade_7 + cidade_8 + cidade_9 + cidade_10 + cidade_11 + cidade_12 + cidade_13+ cidade_14+ cidade_15+ cidade_16+ cidade_17, data = vendas_cidades_dummies)
ols_final_dummies_step_wise = step(object = ols_final_dummies, step = qchisq(p = 0.05, df = 1,lower.tail = F))

# ---- analises ----



BIC(hlm_nulo, hlm_int, hlm_int_inc, hlm_final, ols_final_dummies_step_wise)
BIC(hlm_final, ols_final_dummies_step_wise)
((BIC(ols_final_dummies_step_wise) - BIC(hlm_final)) / BIC(hlm_final))*10
#12%

#Comparação entre os LLs do modelos
data.frame(hlm_Nulo = BIC(hlm_nulo),
           hlm_Intercept_Aleat = BIC(hlm_int),
           hlm_Intercept_Inclin_Aleat = BIC(hlm_int_inc),
           OLS_Final = BIC(ols_final_dummies_step_wise),
           hlm_Modelo_Final = BIC(hlm_final)) %>%
  rename(`hlm Nulo` = 1,
         `hlm c/ Interceptos Aleatórios` = 2,
         `hlm c/ Interceptos e Inclinações Aleatórios` = 3,
         `OLS Final` = 4,
         `hlm Modelo Final` = 5) %>%
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

#  geom_point(aes(x = receita, y = ols_final_fitted,color = "OLS"), size = 0.7) +
#  geom_point(aes(x = receita, y = hlm_final_fitted,color = "HLM"), size = 0.7)  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue2","darkorchid2"))+ #, "darkorchid2", "darkorchid2")) +
  labs(x = "Receita", y = "Fitted Values") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),)

((BIC(ols_final_dummies_step_wise) - BIC(hlm_final)) / BIC(hlm_final))*10
((BIC(hlm_int_inc) - BIC(hlm_final)) / BIC(hlm_final))*10
((BIC(hlm_int) - BIC(hlm_int_inc)) / BIC(hlm_final))*10
((BIC(hlm_nulo) - BIC(hlm_int)) / BIC(hlm_final))*10