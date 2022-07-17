source("01 - Pacotes.R")

# ---- Carregamento da base de dados ----
vendas = 
  fread("dados_vendas_totais_2021.csv", dec =",", sep=";") %>%
  select(everything(), -cluster) %>%
  mutate(cidade = as.factor(cidade),
         loja = as.factor(loja))

# ---- Formulas ----

f_int_nulo = as.formula("receita ~ 1")
f_inc_nula = as.formula("~1 | cidade")

f_int_aleatorio = as.formula("receita ~ perc_fidelidade + tempo_medio_contrato + desemprego")
f_inc_aleatoria = as.formula("~ perc_fidelidade + tempo_medio_contrato | cidade")

f_int_final = 
  as.formula(
    paste("receita ~ perc_fidelidade + tempo_medio_contrato +
          desemprego + 
          perc_fidelidade:desemprego + tempo_medio_contrato:desemprego", 
          sep = ""))


ctrl = lmeControl(maxIter = 100, msMaxIter = 100, niterEM = 100)

# ---- modelo nulo ----
nulo    = lme(fixed = f_int_nulo     , random = f_inc_nula     , data = vendas, method = "REML", control = ctrl)
int     = lme(fixed = f_int_aleatorio, random = f_inc_nula     , data = vendas, method = "REML", control = ctrl)
int_inc = lme(fixed = f_int_aleatorio, random = f_inc_aleatoria, data = vendas, method = "REML", control = ctrl)
final   = lme(fixed = f_int_final    , random = f_inc_aleatoria, data = vendas, method = "REML", control = ctrl)

s_nulo    = summary(nulo)
s_int     = summary(int)
s_int_inc = summary(int_inc)
s_final   = summary(final)

comparacao = 
  rbind(
    data.frame(desc = "BIC"           , nulo = BIC(nulo)                 , int = BIC(s_int)                 , int_inc = BIC(int_inc)                   , final = BIC(final)),
    data.frame(desc = "Loglik"        , nulo = logLik(nulo)              , int = logLik(int)                , int_inc = logLik(int_inc)                , final = logLik(final)),
    data.frame(desc = "p-v/Intecep"   , nulo = round(s_nulo$tTable[5], 4), int = round(s_int$tTable[1,5], 4), int_inc = round(s_int_inc$tTable[1,5], 4), final = round(s_final$tTable[1,5], 4)),
    data.frame(desc = "p-v/perc_fid"  , nulo = NA                        , int = round(s_int$tTable[2,5], 4), int_inc = round(s_int_inc$tTable[2,5], 4), final = round(s_final$tTable[2,5], 4)),
    data.frame(desc = "p-v/tem_med"   , nulo = NA                        , int = round(s_int$tTable[3,5], 4), int_inc = round(s_int_inc$tTable[3,5], 4), final = round(s_final$tTable[3,5], 4)),
    #data.frame(desc = "p-v/inflacao"  , nulo = NA                        , int = round(s_int$tTable[4,5], 4), int_inc = round(s_int_inc$tTable[4,5], 4), final = round(s_final$tTable[4,5], 4)),
    data.frame(desc = "p-v/desemprego", nulo = NA                        , int = round(s_int$tTable[4,5], 4), int_inc = round(s_int_inc$tTable[4,5], 4), final = round(s_final$tTable[4,5], 4)),
    data.frame(desc = "p-v/perc_f:des", nulo = NA                        , int = NA                         , int_inc = NA                             , final = round(s_final$tTable[5,5], 4)),    
    data.frame(desc = "p-v/tem_md:des", nulo = NA                        , int = NA                         , int_inc = NA                             , final = round(s_final$tTable[6,5], 4))
    )


fwrite(comparacao, "comparacao.csv", sep = ";", dec = ",")
