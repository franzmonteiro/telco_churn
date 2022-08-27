# Centrando variaveis numericas que nao sao taxas, pela grande media
tc_mod <- tc %>% 
    select(-c(customer_id, latitude, longitude,
              customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
    mutate(across(where(is.double) & !matches('tx_'), ~ .x - mean(.x, na.rm = T), .names = 'gmc_{.col}')) %>% 
    select(!(where(is.double) & !matches('tx_|gmc_'))) %>% 
    select(-matches('zip_code')) %>% 
    select(-city)



# GLMM - Multinivel
glmm_m0 <- glmmTMB(flg_churn ~ 1 + (1 | city), #satisfaction_score; city; zip_code; offer; contract
                   data = tc_train,
                   family = binomial,
                   REML = FALSE)
performance::icc(glmm_m0)
# 
# glmm_m0_reml <- glmmTMB(flg_churn ~ 1 + (1 | ), #city; zip_code; offer; contract
#                            data = tc_train,
#                            family = binomial,
#                            REML = TRUE)
# performance::icc(glmm_m0_reml)

#satisfaction_score; city; zip_code; offer; contract

efeitos_fixos <- tc_train %>%
    select(-all_of(c('flg_churn', 'contract'))) %>%
    colnames()
# paste(collapse = ' + ')

# ok <- c()
# for(i in 1:length(efeitos_fixos)) {
#     percentual_progresso <- round(100 * (i / length(efeitos_fixos)), 1)
#     message(glue("{i}   {percentual_progresso}%   {var}", var = efeitos_fixos[i]))
#     
#     if(i == 1) {
#         formula_modelo <- glue("flg_churn ~ {efeitos_fixos} + (1 | contract)",
#                                efeitos_fixos = efeitos_fixos[i])
#     } else {
#         formula_modelo <- glue("flg_churn ~ {efeitos_fixos} + (1 | contract)",
#                                efeitos_fixos = paste(c(ok, efeitos_fixos[i]), collapse = ' + '))
#     }
#     
#     message(formula_modelo)
#     
#     possibleError <- tryCatch(
#         glmmTMB(formula = as.formula(formula_modelo),
#                 data = tc_train,
#                 family = binomial,
#                 REML = TRUE),
#         error=function(e) e
#     )
#     
#     if(inherits(possibleError, "error")) {
#         mensagem_erro <- glue("Erro de convergencia, ao incluir variavel '{var}'", var = efeitos_fixos[i])
#         message(mensagem_erro)
#         next
#     }
#     
#     ok <- c(ok, efeitos_fixos[i])
# }
# 
# 
# glmm_m1 <- glmmTMB(formula = flg_churn ~ gender + flg_married + offer + (1 | contract),
#                    data = tc_train,
#                    family = binomial,
#                    REML = TRUE)
# 
# summary(glmm_m1)


# treinar_glmm <- function(criterio, efeito_aleatorio = "zip_code", reml = TRUE) {
#     efeitos_fixos <- tc_train %>%
#         select(-all_of(c('flg_churn', efeito_aleatorio))) %>%
#         colnames() %>%
#         paste(collapse = ' + ')
#     
#     modelo <- buildglmmTMB(formula = as.formula(glue("flg_churn ~ {efeitos_fixos} + (1 | {efeito_aleatorio})")),
#                            data = tc_train,
#                            family = binomial,
#                            buildmerControl = buildmerControl(crit = criterio,
#                                                              elim = criterio,
#                                                              REML = reml))
#     
#     saveRDS(modelo,
#             glue('{NEW_MODELS_DIR}/glmm_step_{criterio}_{efeito_aleatorio}{reml}.rds',
#                  criterio = str_to_lower(criterio),
#                  reml = ifelse(reml, '_reml', '')))
#     
#     return(TRUE)
# }

# # 'satisfaction_score', 'city', 'offer', 'contract'
# for(efeito_aleatorio in c('offer', 'contract')) {
#     treinar_glmm(criterio = 'AIC', efeito_aleatorio = efeito_aleatorio, reml = TRUE)
# }


# 'zip_code'
# for(criterio in c('LRT', 'LL', 'AIC', 'BIC', 'deviance')) {
#     for(efeito_aleatorio in c('county', 'city', 'offer', 'contract')) {
#         treinar_glmm(criterio = criterio, efeito_aleatorio = efeito_aleatorio, reml = TRUE)
#         treinar_glmm(criterio = criterio, efeito_aleatorio = efeito_aleatorio, reml = FALSE)
#     }
# }
