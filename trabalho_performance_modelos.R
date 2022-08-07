library(tigris) #ok
library(tidyverse) # ok
library(glue) #ok
library(readxl) #ok
library(ggrepel) #ok
library(caret) # ok
library(glmmTMB) #ok
library(lmtest) #ok
library(tidycensus) #ok
library(zipcodeR) #ok
library(buildmer) #ok
library(mgcv)
library(pROC) #ok
library(randomForest)
library(rpart)
library(fastDummies)
options(tigris_use_cache = T)
DIRETORIO_MODELOS <- glue('modelos/07-08-2022/')

tc_test <- readRDS('csvs/tc_test.rds')

# predict(modelo_arvore, tc_test, type = 'prob')

glmm_get_coeficientes <- function(modelo, descricao) {
    
    tmp <- data.frame(p.coeff = modelo@summary$p.coeff,
                      se = modelo@summary$se,
                      p.t = modelo@summary$p.t,
                      p.pv = modelo@summary$p.pv) %>%
        rownames_to_column() %>%
        mutate(descricao = descricao,
               across(!c(rowname, descricao), ~ round(.x, 2))) %>%
        rename(`Variável preditora` = rowname,
               Coeficiente = p.coeff,
               `Erro padrão` = se,
               `Valor z` = p.t,
               `Pr(>|z|)` = p.pv) %>%
        select(descricao, everything())
    
    return(tmp)
}


get_indicadores_modelo <- function(modelo, cutoff = 0.5, descricao) {

    tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, tc_test, type = 'response') >= cutoff, 1, 0)),
                                       tc_test$flg_churn,
                                       positive = '1',
                                       mode = 'sens_spec')

    model_stats <- tibble(`Ponto de corte` = cutoff,
                          `Acurácia` = tmp_model_stats$overall[['Accuracy']],
                          Sensitividade = tmp_model_stats$byClass[['Sensitivity']],
                          Especificidade = tmp_model_stats$byClass[['Specificity']])

    tmp_previsoes_rocr <- ROCR::prediction(as.double(predict(modelo, tc_test, type = 'response')),
                                           tc_test$flg_churn)

    area_sob_curva <- ROCR::performance(tmp_previsoes_rocr, 'auc')@y.values %>%
        unlist()

    model_stats <- model_stats %>%
        mutate(AUC = area_sob_curva,
               descricao = descricao) %>% 
        select(descricao, everything())

    return(model_stats)
}


modelo_arvore <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_arvore.rds'))
modelo_rf <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_rf.rds'))
modelo_vazio <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_vazio.rds'))
modelo_tudo <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_tudo.rds'))
modelo_step_forward <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_step_forward.rds'))
modelo_step_backward <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_step_backward.rds'))
modelo_step_both <- readRDS(paste0(DIRETORIO_MODELOS, 'modelo_step_both.rds'))
glmm_vazio <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_vazio.rds'))
glmm_vazio_reml <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_vazio_reml.rds'))
glmm_step_lrt_reml <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_lrt_reml.rds'))
glmm_step_ll_reml <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_ll_reml.rds'))
glmm_step_aic_reml <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_aic_reml.rds'))
glmm_step_bic_reml <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_bic_reml.rds'))
glmm_step_deviance_reml <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_deviance_reml.rds'))
glmm_step_lrt <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_lrt.rds'))
glmm_step_ll <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_ll.rds'))
glmm_step_aic <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_aic.rds'))
glmm_step_bic <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_bic.rds'))
glmm_step_deviance <- readRDS(paste0(DIRETORIO_MODELOS, 'glmm_step_deviance.rds'))


#glmm_step_aic@summary$AICtab

modelos_glmm_reml <- list('glmm_step_lrt_reml' = glmm_step_lrt_reml,
                          'glmm_step_ll_reml' = glmm_step_ll_reml,
                          'glmm_step_aic_reml' = glmm_step_aic_reml,
                          'glmm_step_bic_reml' = glmm_step_bic_reml)

modelos_glmm_ml <- list('glmm_step_lrt' = glmm_step_lrt,
                        'glmm_step_ll' = glmm_step_ll,
                        'glmm_step_aic' = glmm_step_aic,
                        'glmm_step_bic' = glmm_step_bic)

modelos_glmm <- c(modelos_glmm_reml, modelos_glmm_ml)

coeficientes_modelos_glmm_reml <- tibble()
for(i in 1:length(modelos_glmm_reml)) {
    coeficientes <- glmm_get_coeficientes(modelos_glmm_reml[[i]], names(modelos_glmm_reml)[i])
    
    coeficientes_modelos_glmm_reml <- coeficientes_modelos_glmm_reml %>% 
        rbind(coeficientes)
}

unique(coeficientes_modelos_glmm_reml$descricao)


indicadores_modelos_glmm <- tibble()
for(i in 1:length(modelos_glmm)) {
    indicadores <- get_indicadores_modelo(modelos_glmm[[i]], cutoff = 0.5, descricao = names(modelos_glmm)[i])
    
    indicadores_modelos_glmm <- indicadores_modelos_glmm %>% 
        rbind(indicadores)
}

tmp <- indicadores_modelos_glmm %>% 
    mutate(aux = str_detect(descricao, 'reml'))
