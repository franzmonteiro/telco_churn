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
library(rpart.plot)
library(fastDummies)
options(tigris_use_cache = T)
setwd("/home/lmonteiro/Documents/mba/tcc/telco_churn")
NEW_MODELS_DIR <- 'new_modelagem/new_modelos'

read_xlsx_custom <- function(file_name) {
    dados <- read_xlsx(file_name) %>% 
        rename_with(~ str_to_lower(str_replace_all(.x, ' ', '_')))
    
    return(dados)
}


renomear_variaveis_censo <- function(tmp, geografia='condado') {
    tmp <- tmp %>% 
        mutate(variable = case_when(variable == 'B01001_001' ~ glue('{geografia}_qtd_habitantes'),
                                    variable == 'B01001_002' ~ glue('{geografia}_qtd_habitantes_homens'),
                                    variable == 'B01002_001' ~ glue('{geografia}_idade_mediana_habitantes'),
                                    variable == 'B09001_001' ~ glue('{geografia}_qtd_habitantes_menor_18_anos'),
                                    variable == 'B19083_001' ~ glue('{geografia}_indice_gini_desigualdade_renda'),
                                    variable == 'B19113_001' ~ glue('{geografia}_renda_familiar_mediana')))
    return(tmp)
}

descricao <- read_delim('csvs/descricao_variaveis.csv', '|')


# American Community Survey
about_variaveis_censo_acs <- load_variables(year = 2017, dataset = 'acs5', cache = T) %>%
    mutate(across(all_of(c('label', 'concept')), str_to_lower),
           label = str_replace_all(label, '!!', ', ')) %>%
    filter(!str_detect(concept, 'american indian|asian|black or african american|hispanic or latino|native hawaiian and other pacific islander|other race|two or more races|white alone'))

variaveis_censo_condado <- get_acs(geography = "county",
                                   variables = c('B01001_001', 'B01001_002', 'B01002_001', 
                                                 'B09001_001', 'B19083_001', 'B19113_001'),
                                   state = "CA",
                                   year = 2017,
                                   geometry = F) %>% 
    left_join(about_variaveis_censo_acs, by = c('variable' = 'name')) %>% 
    select(NAME, variable, estimate) %>% 
    renomear_variaveis_censo(geografia = 'condado') %>% 
    spread(variable, estimate) %>% 
    mutate(NAME = str_remove(NAME, ', California'),
           condado_tx_habitantes_homens = condado_qtd_habitantes_homens / condado_qtd_habitantes,
           condado_tx_habitantes_menor_18_anos = condado_qtd_habitantes_menor_18_anos / condado_qtd_habitantes) %>% 
    select(-c(condado_qtd_habitantes_homens, condado_qtd_habitantes_menor_18_anos))


variaveis_censo_zip_code <- get_acs(geography = "zip code tabulation area",
                                    variables = c('B01001_001', 'B01001_002', 'B01002_001', 
                                                  'B09001_001', 'B19083_001', 'B19113_001'),
                                    state = "CA",
                                    year = 2017,
                                    geometry = F) %>% 
    select(NAME, variable, estimate) %>% 
    renomear_variaveis_censo(geografia = 'zip_code') %>% 
    spread(variable, estimate) %>% 
    mutate(NAME = str_remove(NAME, 'ZCTA5 '),
           zip_code_tx_habitantes_homens = zip_code_qtd_habitantes_homens / zip_code_qtd_habitantes,
           zip_code_tx_habitantes_menor_18_anos = zip_code_qtd_habitantes_menor_18_anos / zip_code_qtd_habitantes) %>% 
    select(-c(zip_code_qtd_habitantes_homens, zip_code_qtd_habitantes_menor_18_anos))


tc_demographics <- read_xlsx_custom('csvs/Telco_customer_churn_demographics.xlsx') %>% 
    select(customer_id, gender, age, married, number_of_dependents)

tc_location <- read_xlsx_custom('csvs/Telco_customer_churn_location.xlsx') %>% 
    select(customer_id, city, zip_code, latitude, longitude) %>% 
    mutate(zip_code = as.character(zip_code))

tc_population <- read_xlsx_custom('csvs/Telco_customer_churn_population.xlsx') %>% 
    select(zip_code) %>% 
    mutate(zip_code = as.character(zip_code))

tc_services <- read_xlsx_custom('csvs/Telco_customer_churn_services.xlsx') %>% 
    select(customer_id, number_of_referrals, tenure_in_months, offer, phone_service, avg_monthly_long_distance_charges, multiple_lines, internet_service, internet_type, avg_monthly_gb_download, online_security, online_backup, device_protection_plan, premium_tech_support, streaming_tv, streaming_movies, streaming_music, unlimited_data, contract, paperless_billing, payment_method, monthly_charge, total_charges, total_refunds, total_extra_data_charges, total_long_distance_charges)

tc_status <- read_xlsx_custom('csvs/Telco_customer_churn_status.xlsx') %>% 
    select(customer_id, satisfaction_score, customer_status, churn_value, cltv, churn_category, churn_reason)

zip_county <- zipcodeR::zip_code_db %>%
    filter(state == 'CA') %>% 
    select(zipcode, county)

counties_stats <- tigris::counties(state = 'CA', year = 2010) %>% 
    sf::st_drop_geometry() %>%
    select(NAMELSAD10, ALAND10) %>% 
    rename(county = 1,
           condado_area_terra_m2 = 2)

zip_code_stats <- tigris::zctas(state = 'CA', year = 2010) %>% 
    sf::st_drop_geometry() %>%
    select(ZCTA5CE10, ALAND10) %>% 
    rename(zip_code = 1,
           zip_code_area_terra_m2 = 2)

tc <- tc_demographics %>% 
    left_join(tc_location, by = 'customer_id') %>% 
    left_join(tc_population, by = 'zip_code') %>% 
    left_join(tc_services, by = 'customer_id') %>% 
    left_join(tc_status, by = 'customer_id') %>% 
    left_join(zip_county, by = c('zip_code' = 'zipcode')) %>% 
    left_join(variaveis_censo_condado, by = c('county' = 'NAME')) %>% 
    left_join(variaveis_censo_zip_code, by = c('zip_code' = 'NAME')) %>% 
    left_join(counties_stats, by = 'county') %>% 
    left_join(zip_code_stats, by = 'zip_code') %>% 
    rename(flg_device_protection_plan = device_protection_plan,
           flg_internet_service = internet_service,
           flg_online_backup = online_backup,
           flg_online_security = online_security,
           flg_phone_service = phone_service,
           flg_premium_tech_support = premium_tech_support,
           flg_multiple_lines = multiple_lines,
           flg_streaming_tv = streaming_tv,
           flg_streaming_movies = streaming_movies,
           flg_streaming_music = streaming_music,
           flg_unlimited_data = unlimited_data,
           flg_paperless_billing = paperless_billing,
           flg_married = married) %>% 
    mutate(across(matches('flg_'), ~ ifelse(.x == 'Yes', 1, 0))) %>%
    rename(flg_churn = churn_value) %>% 
    mutate(condado_densidade_populacional = condado_qtd_habitantes / condado_area_terra_m2,
           zip_code_densidade_populacional = zip_code_qtd_habitantes / zip_code_area_terra_m2,
           valor_cobranca_geral = total_charges + total_long_distance_charges + total_extra_data_charges,
           tx_valores_reembolsados = total_refunds / valor_cobranca_geral,
           tx_concentracao_cobranca_mes_q3 = monthly_charge / total_charges, # possivel indicador da quantidade de meses que o cliente esta com a companhia, e se o valor da mensalidade atual eh superior ao das mensalidades anteriores
           valor_cobrancas_extras = total_long_distance_charges + total_extra_data_charges,
           tx_contrib_cobrancas_extras_cobranca_geral = valor_cobrancas_extras / valor_cobranca_geral,
           qtd_servicos_principais = flg_internet_service + flg_phone_service,
           qtd_servicos_adicionais = flg_unlimited_data + flg_device_protection_plan + flg_online_backup + flg_online_security + flg_premium_tech_support,
           qtd_streamings = flg_streaming_movies + flg_streaming_music + flg_streaming_tv,
           across(matches('flg_'), as.factor),
           across(where(is.character), as.factor),
           satisfaction_score = as.factor(satisfaction_score),
           flg_churn_numeric = ifelse(flg_churn == '1', 1, 0))

# colnames(tc) %>% paste(collapse = '\n') %>% cat()

tc %>% select(matches('tx_')) %>% summary()

rm(list = c('tc_demographics', 'tc_location', 'tc_population', 'tc_services', 'tc_status'))

# Funcoes
get_indicadores_modelo <- function(modelo, conjunto_teste, cutoff = 0.5, descricao) {
    
    if(inherits(modelo, 'glm')) {
        tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, conjunto_teste, type = 'response') >= cutoff, 1, 0)),
                                           conjunto_teste$flg_churn,
                                           positive = '1',
                                           mode = 'sens_spec')
        
        tmp_previsoes_rocr <- ROCR::prediction(predict(modelo, conjunto_teste, type = 'response'),
                                               conjunto_teste$flg_churn)
        
    } else {
        
        if(inherits(modelo, 'train')) {
            tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, conjunto_teste, type = 'prob')[,2] >= cutoff, 'Sim', 'Não')),
                                               conjunto_teste$flg_churn,
                                               positive = 'Sim',
                                               mode = 'sens_spec')
        } else {
            tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, conjunto_teste, type = 'prob')[,2] >= cutoff, 1, 0)),
                                               conjunto_teste$flg_churn,
                                               positive = '1',
                                               mode = 'sens_spec')
        }
        
        
        
        tmp_previsoes_rocr <- ROCR::prediction(predict(modelo, conjunto_teste, type = 'prob')[,2],
                                               conjunto_teste$flg_churn)
    }
    
    model_stats <- tibble(`Ponto de corte` = cutoff,
                          `Acurácia` = tmp_model_stats$overall[['Accuracy']],
                          Sensitividade = tmp_model_stats$byClass[['Sensitivity']],
                          Especificidade = tmp_model_stats$byClass[['Specificity']])
    
    area_sob_curva <- ROCR::performance(tmp_previsoes_rocr, 'auc')@y.values %>%
        unlist()
    
    model_stats <- model_stats %>%
        mutate(AUC = area_sob_curva,
               descricao = descricao) %>% 
        select(descricao, everything())
    
    return(model_stats)
}


# Modelagem
tc_mod <- tc %>% 
    select(-c(customer_id, latitude, longitude,
              customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
    select(-matches('zip_code')) %>% 
    select(-c(city))

# verifica colunas com valores faltantes
tc_mod %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
    gather(variavel, qtd_nulos, everything()) %>% 
    arrange(desc(qtd_nulos))

variaveis_dummizar <- tc_mod %>%
    select(!where(is.double)) %>%
    select(-matches('flg_')) %>%
    colnames()

tc_dummies <- dummy_columns(.data = tc_mod,
                            select_columns = variaveis_dummizar,
                            remove_selected_columns = TRUE,
                            remove_first_dummy = TRUE) %>%
    rename_with(~ str_replace_all(.x, ' ', '_'), matches(' '))

## Amostragem
set.seed(3)
train_idx <- createDataPartition(tc_mod$flg_churn, p = .7, list = F)
tc_train <- tc_mod[train_idx,]
tc_test <- tc_mod[-train_idx,]

dim(tc_train)

set.seed(3)
dummies_train_idx <- createDataPartition(tc_dummies$flg_churn, p = .7, list = F)
dummies_tc_train <- tc_dummies[dummies_train_idx,]
dummies_tc_test <- tc_dummies[-dummies_train_idx,]

dim(dummies_tc_train)

modelo_rl_vazio <- glm(flg_churn ~ 1,
                       data = dummies_tc_train,
                       na.action = na.omit,
                       family = binomial)

modelo_rl_tudo <- glm(flg_churn ~ .,
                      data = dummies_tc_train,
                      na.action = na.omit,
                      family = binomial)


modelo_rl_forward <- MASS::stepAIC(modelo_rl_vazio,
                                   direction = 'forward',
                                   scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
                                   trace = 1)

modelo_rl_backward <- MASS::stepAIC(modelo_rl_tudo,
                                    direction = 'backward',
                                    scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
                                    trace = 1)

modelo_rl_both <- MASS::stepAIC(modelo_rl_vazio,
                                direction = 'both',
                                scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
                                trace = 1)

# modelo_arvore <- rpart(flg_churn ~ . -county,
#                        data = tc_train
#                        # control = rpart.control(cp = 0)
#                        )
# 
# printcp(modelo_arvore)
# plotcp(modelo_arvore)
# 
# modelo_arvore_podada <- prune(modelo_arvore,
#                               cp = modelo_arvore$cptable[which.min(modelo_arvore$cptable[,"xerror"]), "CP"])
# 
# printcp(modelo_arvore_podada)
# plotcp(modelo_arvore_podada)
# 
# mean(tc_test$flg_churn == predict(modelo_arvore_podada, tc_test, type = "class"))
# 
# indicadores_arvore <- seq(0.1, 0.9, 0.1) %>% 
#     map_dfr(~ get_indicadores_modelo(modelo_arvore_podada, tc_test, cutoff = .x, descricao = 'Árvore de decisão'))
# 
# rpart.rules(modelo_arvore_podada)
# 
# rpart.plot(modelo_arvore, type = 3, clip.right.labs = FALSE, branch = .3, under = TRUE)
# 
# 
# ## Random forest
# modelo_rf <- randomForest(flg_churn ~ . -county,
#                           data = tc_train,
#                           # data = dummies_tc_train,
#                           ntree = 500,
#                           importance = TRUE)
# saveRDS(modelo_rf, glue('{NEW_MODELS_DIR}/modelo_rf.rds'))
# 
# plot(modelo_rf)
# 
# mean(tc_test$flg_churn == predict(modelo_rf, tc_test, type = "class"))
# # mean(dummies_tc_test$flg_churn == predict(modelo_rf, dummies_tc_test, type = "class"))
# indicadores_rf <- seq(0.1, 0.9, 0.1) %>% 
#     map_dfr(~ get_indicadores_modelo(modelo_rf, tc_test, cutoff = .x, descricao = 'Random forest'))
# 
# sqrt(ncol(select(tc_train, -county)))
# 
# library(caret)
# tc_train2 <- tc_train %>% 
#     mutate(across(starts_with('flg_'), ~ as.factor(ifelse(.x == '1', 'Sim', 'Não'))))
# 
# rf_2 <- caret::train(flg_churn ~ .,
#                      data = tc_train2,
#                      method = 'rf',
#                      trControl = trainControl(method = 'cv',
#                                               number = 10,
#                                               classProbs = TRUE,
#                                               summaryFunction = twoClassSummary),
#                      na.action = na.omit,
#                      metric = 'ROC')
# 
# tc_test2 <- tc_test %>% 
#     mutate(across(starts_with('flg_'), ~ as.factor(ifelse(.x == '1', 'Sim', 'Não'))))
# 
# indicadores_rf2 <- seq(0.1, 0.9, 0.1) %>% 
#     map_dfr(~ get_indicadores_modelo(rf_2, tc_test2, cutoff = .x, descricao = 'RF2'))
# 
# indicadores_geral <- indicadores_arvore %>% 
#     rbind(indicadores_rf) %>% 
#     rbind(indicadores_rf2) %>% 
#     rbind(indicadores_rl_tudo) %>% 
#     rbind(indicadores_rl_forward) %>% 
#     rbind(indicadores_rl_both)
# 
# tmp <- indicadores_geral %>% 
#     group_by(descricao) %>% 
#     summarise(AUC = max(AUC)) %>% 
#     arrange(desc(AUC))
# 
# ggplot(indicadores_geral %>% 
#            select(-c(AUC)) %>% 
#            gather(var, value, Acurácia:Especificidade), aes(`Ponto de corte`, value, color = descricao)) +
#     geom_line() +
#     geom_point() +
#     scale_color_brewer(palette = 'Set3') +
#     # scale_color_viridis_d() +
#     facet_wrap(~ var) +
#     theme_dark() +
#     labs(x = 'Ponto de corte', y = NULL, color = 'Modelo')
# 
# 
# ## Xgboost
# 
# 
# ## Regressao logistica
# 
# modelo_rl_vazio <- glm(flg_churn ~ 1,
#                     data = tc_train,
#                     na.action = na.omit,
#                     family = binomial)
# saveRDS(modelo_rl_vazio, glue('{NEW_MODELS_DIR}/modelo_rl_vazio.rds'))
# 
# modelo_rl_tudo <- glm(flg_churn ~ . -county,
#                    data = tc_train,
#                    na.action = na.omit,
#                    family = binomial)
# saveRDS(modelo_rl_tudo, glue('{NEW_MODELS_DIR}/modelo_rl_tudo.rds'))
# 
# indicadores_rl_tudo <- seq(0.1, 0.9, 0.1) %>% 
#     map_dfr(~ get_indicadores_modelo(modelo_rl_tudo, tc_test, cutoff = .x, descricao = 'Regressão logística, sem stepwise'))
# 
# 
# modelo_rl_forward <- MASS::stepAIC(modelo_rl_vazio,
#                                      direction = 'forward',
#                                      scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
#                                      trace = 1)
# saveRDS(modelo_rl_forward, glue('{NEW_MODELS_DIR}/modelo_rl_forward.rds'))
# 
# indicadores_rl_forward <- seq(0.1, 0.9, 0.1) %>% 
#     map_dfr(~ get_indicadores_modelo(modelo_step_forward, tc_test, cutoff = .x, descricao = 'Regressão logística, com forward elimination'))
# 
# 
# modelo_rl_backward <- MASS::stepAIC(modelo_rl_tudo,
#                                       direction = 'backward',
#                                       scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
#                                       trace = 1)
# saveRDS(modelo_rl_backward, glue('{NEW_MODELS_DIR}/modelo_rl_backward.rds'))
# 
# modelo_rl_both <- MASS::stepAIC(modelo_rl_vazio,
#                                   direction = 'both',
#                                   scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
#                                   trace = 1)
# saveRDS(modelo_rl_both, glue('{NEW_MODELS_DIR}/modelo_rl_both.rds'))
# 
# indicadores_rl_both <- seq(0.1, 0.9, 0.1) %>% 
#     map_dfr(~ get_indicadores_modelo(modelo_rl_both, tc_test, cutoff = .x, descricao = 'Regressão logística, com forward-backward elimination'))
# 
# ## Performance dos modelos
