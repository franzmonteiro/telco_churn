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
library(lme4)
options(tigris_use_cache = T)
theme_set(theme_light())
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

tc_scaled <- tc_mod %>% 
    mutate(across(where(is.double) & !matches('tx_'), ~ .x - mean(.x, na.rm = T)))

# efeitos aleatorios para testar
# calcular tambem coeficiente de correlacao intra-classe (icc)

efeitos_aleatorios <- select(tc_mod, !where(is.double) & !flg_churn) %>% colnames()

# Coeficientes de correlacao intraclasse
ccis <- tibble()

for(ea in efeitos_aleatorios) {
    print(ea)
    # glmm_m0 <- glmmTMB(as.formula(glue("flg_churn ~ 1 + (1 | {ea})")),
    #                    data = tc_scaled,
    #                    family = binomial,
    #                    REML = FALSE)
    
    glmm_m0 <- glmer(as.formula(glue("flg_churn ~ 1 + (1 | {ea})")),
                     data = tc_scaled,
                     family = binomial)

    cci <- performance::icc(glmm_m0)
    
    tmp_ccis <- cci %>% as.data.frame() %>% 
        mutate(efeito_aleatorio = ea)
    
    if(ncol(tmp_ccis) == 4) {
    
        ccis <- rbind(ccis,
                      tmp_ccis)
    }
}

todos_ccis <- ccis %>% 
    rename(icc_ajustado = ICC_adjusted) %>% 
    mutate(icc_ajustado = round(icc_ajustado, 3)) %>% 
    select(efeito_aleatorio, icc_ajustado) %>% 
    arrange(desc(icc_ajustado))

write_delim(todos_ccis, 'new_modelagem/todos_ccis.csv', delim = ';')

# efeitos_aleatorios_para_testar <- c('offer', 'flg_internet_service', 'contract', 'satisfaction_score')
# efeitos_aleatorios_para_testar <- c('flg_internet_service', 'contract', 'satisfaction_score')
# efeitos_aleatorios_para_testar <- c('satisfaction_score')
efeitos_aleatorios_para_testar <- c('offer', 'flg_internet_service', 'contract')


for(efeito_aleatorio in efeitos_aleatorios_para_testar) {
    variaveis_dummizar <- tc_mod %>%
        select(!where(is.double)) %>%
        select(-matches('flg_')) %>%
        select(-any_of(c(efeito_aleatorio))) %>%
        colnames()
    
    tc_scaled_dummies <- dummy_columns(.data = tc_scaled,
                                       select_columns = variaveis_dummizar,
                                       remove_selected_columns = TRUE,
                                       remove_first_dummy = TRUE) %>%
        rename_with(~ str_replace_all(.x, ' ', '_'), matches(' '))
    
    set.seed(3)
    train_idx <- createDataPartition(tc_scaled_dummies$flg_churn, p = .7, list = F)
    tc_scaled_dummies_train <- tc_scaled_dummies[train_idx,]
    tc_scaled_dummies_test <- tc_scaled_dummies[-train_idx,]
    
    efeitos_fixos <- tc_scaled_dummies_train %>%
        select(-all_of(c('flg_churn', efeito_aleatorio))) %>%
        colnames() %>%
        paste(collapse = ' + ')
    
    modelo_rl_multinivel_step <- buildmer(formula = as.formula(glue("flg_churn ~ {efeitos_fixos} + (1 | {efeito_aleatorio})")),
                                          data = tc_scaled_dummies_train,
                                          family = binomial,
                                          buildmerControl = buildmerControl(crit = 'AIC',
                                                                            elim = 'AIC',
                                                                            args = list(nAGQ = 0),
                                                                            include = glue("(1 | {efeito_aleatorio})")))
}
