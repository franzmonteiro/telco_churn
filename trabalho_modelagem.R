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
options(tigris_use_cache = T)

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
           flg_churn_numeric = ifelse(flg_churn == '1', 1, 0)) %>% 
    select(-c(flg_internet_service))

# colnames(tc) %>% paste(collapse = '\n') %>% cat()

tc %>% select(matches('tx_')) %>% summary()

rm(list = c('tc_demographics', 'tc_location', 'tc_population', 'tc_services', 'tc_status'))


# Modelagem

get_sens_spec <- function(modelo) {
    
    previsoes <- ROCR::prediction(predict(modelo, tc_test, type = 'response'),
                                  tc_test$flg_churn)
    
    sensitividade <- ROCR::performance(previsoes, measure = 'sens')@y.values %>% unlist()
    especificidade <- ROCR::performance(previsoes, measure = 'spec')@y.values %>% unlist()
    cutoffs <- ROCR::performance(previsoes, measure = 'sens')@x.values %>% unlist()
    
    resultado <- data.frame(sensitividade = sensitividade,
                            especificidade = especificidade,
                            cutoffs = cutoffs)
    
    return(resultado)
}

plot_spec_sens <- function(dados_plotagem) {
    g1 <- ggplot(dados_plotagem, aes(cutoffs, value, color = var)) +
        geom_line() +
        scale_color_viridis_d() +
        theme_light() +
        labs(x = "Ponto de corte", y = NULL,
             color = NULL)
    
    return(g1)
}

# retorna tres estatisticas do modelo
# Acuracia; Sensitividade e especificidade
get_model_stats <- function(modelo, cutoff) {
    
    tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, tc_test, type = 'response') >= cutoff, 1, 0)),
                                       tc_test$flg_churn)
    
    model_stats <- tibble(`Acurácia` = tmp_model_stats$overall[['Accuracy']],
                          Sensitividade = tmp_model_stats$byClass[['Sensitivity']],
                          Especificidade = tmp_model_stats$byClass[['Specificity']])
    
    tmp_previsoes_rocr <- ROCR::prediction(as.double(predict(modelo, tc_test, type = 'response')), ifelse(tc_test$flg_churn == '1', 1, 0))
    
    area_sob_curva <- ROCR::performance(tmp_previsoes_rocr, 'auc')@y.values %>%
        unlist()
    
    model_stats <- model_stats %>% 
        mutate(AUC = area_sob_curva)
    
    return(model_stats)
}

## Padronizacao das variaveis numericas
# glimpse(tc)
# 
# tc %>% 
#     select(where(is.numeric)) %>% 
#     glimpse()
# 
# tc_std <- tc %>% 
#     select(-c(customer_id, 
#               city, latitude, longitude,
#               # satisfaction_score,
#               customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
#     select(-matches('zip_code')) %>% 
#     mutate(across(where(is.numeric) & !matches('tx_'),
#                   ~ (.x - mean(.x)) / sd(.x)))

tc_mod <- tc %>% 
    select(-c(customer_id, 
              city, latitude, longitude,
              # satisfaction_score,
              customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
    select(-matches('zip_code'))

## Amostragem
set.seed(3)
train_idx <- createDataPartition(tc_mod$flg_churn, p = .7, list = F)
tc_train <- tc_mod[train_idx,]
tc_test <- tc_mod[-train_idx,]

dim(tc_train)

## Treinamento
modelo_vazio <- glm(flg_churn ~ 1,
                    data = tc_train,
                    # data = na.omit(tc_train),
                    na.action = na.omit,
                    family = binomial)

modelo_tudo <- glm(flg_churn ~ .,
                   data = tc_train,
                   na.action = na.omit,
                   family = binomial)

# modelo_tudo <- glm(flg_churn ~ .^2,
#                    data = tc_train,
#                    family = binomial)

# summary(modelo_tudo)

modelo_step <- MASS::stepAIC(modelo_tudo,
                             direction = 'backward',
                             # scope = list(lower = modelo_vazio, upper = modelo_tudo,
                             trace = 1)

# saveRDS(modelo_step, 'modelos/modelo_step.rds')
# modelo_step <- readRDS('modelos/modelo_step.rds')

get_sens_spec(modelo_step) %>% 
    gather(var, value, especificidade, sensitividade) %>% 
    plot_spec_sens()

get_model_stats(modelo_step, 0.5)

resumo_modelo_step <- summary(modelo_step)

coef_modelo_step <- resumo_modelo_step$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(across(!c(rowname), ~ round(.x, 2))) %>% 
    rename(`Variável preditora` = rowname,
           Coeficiente = Estimate,
           `Erro padrão` = `Std. Error`,
           `Valor z` = `z value`)

# pagsr::salvar_planilha_excel(coef_modelo_step,
#                              'coef_modelo_step',
#                              'csvs/coefs.xlsx')

efeitos_fixos <- tc_train %>%
    select(-c(flg_churn, county)) %>%
    colnames() %>%
    paste(collapse = ' + ')

glmm_vazio <- glmmTMB(flg_churn ~ 1 + (1 | county),
                      data = tc_train,
                      family = binomial,
                      REML = T)

# Erro ao estimar modelo com todas as variaveis preditoras disponiveis
# glmm_tudo <- glmmTMB(as.formula(glue("flg_churn ~ {efeitos_fixos} + (1 | county)")),
#                      data = tc_train,
#                      family = binomial,
#                      REML = T)


# glmm_step <- buildglmmTMB(as.formula(glue("flg_churn ~ (1 | county) + {teste}")),
#                           data = tc_train,
#                           family = binomial,
#                           buildmerControl = buildmerControl(crit = 'AIC', elim = 'AIC', REML = T))

# glmm_step <- buildglmmTMB(as.formula(glue("flg_churn ~ (1 | county) + {efeitos_fixos}")),
#                           data = tc_train,
#                           family = binomial,
#                           buildmerControl = buildmerControl(crit = 'AIC', elim = 'AIC', REML = T))

# save(glmm_step, file = 'modelos/glmm_step.RData')
# saveRDS(glmm_step, 'modelos/glmm_step.rds')
glmm_step <- readRDS('modelos/glmm_step.rds')

summary(glmm_step)

tmp1 <- get_sens_spec(modelo_step) %>% 
    mutate(modelo = 'modelo_step') %>% 
    gather(var, value, especificidade, sensitividade)

tmp2 <- get_sens_spec(glmm_step) %>% 
    mutate(modelo = 'glmm_step') %>% 
    gather(var, value, especificidade, sensitividade)

tmp1 %>% 
    rbind(tmp2) %>% 
    mutate(var = glue("{modelo}: {var}")) %>% 
    select(-c(modelo)) %>% 
    plot_spec_sens()



stats_modelo_step <- get_model_stats(modelo_step, 0.5) %>% mutate(Modelo = 'Clássico')
stats_glmm_step <- get_model_stats(glmm_step, 0.5) %>% mutate(Modelo = 'Multinível')

stats_modelos <- stats_modelo_step %>% 
    rbind(stats_glmm_step) %>% 
    select(Modelo, everything())

pagsr::salvar_planilha_excel(stats_modelos, 'stats_modelos', 'csvs/stats_modelos.xlsx')

# resumo_glmm_step <- summary(glmm_step)
# resumo_glmm_step$p.coeff %>% View() # estimate
# resumo_glmm_step$se %>% View() # std. error
# resumo_glmm_step$p.t %>% View() # z value
# resumo_glmm_step$p.pv %>% View() # pr(>|z|)

coef_glmm_step <- data.frame(p.coeff = resumo_glmm_step$p.coeff,
                             se = resumo_glmm_step$se,
                             p.t = resumo_glmm_step$p.t,
                             p.pv = resumo_glmm_step$p.pv) %>% 
    rownames_to_column() %>% 
    mutate(modelo = 'coef_glmm_step',
           across(!c(rowname, modelo), ~ round(.x, 2))) %>% 
    rename(`Variável preditora` = rowname,
           Coeficiente = p.coeff,
           `Erro padrão` = se,
           `Valor z` = p.t,
           `Pr(>|z|)` = p.pv) %>% 
    select(modelo, everything())

# write_delim(coef_glmm_step, 'csvs/coef_glmm_step.csv', ';')
pagsr::salvar_planilha_excel(coef_glmm_step,
                             'coef_glmm_step',
                             'csvs/coefs.xlsx')


coef_glmm_step <- resumo_glmm_step$p.coeff %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(across(!c(rowname), ~ round(.x, 3)))

modelo_step$aic
glmm_step@model$aic
modelo_step$aic - glmm_step@model$aic

get_area_sob_curva(glmm_step@model$fitted.values, tc_train$flg_churn)

logLik(modelo_step)
logLik(glmm_step)
