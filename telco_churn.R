library(tidyverse)
library(stringr)
library(glue)
library(lubridate)
library(readxl)
library(caret)
library(glmmTMB)
library(lmtest)
library(Information)

tc <- list.files('csvs/churn/', pattern = '*.xlsx', full.names = T) %>% 
    map(read_xlsx) %>% 
    map(rename_with, ~ str_to_lower(str_replace_all(.x, ' ', '_'))) %>% 
    map(select, !any_of(c('count'))) %>% 
    reduce(left_join) %>% 
    select(-c(id, latitude, longitude, lat_long,
              churn_label,
              customer_id, customer_status, churn_score, churn_category, churn_reason,
              quarter, state, country,
              under_30, senior_citizen, dependents, referred_a_friend)) %>% 
    rename(flg_churn = churn_value,
           zip_code_population = population) %>% 
    mutate(across(all_of(c('zip_code')), as.factor),
           across(where(is.character), as.factor),
           across(where(is.numeric), ~ .x - mean(.x), .names = '{.col}_gmc')) %>% 
    select(-flg_churn_gmc)


tmp <- create_infotables(data = select(tc, -matches('_gmc')), y = 'flg_churn')

tc <- tc %>%
    mutate(flg_churn = as.factor(flg_churn))

iv <- tmp$Summary
discarded_dep_vars <- iv %>% filter(IV < .02)

xpto <- tmp$Tables %>%
    map(~ mutate(.x, nome_var = names(.x)[1])) %>% 
    map(~ rename(.x, grupo = 1)) %>% 
    reduce(rbind) %>% 
    group_by(nome_var) %>% 
    mutate(grupo = as_factor(grupo),
           max_iv = max(IV),
           n_item = row_number()) %>% 
    ungroup() %>% 
    arrange(desc(max_iv)) %>% 
    select(-max_iv) %>% 
    mutate(n_grupo = cumsum(n_item == 1),
           tmp = ifelse(n_grupo %% 12 == 0, 1, 0),
           xpto = cumsum(coalesce(ifelse(tmp == 0 & lag(tmp) == 1, 1, 0), 0))) %>% 
    group_split(xpto)

graficos <- map(xpto, ~ ggplot(.x, aes(WOE, grupo)) + 
                    geom_col() +
                    facet_wrap(~ nome_var, scales = 'free_y') +
                    theme_light() +
                    labs(y = NULL))

graficos[[1]]
graficos[[2]]
graficos[[3]]


set.seed(1)
train <- createDataPartition(tc$flg_churn, p = .7, list = F)
tc_train <- tc[train,]
tc_test <- tc[-train,]


# Modelagem multinivel
train_glmm <- function(efeitos_fixos, efeitos_aleatorios) {
    out <- tryCatch(
        {
            formula_str <- glue('flg_churn ~ {efeitos_fixos} + {efeitos_aleatorios}')
            
            list(modelo = glmmTMB(as.formula(formula_str),
                                  data = tc_train,
                                  family = binomial, 
                                  REML = T),
                 efeitos_fixos = efeitos_fixos,
                 efeitos_aleatorios = efeitos_aleatorios,
                 error = NA,
                 warning = NA)
        },
        error=function(cond) {
            message("message:", cond)
            list(modelo = NA,
                 efeitos_fixos = efeitos_fixos,
                 efeitos_aleatorios = efeitos_aleatorios,
                 error = as.character(cond),
                 warning = NA)
        },
        warning=function(cond) {
            message("message:", cond)
            list(modelo = NA,
                 efeitos_fixos = efeitos_fixos,
                 efeitos_aleatorios = efeitos_aleatorios,
                 error = NA,
                 warning = as.character(cond))
        },
        finally={
            message(formula_str)
        }
    )
    return(out)
}


dep_vars <- tc_train %>%
    select(matches('_gmc') | !where(is.numeric)) %>%
    select(!matches(paste(discarded_dep_vars$Variable, collapse = '|'))) %>% 
    select(-flg_churn) %>% 
    colnames() %>% 
    map_dfr(train_glmm)


filter(dep_vars, !is.na(coalesce(warning, error)))

# Passo 1: Construindo modelo vazio
m0 <- glmmTMB(flg_churn ~ (1 | zip_code),
              data = tc_train,
              family = binomial,
              REML = T)

paste(dep_vars$var_preditora, collapse = ' + ')

# Funcao pra dividir o passo a passo da formacao de um perfil de estudo,
# por padrao, os passos sao separados por ;;
dep_vars_to_test <- dep_vars %>% 
    filter(is.na(coalesce(warning, error))) %>% 
    filter(!var_preditora %in% c('zip_code', 'internet_type'))

dep_vars_to_test <- dep_vars_to_test$var_preditora


ci_models <- map(12:length(dep_vars_to_test), ~ paste(dep_vars_to_test[1:.x], collapse = ' + ')) %>%
    # map(1:length(dep_vars_to_test), ~ paste(dep_vars_to_test[1:.x], collapse = ' + ')) %>% 
    map_dfr(~ train_glmm(.x, '(1 | zip_code)'))


paste(dep_vars_to_test, collapse = ' + ')

cim <- glmmTMB(flg_churn ~
                   age_gmc + number_of_dependents_gmc + zip_code_population_gmc + number_of_referrals_gmc + tenure_in_months_gmc 
               + avg_monthly_gb_download_gmc + monthly_charge_gmc + total_charges_gmc + total_long_distance_charges_gmc + total_revenue_gmc 
               + cltv_gmc + married + offer + internet_service + online_security + online_backup + device_protection_plan + premium_tech_support
               + streaming_tv + unlimited_data + contract + paperless_billing + payment_method
               + (1 | zip_code),
               data = tc_train,
               family = binomial,
               REML = T)

lrtest(m0, cim)

ai_models <- dep_vars_to_test %>% 
    map(~ train_glmm(paste(dep_vars_to_test, collapse = ' + '), glue('(1 + {.x} || zip_code)')))

log_liks <- map_dfr(ai_models, ~ data.frame(efeitos_aleatorios = .x$efeitos_aleatorios,
                                            log_lik = unique(ifelse(is.na(.x$modelo), NA, as.numeric(logLik(.x$modelo)))),
                                            aic = unique(ifelse(is.na(.x$modelo), NA, AIC(.x$modelo)))))

log_liks %>% filter(log_lik == max(log_lik, na.rm = T))

# map(ai_models, ~ lrtest(m0, .x[['modelo']]))
