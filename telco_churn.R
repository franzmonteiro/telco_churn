library(tidyverse)
library(stringr)
library(glue)
library(lubridate)
library(readxl)
library(caret)
library(glmmTMB)
library(lmtest)
library(Information)
library(performance)
library(plotly)


get_best_model <- function(models, log_lik_criterion = T) {
    if(log_lik_criterion) {
        best_model <- models %>% filter(log_lik == max(log_lik, na.rm = T))
    } else {
        best_model <- models %>% filter(aic == min(aic, na.rm = T))
    }
    
    return(best_model)
}

tc <- list.files('csvs/', pattern = '*.xlsx', full.names = T) %>% 
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

graficos <- tmp$Tables %>%
    map(~ mutate(.x, nome_var = names(.x)[1])) %>% 
    map(~ rename(.x, grupo = 1)) %>% 
    reduce(rbind) %>% 
    group_by(nome_var) %>% 
    mutate(max_iv = max(IV),
           n_item = row_number(),
           inicio_intervalo = str_extract(grupo, '[0-9.]+'),
           fim_intervalo = str_extract(grupo, '(?<=,)[0-9.]+'),
           across(matches('_intervalo'), as.numeric)) %>%
    ungroup() %>% 
    arrange(inicio_intervalo, fim_intervalo) %>% 
    mutate(grupo = as_factor(grupo)) %>%
    arrange(desc(max_iv), n_item) %>% 
    select(-max_iv) %>% 
    mutate(n_grupo = cumsum(n_item == 1),
           tmp = ifelse(n_grupo %% 12 == 0, 1, 0),
           xpto = cumsum(coalesce(ifelse(tmp == 0 & lag(tmp) == 1, 1, 0), 0))) %>% 
    group_split(xpto) %>% 
    map(~ ggplot(.x, aes(WOE, grupo)) + 
            geom_col(fill = '#fbc9a0', color = '#fbc9a0') +
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
train_glmm <- function(efeitos_fixos, efeitos_aleatorios,
                       interacoes_intra_nivel = NA,
                       interacoes_entre_niveis = NA) {
    out <- tryCatch(
        {
            formula_str <- glue('flg_churn ~ {efeitos_fixos} + {efeitos_aleatorios}')
            
            if(!is.na(interacoes_intra_nivel)) formula_str <- glue('{formula_str} + {interacoes_intra_nivel}')
            if(!is.na(interacoes_entre_niveis)) formula_str <- glue('{formula_str} + {interacoes_entre_niveis}')
            
            modelo <- glmmTMB(as.formula(formula_str),
                             data = tc_train,
                             family = binomial, 
                             REML = T)
            
            list(modelo = modelo,
                 statistics = tibble(efeitos_fixos = efeitos_fixos,
                                     efeitos_aleatorios = efeitos_aleatorios,
                                     interacoes_intra_nivel = interacoes_intra_nivel,
                                     interacoes_entre_niveis = interacoes_entre_niveis,
                                     formula_str = formula_str,
                                     log_lik = as.numeric(logLik(modelo)),
                                     aic = AIC(modelo),
                                     error = NA,
                                     warning = NA)
            )
        },
        error=function(cond) {
            message("message:", cond)
            list(modelo = NA,
                 statistics = tibble(efeitos_fixos = efeitos_fixos,
                                     efeitos_aleatorios = efeitos_aleatorios,
                                     interacoes_intra_nivel = interacoes_intra_nivel,
                                     interacoes_entre_niveis = interacoes_entre_niveis,
                                     formula_str = formula_str,
                                     log_lik = NA,
                                     aic = NA,
                                     error = as.character(cond),
                                     warning = NA)
            )
        },
        warning=function(cond) {
            message("message:", cond)
            list(modelo = NA,
                 statistics = tibble(efeitos_fixos = efeitos_fixos,
                                     efeitos_aleatorios = efeitos_aleatorios,
                                     interacoes_intra_nivel = interacoes_intra_nivel,
                                     interacoes_entre_niveis = interacoes_entre_niveis,
                                     formula_str = formula_str,
                                     log_lik = NA,
                                     aic = NA,
                                     error = NA,
                                     warning = as.character(cond))
            )
        },
        finally={
            message(paste(Sys.time(), '   ', formula_str, '\n'))
        }
    )
    return(out)
}


dep_vars <- tc_train %>%
    select(matches('_gmc') | !where(is.numeric)) %>%
    select(!matches(paste(discarded_dep_vars$Variable, collapse = '|'))) %>% 
    select(-c(flg_churn, zip_code)) %>% 
    colnames() %>% 
    map(~ train_glmm(.x, '(1 | zip_code)')) %>% 
    map_dfr(~ .x$statistics) %>% 
    arrange(desc(log_lik))


filter(dep_vars, !is.na(coalesce(warning, error)))

# Passo 1: Construindo modelo vazio
m0 <- glmmTMB(flg_churn ~ (1 | zip_code),
              data = tc_train,
              family = binomial,
              REML = T)

icc(m0)

paste(dep_vars$efeitos_fixos, collapse = ' + ')

# Nas interacoes, foi detectado que a variavel 'internet_type', ao ser incluida com as demais,
# gera problemas na convergencia do modelo.
dep_vars_to_test <- dep_vars %>% 
    filter(is.na(coalesce(warning, error))) %>% 
    filter(!efeitos_fixos %in% c('zip_code', 'internet_type'))

(dep_vars_to_test <- dep_vars_to_test$efeitos_fixos)


# Doses homeopaticas.
# A cada interacao insere uma nova variavel dependente no modelo,
# De modo a descobrir variaveis que geram problema na convergencia.
ci_models <- map(1:length(dep_vars_to_test), ~ paste(dep_vars_to_test[1:.x], collapse = ' + ')) %>%
    map(~ train_glmm(.x, '(1 | zip_code)')) %>% 
    map_dfr(~ .x$statistics) %>% 
    arrange(desc(log_lik))

# get_best_model(ci_models) %>% 
#     rbind(get_best_model(ci_models, F)) %>% 
#     View()

cim <- glmmTMB(as.formula(get_best_model(ci_models)$formula_str),
               data = tc_train,
               family = binomial,
               REML = T)

lrtest(m0, cim)


interacoes_n1 <- dep_vars_to_test[dep_vars_to_test != 'zip_code_population_gmc'] %>% 
    combn(2) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(interacao = glue('{V1}:{V2}'))


# ci_models_com_interacoes <- interacoes_n1$interacao %>%
#     map(~ train_glmm(get_best_model(ci_models)$efeitos_fixos, '(1 | zip_code)', .x)) %>%
#     map_dfr(~ .x$statistics) %>% 
#     arrange(desc(log_lik))

ci_models_com_interacoes <- readRDS('r_objects/ci_models_com_interacoes.rds')
# saveRDS(ci_models_com_interacoes, 'r_objects/ci_models_com_interacoes.rds')

interacoes_to_plot <- ci_models_com_interacoes %>% 
    filter(is.na(coalesce(warning, error))) %>% 
    left_join(interacoes_n1, by = c('interacoes_intra_nivel' = 'interacao')) %>% 
    select(V1, V2, log_lik) %>% 
    arrange(V1, V2)

tmp <- interacoes_to_plot %>% 
    mutate(V3 = V1,
           V1 = V2,
           V2 = V3) %>% 
    select(-V3) %>% 
    rbind(interacoes_to_plot)

ggplot(tmp, aes(V1, V2, fill = log_lik)) +
    geom_tile() +
    scale_fill_distiller(palette = 'YlOrRd', direction = 1) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(x = NULL, y = NULL,
         title = 'Log-likelihood dos modelos treinados com as interações das variáveis de nível 1',
         subtitle = 'Além das interações, os modelos incluem as mesmas variáveis do melhor modelo obtido na etapa anterior')

ggplotly()

# test <- train_glmm(paste(dep_vars_to_test, collapse = ' + '), '(1 | zip_code)', 'contract:tenure_in_months_gmc')
# test <- train_glmm(glue(" ( { paste(dep_vars_to_test, collapse = ' + ') } ) ^ 2" ), '(1 | zip_code)')

# get_best_model(ci_models) %>%
#     rbind(get_best_model(ci_models, F)) %>%
#     View()

# cim_com_interacao <- train_glmm(paste(dep_vars_to_test, collapse = ' + '),
#                                 '(1 | zip_code)',
#                                 'contract:offer')

cim_com_interacao <- glmmTMB(as.formula(get_best_model(ci_models_com_interacoes)$formula_str),
                             data = tc_train,
                             family = binomial,
                             REML = T)

cim_com_interacao_2 <- 2:10 %>%
    map(~ paste(ci_models_com_interacoes$interacoes_intra_nivel[1:.x], collapse = ' + ')) %>% 
    map(~ train_glmm(get_best_model(ci_models_com_interacoes)$efeitos_fixos,
                     get_best_model(ci_models_com_interacoes)$efeitos_aleatorios,
                     .x))

# Identificou-se que incluir as cinco interacoes de nivel 1, que separadamente 
# geram os modelos com os menores log-liks, resultam em um modelo com melhor
# capacidade preditiva
2:length(cim_com_interacao_2) %>% 
    map(~ lrtest(cim_com_interacao_2[[.x-1]][['modelo']],
                 cim_com_interacao_2[[.x]][['modelo']]))


ai_models <- dep_vars_to_test %>% 
    map(~ train_glmm(paste(dep_vars_to_test, collapse = ' + '), glue('(1 + {.x} || zip_code)'))) %>% 
    map_dfr(~ .x$statistics) %>% 
    arrange(desc(log_lik))

aim <- glmmTMB(as.formula(get_best_model(ai_models)$formula_str),
               data = tc_train,
               family = binomial,
               REML = T)

lrtest(cim_com_interacao, aim)


ai_models_com_interacoes <- dep_vars_to_test %>% 
    map(~ train_glmm(paste(dep_vars_to_test, collapse = ' + '),
                     glue('(1 + {.x} || zip_code)'),
                     paste(ci_models_com_interacoes$interacoes_intra_nivel[1:5], collapse = ' + '))) %>% 
    map_dfr(~ .x$statistics) %>% 
    arrange(desc(log_lik))

ai_models_com_interacoes <- readRDS('r_objects/ai_models_com_interacoes.rds')
# saveRDS(ai_models_com_interacoes, 'r_objects/ai_models_com_interacoes.rds')

aim_com_interacao <- train_glmm(get_best_model(ai_models_com_interacoes)$efeitos_fixos,
                                get_best_model(ai_models_com_interacoes)$efeitos_aleatorios,
                                get_best_model(ai_models_com_interacoes)$interacoes_intra_nivel)

# lrtest(aim, aim_com_interacao)
lrtest(cim_com_interacao, aim_com_interacao$modelo)


interacoes_n1_com_n2 <- dep_vars_to_test[dep_vars_to_test != 'zip_code_population_gmc'] %>% 
    map_chr(~ glue('zip_code_population_gmc:{.x}'))

final_models <- interacoes_n1_com_n2 %>% 
    map(~ train_glmm(get_best_model(ai_models_com_interacoes)$efeitos_fixos,
                     get_best_model(ai_models_com_interacoes)$efeitos_aleatorios,
                     get_best_model(ai_models_com_interacoes)$interacoes_intra_nivel,
                     .x)) %>% 
    map_dfr(~ .x$statistics) %>% 
    arrange(desc(log_lik))

# saveRDS(final_models, 'r_objects/final_models.rds')
final_models <- readRDS('r_objects/final_models.rds')

final_models_2 <- 2:5 %>%
    map(~ paste(final_models$interacoes_entre_niveis[1:.x], collapse = ' + ')) %>% 
    map(~ train_glmm(paste(dep_vars_to_test, collapse = ' + '),
                     get_best_model(final_models)$efeitos_aleatorios,
                     get_best_model(final_models)$interacoes_intra_nivel,
                     .x))

# saveRDS(final_models_2, 'r_objects/final_models_2.rds')
