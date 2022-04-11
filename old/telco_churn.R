library(tidyverse)
library(stringr)
library(glue)
library(readxl)
library(caret)
library(glmmTMB)
library(lmtest)
library(Information)
library(performance)
library(GGally)
library(lubridate)


CSVS_DIR <- 'csvs/models'
R_OBJECTS_DIR <- 'r_objects/models'


get_best_model <- function(models, log_lik_criterion = T) {
    if(log_lik_criterion) {
        best_model <- models %>% filter(log_lik == max(log_lik, na.rm = T))
    } else {
        best_model <- models %>% filter(aic == min(aic, na.rm = T))
    }
    
    return(best_model)
}


get_models_time_stats <- function(models) {
    # Tempo medio pra treinar cada modelo
    # Tempo total, necessario para treinar todos os modelos, inclusive os que nao convergiram
    time_stats <- summarise(models,
                            tempo_medio_treino = mean(training_time),
                            tempo_total_treino = sum(training_time))
    
    return(time_stats)
}


# Modelagem multinivel
train_glmm <- function(efeitos_fixos = NA, efeitos_aleatorios = NA,
                       interacoes_intra_nivel = NA,
                       interacoes_entre_niveis = NA,
                       time_unit = 'secs',
                       base_name = 'test',
                       save_rds = T) {
    out <- tryCatch(
        {
            params <- c(efeitos_fixos, interacoes_intra_nivel, interacoes_entre_niveis, efeitos_aleatorios)
            params <- paste0(params[!is.na(params)], collapse = ' + ')
            formula_str <- glue('flg_churn ~ {params}')
            data_hora <- str_replace_all(Sys.time(), '-|:', '_')
            dir.create(glue('{R_OBJECTS_DIR}/{base_name}/'), showWarnings = F)
            
            csv_file <- glue('{CSVS_DIR}/{base_name}.csv')
            rds_file <- glue('{R_OBJECTS_DIR}/{base_name}/model_{data_hora}.rds')
            
            statistics <- tibble(efeitos_fixos = efeitos_fixos,
                                 efeitos_aleatorios = efeitos_aleatorios,
                                 interacoes_intra_nivel = interacoes_intra_nivel,
                                 interacoes_entre_niveis = interacoes_entre_niveis,
                                 formula_str = formula_str,
                                 start_time = Sys.time(),
                                 end_time = NA,
                                 training_time = NA,
                                 time_unit = time_unit,
                                 log_lik = NA,
                                 aic = NA,
                                 error = NA,
                                 warning = NA)
            
            modelo <- glmmTMB(as.formula(formula_str),
                              data = tc_train,
                              family = binomial, 
                              REML = F)
            
            statistics <- mutate(statistics,
                                 end_time = Sys.time(),
                                 training_time = difftime(end_time, start_time, units = time_unit),
                                 log_lik = as.numeric(logLik(modelo)),
                                 aic = AIC(modelo))
            
            if(save_rds) saveRDS(modelo, rds_file)
            
            write_delim(statistics, csv_file, delim = ';', append = ifelse(file.exists(csv_file), T, F))
            
            list(modelo = modelo,
                 statistics = statistics)
        },
        error=function(cond) {
            message("message:", cond)
            
            statistics <- mutate(statistics,
                                 end_time = Sys.time(),
                                 training_time = difftime(end_time, start_time, units = time_unit),
                                 error = as.character(cond))
            
            write_delim(statistics, csv_file, delim = ';', append = ifelse(file.exists(csv_file), T, F))
            
            list(modelo = NA,
                 statistics = statistics)
        },
        warning=function(cond) {
            message("message:", cond)
            
            statistics <- mutate(statistics,
                                 end_time = Sys.time(),
                                 training_time = difftime(end_time, start_time, units = time_unit),
                                 warning = as.character(cond))
            
            write_delim(statistics, csv_file, delim = ';', append = ifelse(file.exists(csv_file), T, F))
            
            list(modelo = NA,
                 statistics = statistics)
        },
        finally={
            message(paste(Sys.time(), '   ', formula_str, '\n'))
        }
    )
    return(out)
}


tc <- list.files('csvs/', pattern = '*.xlsx', full.names = T) %>% 
    map(read_xlsx) %>% 
    map(rename_with, ~ str_to_lower(str_replace_all(.x, ' ', '_'))) %>% 
    map(select, !any_of(c('count'))) %>% 
    reduce(left_join) %>% 
    select(-c(id, latitude, longitude, lat_long,
              churn_label,
              customer_id,
              customer_status, churn_score, churn_category, churn_reason,
              quarter, state, country,
              under_30, senior_citizen, dependents, referred_a_friend)) %>% 
    rename(flg_churn = churn_value,
           zip_code_population = population) %>% 
    mutate(across(all_of(c('zip_code', 'satisfaction_score')), as.factor),
           across(where(is.character), as.factor),
           across(where(is.numeric), ~ .x - mean(.x), .names = '{.col}_gmc')
           # across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x), .names = '{.col}_std')
           ) %>% 
    select(-flg_churn_gmc)


estatisticas_descritivas <- tc %>% 
    group_by(zip_code) %>% 
    summarise(qtd_clientes = n(),
              taxa_churn = sum(flg_churn) / n())

summary(estatisticas_descritivas$qtd_clientes)

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





# Correlacao entre variaveis numericas
corr_m <- tc %>%
    select(where(is.numeric) & -matches('_std')) %>%
    # select(where(is.numeric) & -matches('_gmc')) %>% 
    cor()

corr_m[upper.tri(corr_m, diag = T)] <- NA
corr_m <- corr_m %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(v1 = 1) %>% 
    gather(v2, correlacao, -v1) %>% 
    mutate(correlacao = round(correlacao, 2)) %>% 
    filter(!is.na(correlacao)) %>% 
    arrange(desc(abs(correlacao)))


set.seed(1)
train <- createDataPartition(tc$flg_churn, p = .7, list = F)
tc_train <- tc[train,]
tc_test <- tc[-train,]

# Construindo variaveis
tc_train <- tc_train %>% 
    group_by(zip_code) %>% 
    mutate(pct_zip_cliente_companhia = n() / zip_code_population) %>% 
    ungroup() %>% 
    mutate(v1 = total_refunds / total_charges,
           # v2 = total_extra_data_charges / total_charges,
           # v3 = total_long_distance_charges / total_charges,
           # v4 = (total_extra_data_charges + total_long_distance_charges) / total_charges,
           v5 = monthly_charge / total_charges
           # v6 = total_charges / tenure_in_months,
           )

# Modelo logistico binario classico
cm <- glm(flg_churn ~ . -city -total_revenue -internet_service + offer:contract + offer:multiple_lines
          + zip_code + pct_zip_cliente_companhia,
          # + tenure_in_months:satisfaction_score,# + avg_monthly_long_distance_charges:total_long_distance_charges, #+ avg_monthly_gb_download:unlimited_data,
    family = 'binomial',
    data = select(tc_train, -matches('_gmc')))

summary(cm)
logLik(cm)

table(cut(tc_train$v5, seq(0,1,.1), include.lowest = T, right = T),
      cut(tc_train$tenure_in_months, seq(1,72,12), include.lowest = T, right = T),
      tc_train$flg_churn) %>% 
    prop.table(1)

# Modelagem multinivel
# Treina diversos modelos, um para cada variavel dependente
# e verifica quais convergem
dep_vars <- tc_train %>%
    select(matches('_std') | !where(is.numeric)) %>%
    # select(matches('_gmc') | !where(is.numeric)) %>%
    select(!matches(paste(discarded_dep_vars$Variable, collapse = '|'))) %>%
    select(-c(flg_churn, zip_code)) %>%
    colnames() %>%
    map(~ train_glmm(.x, '(1 | zip_code)', base_name = 'dep_vars')) %>%
    map_dfr(~ .x$statistics) %>%
    arrange(desc(log_lik))

dep_vars <- read_delim(glue('{CSVS_DIR}/dep_vars.csv'), ';') %>% 
    mutate(across(all_of(c('start_time', 'end_time')), ~ .x - hours(3)))
# dep_vars <- list.files(glue('{R_OBJECTS_DIR}/dep_vars/'), full.names = T) %>% 
#     map(readRDS)

get_models_time_stats(dep_vars) / 60

# filter(dep_vars, !is.na(coalesce(warning, error)))

# Passo 1: Construindo modelo vazio
m0 <- train_glmm(efeitos_aleatorios = '(1 | zip_code)', base_name = 'm0')

icc(m0$modelo)
get_models_time_stats(m0$statistics)

# Nas interacoes, foi detectado que as variaveis 'internet_service' e 'internet_type',
# ao serem incluidas juntas no modelo, com as demais variaveis,
# geram problemas na convergencia.
# Deste modo, a variavel 'internet_type' foi escolhida para ser mantida, uma
# vez que aumenta a capacidade preditiva do modelo, comparada a 'internet_service'
dep_vars %>%
    filter(efeitos_fixos %in% c('internet_type', 'internet_service')) %>% 
    arrange(desc(log_lik)) %>% 
    select(efeitos_fixos, log_lik, aic)

principal_dep_vars <- dep_vars %>% 
    filter(is.na(coalesce(warning, error))) %>%
    filter(!efeitos_fixos %in% c('internet_service'))

# other_dep_vars <- dep_vars %>% 
#     filter(!is.na(coalesce(warning, error))
#            | efeitos_fixos == 'internet_service')

# Organiza o vetor de variaveis dependentes,
# de modo que as variaveis problematicas,
# sejam as ultimas a serem incluidas no modelo,
# a fim de varificar se continuam impossibilitando a convergencia
dep_vars <- principal_dep_vars$efeitos_fixos
# dep_vars <- c(principal_dep_vars$efeitos_fixos, other_dep_vars$efeitos_fixos)

# Doses homeopaticas.
# A cada interacao insere uma nova variavel dependente no modelo,
# De modo a descobrir variaveis que geram problema na convergencia.
cims <- map(1:length(dep_vars), ~ paste(dep_vars[1:.x], collapse = ' + ')) %>%
    map(~ train_glmm(.x, '(1 | zip_code)', base_name = 'cims')) %>%
    map_dfr(~ .x$statistics) %>%
    arrange(desc(log_lik))

# cims <- read_delim(glue('{CSVS_DIR}/cims.csv'), ';')
get_models_time_stats(cims) / 60


# Algumas vezes, o modelo com maior loglik, nao eh o modelo com o menor AIC
# get_best_model(cims) %>%
#     rbind(get_best_model(cims, F)) %>%
#     View()

# Identifica o melhor modelo, pelo criterio do loglik
cim <- train_glmm(get_best_model(cims)$efeitos_fixos,
                  get_best_model(cims)$efeitos_aleatorios)

summary(cim$modelo)

# Testa se a inclusao dos efeitos fixos
# melhora o poder preditivo do modelo nulo
# lrtest(m0$modelo, cim$modelo)

# 'zip_code_population_gmc' eh uma variavel de nivel dois.
# Nao fez-se interacoes das variaveis 'other_dep_vars' com as demais,
# uma vez que estas variaveis base nao serao inclusas no modelo,
# por impossibilitarem a convergencia
level_one_interactions <- principal_dep_vars %>% 
    filter(efeitos_fixos != 'zip_code_population_gmc')

level_one_interactions <- level_one_interactions$efeitos_fixos %>% 
    combn(2) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(interaction = glue('{V1}:{V2}'))

# Ao incluir as interacoes, deve-se manter as variaveis de nivel 1 base,
# que interagem com outras.
# Por isso, aqui manteve-se todas as variaveis dependentes principais nos efeitos fixos.
cims_one_interaction <- level_one_interactions$interaction %>%
    map(~ train_glmm(paste(principal_dep_vars$efeitos_fixos, collapse = ' + '),
                     '(1 | zip_code)',
                     .x,
                     base_name = 'cims_one_interaction')) %>%
    map_dfr(~ .x$statistics) %>%
    arrange(desc(log_lik))

cims_one_interaction <- read_delim(glue('{CSVS_DIR}/cims_one_interaction.csv'), ';')
get_models_time_stats(cims_one_interaction)

interactions_to_plot <- cims_one_interaction %>% 
    filter(is.na(coalesce(warning, error))) %>% 
    left_join(level_one_interactions, by = c('interacoes_intra_nivel' = 'interacao')) %>% 
    select(V1, V2, log_lik) %>% 
    arrange(V1, V2)

tmp <- interactions_to_plot %>% 
    mutate(V3 = V1,
           V1 = V2,
           V2 = V3) %>% 
    select(-V3) %>% 
    rbind(interactions_to_plot)

ggplot(tmp, aes(V1, V2, fill = log_lik)) +
    geom_tile() +
    scale_fill_distiller(palette = 'YlOrRd', direction = 1) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(x = NULL, y = NULL,
         title = 'Log-likelihood dos modelos treinados com as interações das variáveis de nível 1',
         subtitle = 'Além das interações, os modelos incluem as mesmas variáveis do melhor modelo obtido na etapa anterior')

# ggplotly()

# cim_one_interaction <- train_glmm(get_best_model(cims_one_interaction)$efeitos_fixos,
#                                   get_best_model(cims_one_interaction)$efeitos_aleatorios,
#                                   get_best_model(cims_one_interaction)$interacoes_intra_nivel)

# Testa se a inclusao da interacao de nivel 1
# aumenta significativamente, o poder preditivo do modelo
# lrtest(cim$modelo, cim_one_interaction$modelo)

# Doses homeopaticas.
# Inclue de forma incremental, uma interacao de nivel 1 por vez
# cims_n_interactions <- 2:nrow(cims_one_interaction) %>%
#     map(~ paste(cims_one_interaction$interacoes_intra_nivel[1:.x], collapse = ' + ')) %>% 
#     map(~ train_glmm(paste(principal_dep_vars$efeitos_fixos, collapse = ' + '),
#                      '(1 | zip_code)',
#                      .x,
#                      base_name = 'cims_n_interactions'))

cims_n_interactions <- read_delim(glue('{CSVS_DIR}/cims_n_interactions.csv'), ';')

get_best_model(cims_n_interactions) %>% 
    mutate(qtd_interactions = str_count(interacoes_intra_nivel, '[+]')) %>% 
    View()

cims_n_interactions_plot <- cims_n_interactions %>% 
    mutate(qtd_interactions = str_count(interacoes_intra_nivel, '[+]')) %>% 
    select(qtd_interactions, log_lik, aic) %>% 
    gather(var, value, -qtd_interactions)

ggplot(cims_n_interactions, aes(qtd_interactions, value)) +
    geom_line() +
    # geom_point() +
    facet_wrap(~ var, scales = 'free_y', nrow = 2) +
    theme_light() +
    labs(x = 'Quantidade de interações de nível 1', y = NULL)

# Identificou-se que incluir as cinco interacoes de nivel 1, que separadamente 
# geram os modelos com os maiores log-liks, resultam em um modelo com melhor
# capacidade preditiva
2:length(cims_n_interactions) %>% 
    map(~ lrtest(cims_n_interactions[[.x-1]][['modelo']],
                 cims_n_interactions[[.x]][['modelo']]))

# Teste se se a inclusao de novas interacoes
# aumenta o poder preditivo do modelo
lrtest(cim_one_interaction$modelo, cims_n_interactions$modelo)

# aims <- principal_dep_vars$efeitos_fixos %>% 
#     map(~ train_glmm(paste(principal_dep_vars$efeitos_fixos, collapse = ' + '),
#                      glue('(1 + {.x} || zip_code)'),
#                      base_name = 'aims')) %>% 
#     map_dfr(~ .x$statistics) %>% 
#     arrange(desc(log_lik))
# 
# aim <- train_glmm(get_best_model(aims)$efeitos_fixos,
#                   get_best_model(aims)$efeitos_aleatorios,
#                   get_best_model(aims)$interacoes_intra_nivel,
#                   get_best_model(aims)$interacoes_entre_niveis)
# 
# lrtest(cim_one_interaction, aim)
# 
# 
# aims_com_interacoes <- dep_vars %>% 
#     map(~ train_glmm(paste(principal_dep_vars$efeitos_fixos, collapse = ' + '),
#                      glue('(1 + {.x} || zip_code)'),
#                      paste(cims_one_interaction$interacoes_intra_nivel[1:5], collapse = ' + '))) %>% 
#     map_dfr(~ .x$statistics) %>% 
#     arrange(desc(log_lik))
# 
# aims_com_interacoes <- readRDS('r_objects/aims_com_interacoes.rds')
# # saveRDS(aims_com_interacoes, 'r_objects/aims_com_interacoes.rds')
# 
# aim_com_interacao <- train_glmm(get_best_model(aims_com_interacoes)$efeitos_fixos,
#                                 get_best_model(aims_com_interacoes)$efeitos_aleatorios,
#                                 get_best_model(aims_com_interacoes)$interacoes_intra_nivel)
# 
# # lrtest(aim, aim_com_interacao)
# lrtest(cim_one_interaction, aim_com_interacao$modelo)
# 
# 
# level_one_interactions_com_n2 <- dep_vars[dep_vars != 'zip_code_population_gmc'] %>% 
#     map_chr(~ glue('zip_code_population_gmc:{.x}'))
# 
# final_models <- level_one_interactions_com_n2 %>% 
#     map(~ train_glmm(get_best_model(aims_com_interacoes)$efeitos_fixos,
#                      get_best_model(aims_com_interacoes)$efeitos_aleatorios,
#                      get_best_model(aims_com_interacoes)$interacoes_intra_nivel,
#                      .x)) %>% 
#     map_dfr(~ .x$statistics) %>% 
#     arrange(desc(log_lik))
# 
# # saveRDS(final_models, 'r_objects/final_models.rds')
# final_models <- readRDS('r_objects/final_models.rds')
# 
# final_models_2 <- 2:5 %>%
#     map(~ paste(final_models$interacoes_entre_niveis[1:.x], collapse = ' + ')) %>% 
#     map(~ train_glmm(paste(dep_vars, collapse = ' + '),
#                      get_best_model(final_models)$efeitos_aleatorios,
#                      get_best_model(final_models)$interacoes_intra_nivel,
#                      .x))
# 
# # saveRDS(final_models_2, 'r_objects/final_models_2.rds')
