library(sf)
library(tigris)
library(tidyverse) # ok
library(stringr)
library(glue)
library(readxl)
library(ggpubr)
library(ggrepel)
library(caret) # ok
library(glmmTMB) # ok
library(lmtest) # ok
library(tidycensus)
library(zipcodeR)
# library(StepReg)
library(buildmer)
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

# write_delim(about_variaveis_censo_acs, 'csvs/about_variaveis_censo_acs.csv', '|')

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
    st_drop_geometry() %>%
    select(NAMELSAD10, ALAND10) %>% 
    rename(county = 1,
           condado_area_terra_m2 = 2)

zip_code_stats <- tigris::zctas(state = 'CA', year = 2010) %>% 
    st_drop_geometry() %>%
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
           flg_churn_numeric = ifelse(flg_churn == '1', 1, 0)) %>% 
    select(-c(flg_internet_service))

colnames(tc) %>% paste(collapse = '\n') %>% cat()

tc %>% select(matches('tx_')) %>% summary()

rm(list = c('tc_demographics', 'tc_location', 'tc_population', 'tc_services', 'tc_status'))


tc$flg_churn %>% table() %>% prop.table()
tc$customer_status %>% table() %>% prop.table()
tc$churn_category %>% table() %>% prop.table()


## Graficos

# Distribuicao dos clientes da companhia, por condado
qtd_clientes_condado <- tc %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(faixa_qtd_clientes = cut(qtd_clientes,
                                    quantile(qtd_clientes, probs = seq(0, 1, 0.2)),
                                    include.lowest = T, right = F,
                                    dig.lab = 5, ordered_result = T))

sf_condados_1 <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(qtd_clientes_condado, by = c('NAMELSAD10' = 'county'))

top_3_maiores <- head(arrange(sf_condados_1, desc(qtd_clientes)), 3)
top_3_menores <- head(arrange(sf_condados_1, qtd_clientes), 3)
top_6_condados <- top_3_maiores %>% 
    rbind(top_3_menores)

g1_clientes <- ggplot() +
    geom_sf(data = sf_condados_1, mapping = aes(fill = faixa_qtd_clientes), size = 0.3) +
    geom_label_repel(
        data = top_6_condados,
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_d() +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL, title = 'Qtd clientes')


tx_contrib_clientes_condado <- tc %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(tx_contrib_clientes = qtd_clientes / sum(qtd_clientes))

sf_condados_2 <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(tx_contrib_clientes_condado, by = c('NAMELSAD10' = 'county'))

top_3_maiores <- head(arrange(sf_condados_2, desc(tx_contrib_clientes)), 3)
top_3_menores <- head(arrange(sf_condados_2, tx_contrib_clientes), 3)
top_6_condados <- top_3_maiores %>% 
    rbind(top_3_menores)

g2_clientes <- ggplot() +
    geom_sf(data = sf_condados_2,
            mapping = aes(fill = tx_contrib_clientes), size = 0.3) +
    geom_label_repel(
        data = top_6_condados,
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL, fill = NULL, title = 'Concentração de clientes')

g3_clientes <- ggarrange(g1_clientes, g2_clientes, common.legend = F, legend = 'right', align = 'hv')

ggsave("plots/clientes_condado.png", plot = g3_clientes, width = 9, height = 5)



proporcao_hab_clientes_condado <- variaveis_censo_condado %>% 
    left_join(tx_contrib_clientes_condado, by = c('NAME' = 'county')) %>% 
    mutate(proporcao_habitantes_clientes = qtd_clientes / condado_qtd_habitantes)

sf_condados_3 <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(proporcao_hab_clientes_condado, by = c('NAMELSAD10' = 'NAME'))

top_3_maiores <- head(arrange(sf_condados_3, desc(proporcao_habitantes_clientes)), 3)
top_3_menores <- head(arrange(sf_condados_3, proporcao_habitantes_clientes), 3)
top_6_condados <- top_3_maiores %>% 
    rbind(top_3_menores)

ggplot() +
    geom_sf(data = sf_condados_3,
            mapping = aes(fill = proporcao_habitantes_clientes), size = 0.3) +
    geom_label_repel(
        data = top_6_condados,
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL, title = 'Prop. habitantes que foram ou são clientes')


ggsave("plots/prop_hab_clientes_condado.png", width = 9, height = 5)

prep_motivo_churn <- tc %>% 
    filter(flg_churn_numeric == 1) %>% 
    group_by(churn_category, churn_reason) %>% 
    summarise(qtd_clientes = n()) %>% 
    ungroup() %>% 
    mutate(tx_contrib_clientes_reason = qtd_clientes / sum(qtd_clientes),
           tx_contrib_clientes_reason = round(tx_contrib_clientes_reason * 100, 1)) %>% 
    group_by(churn_category) %>% 
    mutate(qtd_clientes_category = sum(qtd_clientes)) %>% 
    ungroup() %>% 
    mutate(tx_contrib_category = qtd_clientes_category / sum(qtd_clientes),
           tx_contrib_category = round(tx_contrib_category * 100, 1),
           churn_category = paste(churn_category, tx_contrib_category, sep = '; '))

g1_motivo_churn <- ggplot(prep_motivo_churn,
       aes(reorder(churn_reason, qtd_clientes), qtd_clientes,
           fill = churn_category, label = tx_contrib_clientes_reason)) +
    geom_col(position = 'dodge') +
    geom_text(hjust = -0.15, size = 2.5, position = position_dodge(width = 1)) +
    scale_fill_viridis_d() +
    labs(x = NULL, y = NULL, fill = "Motivo") +
    coord_flip() +
    theme_light()

ggsave("plots/motivo_churn_geral.png", plot = g1_motivo_churn, width = 9, height = 5)


txs_churn_condado <- tc %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn_numeric)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes,
           taxa_contrib_qtd_churn = qtd_clientes_churn / sum(qtd_clientes_churn, na.rm = T)) %>%
    arrange(desc(taxa_churn))

sf_condados <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(txs_churn_condado, by = c('NAMELSAD10' = 'county'))

g1_condado <- ggplot() +
    geom_sf(data = sf_condados, mapping = aes(fill = taxa_churn), size = 0.3) +
    geom_label_repel(
        data = head(arrange(sf_condados, desc(taxa_churn)), 3),
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    # scale_fill_distiller(palette = 'YlOrRd', direction = 1, labels = scales::percent_format(accuracy = 1)) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = '', title = '% Churn')


g2_condado <- ggplot() +
    geom_sf(data = sf_condados, mapping = aes(fill = taxa_contrib_qtd_churn), size = 0.3) +
    geom_label_repel(
        data = head(arrange(sf_condados, desc(taxa_contrib_qtd_churn)), 3),
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    # scale_fill_distiller(palette = 'YlOrRd', direction = 1, labels = scales::percent_format(accuracy = 1)) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL, fill = '',
         title = 'Concentração do churn')

g3_condado <- ggarrange(g1_condado, g2_condado, common.legend = F, legend = 'right', align = 'hv')

ggsave("plots/churn_por_condado.png", plot = g3_condado, width = 9, height = 5)


# motivo_churn <- tc %>% 
#     filter(flg_churn_numeric == 1) %>% 
#     group_by(county, churn_category) %>% 
#     summarise(qtd_clientes = n()) %>% 
#     mutate(tx_clientes = qtd_clientes / sum(qtd_clientes)) %>% 
#     filter(qtd_clientes == max(qtd_clientes, na.rm = T)) %>% 
#     summarise(tx_clientes_motivo = sum(tx_clientes, na.rm = T),
#               churn_category = paste(churn_category, collapse = ' & '))

# Modelagem

## Padronizacao das variaveis numericas
glimpse(tc)

tc_std <- tc %>% 
    select(-c(customer_id, 
              city, latitude, longitude,
              satisfaction_score, customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
    select(-matches('zip_code')) %>% 
    mutate(across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x)))

polinomios <- tc_std %>% 
    select(where(is.numeric)) %>%
    select(-qtd_servicos_principais) %>% 
    colnames() %>% 
    as_tibble() %>% 
    rename(var = 1) %>% 
    mutate(termo = glue("poly({var}, 3)"))


## Amostragem
set.seed(1)
train_idx <- createDataPartition(tc_std$flg_churn, p = .7, list = F)
tc_train <- tc_std[train_idx,]
tc_test <- tc_std[-train_idx,]

dim(tc_train)
formula_parte_1 <- tc_train %>% 
    select(-c(flg_churn, county)) %>%
    colnames() %>% 
    as_tibble() %>% 
    rename(var = 1) %>% 
    mutate(var = ifelse(var %in% c('age',
                                   'number_of_dependents',
                                   'number_of_referrals',
                                   'tenure_in_months',
                                   'qtd_servicos_adicionais',
                                   'qtd_streamings',
                                   'cltv'), glue("poly({var}, 3)"), var))

formula_parte_1 <- formula_parte_1$var %>% paste(collapse = ' + ')
formula_parte_1 <- glue("( {formula_parte_1} ) ^ 2")
formula_final <- glue("flg_churn ~ county + {formula_parte_1}")

## Treinamento
modelo_vazio <- glm(flg_churn ~ 1,
                    data = tc_train,
                    family = binomial)

modelo_tudo <- glm(as.formula(formula_final),
                   data = tc_train,
                   family = binomial)

modelo_step <- MASS::stepAIC(modelo_vazio,
                             direction = 'both',
                             scope = list(lower = modelo_vazio, upper = modelo_tudo),
                             trace = 1)

summary(modelo_step)


glmm_vazio <- glmmTMB(flg_churn ~ 1 + (1 | county),
                      data = tc_train,
                      family = binomial,
                      REML = T)

formula_glmm <- glue("flg_churn ~ {formula_parte_1} + (1 | county)")


glmm_step <- buildglmmTMB(as.formula(formula_str),
                          data = tc_train,
                          family = binomial,
                          buildmerControl = buildmerControl(crit = 'AIC', REML = T))
