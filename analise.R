library(sf)
library(tigris)
library(tidyverse)
library(stringr)
library(readxl)
library(ggpubr)
library(ggrepel)
library(caret)
# library(fastDummies)
library(glmmTMB)
library(lmtest)
library(tidycensus)
library(zipcodeR)
# census_api_key("63f47d6abc910026caf797fa324119d8a1b26f2d", install = T)
options(tigris_use_cache = T)

# fonte da base com as associacoes entre cep e condado: https://worldpopulationreview.com/zips

# a variavel revenue nao sera utilizada, por nao aparecer na documentacao da ibm

read_xlsx_custom <- function(file_name) {
    dados <- read_xlsx(file_name) %>% 
        rename_with(~ str_to_lower(str_replace_all(.x, ' ', '_')))
    
    return(dados)
}

descricao <- read_delim('csvs/descricao_variaveis.csv', '|')


# American Community Survey
about_variaveis_censo_acs <- load_variables(year = 2017, dataset = 'acs5', cache = T) %>%
    mutate(across(all_of(c('label', 'concept')), str_to_lower),
           label = str_replace_all(label, '!!', ', ')) %>%
    filter(!str_detect(concept, 'american indian|asian|black or african american|hispanic or latino|native hawaiian and other pacific islander|other race|two or more races|white alone'))

# write_delim(about_variaveis_censo_acs, 'csvs/about_variaveis_censo_acs.csv', '|')

variaveis_censo <- get_acs(geography = "county",
                           variables = c('B01001_001', 'B01001_002', 'B01002_001', 
                                         'B09001_001', 'B19083_001', 'B19113_001'),
                           state = "CA",
                           year = 2017,
                           geometry = F) %>% 
    left_join(about_variaveis_censo_acs, by = c('variable' = 'name')) %>% 
    select(NAME, variable, estimate) %>% 
    mutate(variable = case_when(variable == 'B01001_001' ~ 'condado_qtd_populacao',
                                variable == 'B01001_002' ~ 'condado_qtd_populacao_masculina',
                                variable == 'B01002_001' ~ 'condado_idade_mediana_populacao',
                                variable == 'B09001_001' ~ 'condado_qtd_populacao_menor_18_anos',
                                variable == 'B19083_001' ~ 'condado_indice_gini_desigualdade_renda',
                                variable == 'B19113_001' ~ 'condado_renda_familiar_mediana')) %>% 
    spread(variable, estimate) %>% 
    mutate(NAME = str_remove(NAME, ', California'),
           tx_populacao_masculina = condado_qtd_populacao_masculina / condado_qtd_populacao,
           tx_populacao_menor_18_anos = condado_qtd_populacao_menor_18_anos / condado_qtd_populacao) %>% 
    select(-c(condado_qtd_populacao_masculina, condado_qtd_populacao_menor_18_anos))


tc_demographics <- read_xlsx_custom('csvs/Telco_customer_churn_demographics.xlsx') %>% 
    select(customer_id, gender, age, under_30, senior_citizen, married, dependents, number_of_dependents)

tc_location <- read_xlsx_custom('csvs/Telco_customer_churn_location.xlsx') %>% 
    select(customer_id, city, zip_code, latitude, longitude) %>% 
    mutate(zip_code = as.character(zip_code))

tc_population <- read_xlsx_custom('csvs/Telco_customer_churn_population.xlsx') %>% 
    select(zip_code, population) %>% 
    mutate(zip_code = as.character(zip_code))

tc_services <- read_xlsx_custom('csvs/Telco_customer_churn_services.xlsx') %>% 
    select(customer_id, referred_a_friend, number_of_referrals, tenure_in_months, offer, 
           phone_service, avg_monthly_long_distance_charges, multiple_lines, internet_service, 
           internet_type, avg_monthly_gb_download, online_security, online_backup, device_protection_plan, 
           premium_tech_support, streaming_tv, streaming_movies, streaming_music, unlimited_data,
           contract, paperless_billing, payment_method, monthly_charge, total_charges, total_refunds, 
           total_extra_data_charges, total_long_distance_charges)


tc_status <- read_xlsx_custom('csvs/Telco_customer_churn_status.xlsx') %>% 
    select(customer_id, satisfaction_score, customer_status, churn_value, cltv, churn_category, churn_reason)


zip_county <- zipcodeR::zip_code_db %>%
    select(zipcode, county)

tc <- tc_demographics %>% 
    left_join(tc_location, by = 'customer_id') %>% 
    left_join(tc_population, by = 'zip_code') %>% 
    left_join(tc_services, by = 'customer_id') %>% 
    left_join(tc_status, by = 'customer_id') %>% 
    left_join(zip_county, by = c('zip_code' = 'zipcode')) %>% 
    left_join(variaveis_censo, by = c('county' = 'NAME')) %>% 
    rename(flg_churn = churn_value) %>% 
    mutate(across(all_of(c('device_protection_plan', 'internet_service', 'online_backup',
                           'online_security', 'phone_service', 'premium_tech_support',
                           'referred_a_friend', 'multiple_lines',
                           'streaming_tv', 'streaming_movies', 'streaming_music',
                           'unlimited_data', 'paperless_billing', 'under_30',
                           'senior_citizen', 'married', 'dependents')), ~ ifelse(.x == 'Yes', 1, 0))) %>% 
    mutate(valor_cobranca_geral = total_charges + total_long_distance_charges + total_extra_data_charges,
           tx_valores_reembolsados = total_refunds / valor_cobranca_geral,
           tx_concentracao_cobranca_mes_q3 = monthly_charge / total_charges, # possivel indicador da quantidade de meses que o cliente esta com a companhia, e se o valor da mensalidade atual eh superior ao das mensalidades anteriores
           tx_contrib_cobrancas_extras_cobranca_geral = (total_long_distance_charges + total_extra_data_charges) / valor_cobranca_geral,
           total_gb_downloaded = tenure_in_months * avg_monthly_gb_download,
           qtd_mensal_media_indicacoes  = number_of_referrals / tenure_in_months,
           valor_mensal_medio_cobrancas_extras = (total_long_distance_charges + total_extra_data_charges) / tenure_in_months,
           valor_mensal_medio_cobrancas_totais = total_charges / tenure_in_months,
           valor_mensal_medio_cobranca_geral = valor_cobranca_geral / tenure_in_months,
           relacao_valor_cobranca_atual_valor_medio_mensal = monthly_charge / valor_mensal_medio_cobrancas_totais, # possivel forma de identificar que o cliente teve aumento na fatura
           qtd_servicos = internet_service + phone_service,
           qtd_servicos_adicionais = unlimited_data + device_protection_plan + online_backup + online_security + premium_tech_support,
           qtd_streamings_utilizados = streaming_movies + streaming_music + streaming_tv,
           tem_servicos_adicionais = ifelse(qtd_servicos_adicionais > 0, '1', '0'),
           utiliza_streaming = ifelse(qtd_streamings_utilizados > 0, '1', '0'),
           tx_contrib_cobrancas_cliente_base_total_1 = total_charges / sum(total_charges),
           tx_contrib_cobrancas_cliente_base_total_2 = valor_cobranca_geral / sum(valor_cobranca_geral),
           across(all_of(c('device_protection_plan', 'internet_service', 'online_backup',
                           'online_security', 'phone_service', 'premium_tech_support',
                           'referred_a_friend', 'multiple_lines',
                           'streaming_tv', 'streaming_movies', 'streaming_music',
                           'unlimited_data', 'paperless_billing', 'under_30',
                           'senior_citizen', 'married', 'dependents')), as.factor),
           zip_code = as.factor(zip_code),
           across(where(is.character), as.factor))

# densidade_populacional

glimpse(tc)

select(tc, where(is.numeric)) %>% glimpse()

# Caracteristicas estranhas observadas nos dados:
#   filter(tc, total_long_distance_charges > total_charges) %>% select(total_long_distance_charges, total_charges) %>% View()
#   filter(tc, total_extra_data_charges > total_charges) %>% select(total_extra_data_charges, total_charges) %>% View()
# Por isso, entende-se que a variavel 'total_charges', nao compreende as cobrancas extras

# interacoes para teste: tenure_in_months * cltv

# Servicos extras, de internet:
#   unlimited_data
#   device_protection_plan

tc %>% select(matches('tx_')) %>% summary()

rm(tc_demographics)
rm(tc_location)
rm(tc_population)
rm(tc_services)
rm(tc_status)

dados_geo <- tc %>% 
    select(customer_id, churn_category, flg_churn, zip_code, latitude, longitude, county)

churn_zip_code <- dados_geo %>% 
    group_by(zip_code) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes,
           taxa_contrib_qtd_churn = qtd_clientes_churn / sum(qtd_clientes_churn, na.rm = T)) %>%
    arrange(desc(taxa_churn))

churn_county <- dados_geo %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes,
           taxa_contrib_qtd_churn = qtd_clientes_churn / sum(qtd_clientes_churn, na.rm = T)) %>%
    arrange(desc(taxa_churn))

motivo_churn <- dados_geo %>% 
    filter(flg_churn == 1) %>% 
    group_by(county, churn_category) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(taxa_clientes = qtd_clientes / sum(qtd_clientes)) %>% 
    ungroup()

tp_motivo_churn <- motivo_churn %>% 
    group_by(county) %>% 
    filter(qtd_clientes == max(qtd_clientes, na.rm = T)) %>% 
    summarise(taxa_clientes_categoria = sum(taxa_clientes, na.rm = T),
              churn_category = paste(churn_category, collapse = ' & '))

churn_county <- churn_county %>% 
    left_join(tp_motivo_churn, by = 'county')

churn_long_lat <- dados_geo %>% 
    group_by(longitude, latitude) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn)) %>% 
    ungroup() %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes,
           taxa_contrib_qtd_churn = qtd_clientes_churn / sum(qtd_clientes_churn, na.rm = T)) %>%
    arrange(desc(taxa_churn))

sf_zip_codes <- tigris::zctas(state = 'CA', year = 2010) %>% 
    left_join(churn_zip_code, by = c('ZCTA5CE10' = 'zip_code'))

sf_counties <- tigris::counties(state = 'CA', year = 2010) %>%
    left_join(churn_county, by = c('NAMELSAD10' = 'county'))

sf_long_lat <- churn_long_lat %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# p1 <- ggplot() +
#     geom_sf(data = sf_counties, fill = NA, size = 0.3) +
#     geom_sf(data = sf_zip_codes, mapping = aes(fill = taxa_churn), size = 0.3) +
#     scale_fill_viridis_c(na.value = 'white', labels = scales::percent_format(accuracy = 1)) +
#     theme_light() +
#     labs(x = NULL, y = NULL, fill = '',
#          title = '% Churn')
# 
# p2 <- ggplot() +
#     geom_sf(data = sf_counties, fill = NA, size = 0.3) +
#     geom_sf(data = sf_zip_codes, mapping = aes(fill = taxa_contrib_qtd_churn), size = 0.3) +
#     scale_fill_viridis_c(na.value = 'white', labels = scales::percent_format(accuracy = 1)) +
#     theme_light() +
#     theme(axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()) +
#     labs(x = NULL, y = NULL, fill = '',
#          title = 'Concentração do churn')
# 
# ggarrange(p1, p2, common.legend = F, legend = 'right', align = 'hv')
# 
# ggsave("plots/taxa_churn_zip_code.png", width = 9, height = 5)

p1 <- ggplot() +
    geom_sf(data = sf_counties, fill = NA, size = 0.3) +
    geom_sf(data = sf_long_lat, mapping = aes(color = taxa_churn, size = taxa_churn, alpha = taxa_churn)) +
    scale_size(guide = 'none') +
    scale_alpha(guide = 'none') +
    scale_color_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, color = NULL,
         title = '% Churn')

p2 <- ggplot() +
    geom_sf(data = sf_counties, fill = NA, size = 0.3) +
    geom_sf(data = sf_long_lat, mapping = aes(color = taxa_contrib_qtd_churn, size = taxa_contrib_qtd_churn, alpha = taxa_contrib_qtd_churn)) +
    scale_size(guide = 'none') +
    scale_alpha(guide = 'none') +
    scale_color_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL, color = NULL,
         title = 'Concentração do churn')


ggarrange(p1, p2, common.legend = F, legend = 'right', align = 'hv')

ggsave("plots/taxa_churn.png", width = 9, height = 5)


p3 <- ggplot() +
    geom_sf(data = sf_counties, mapping = aes(fill = taxa_churn), size = 0.3) +
    geom_label_repel(
        data = head(arrange(sf_counties, desc(taxa_churn)), 5),
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = '',
         title = '% Churn')


p4 <- ggplot() +
    geom_sf(data = sf_counties, mapping = aes(fill = taxa_contrib_qtd_churn), size = 0.3) +
    geom_label_repel(
        data = head(arrange(sf_counties, desc(taxa_contrib_qtd_churn)), 5),
        aes(label = NAME10, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL, fill = '',
         title = 'Concentração do churn')

ggarrange(p3, p4, common.legend = F, legend = 'bottom', align = 'hv')

ggsave("plots/churn_por_condado.png", width = 9, height = 5)


ggplot() +
    geom_sf(data = sf_counties, mapping = aes(fill = churn_category), size = 0.3) +
    scale_fill_brewer(palette = 'Set3') +
    theme_light() +
    labs(x = NULL, y = NULL, fill = '',
         title = 'Principais motivos de churn')

ggsave("plots/principais_motivos_churn.png", width = 9, height = 5)

# Condados com maiores taxas de churn
top_motivo_churn_1 <- motivo_churn %>% 
    filter(county %in% c('Del Norte', 'Colusa', 'Napa', 'Stanislaus', 'San Diego')) %>% 
    group_by(churn_category) %>% 
    summarise(qtd_clientes = sum(qtd_clientes)) %>% 
    mutate(taxa_clientes = qtd_clientes / sum(qtd_clientes)) %>% 
    arrange(desc(qtd_clientes))


# Condados com maior contribuicao no churn total
top_motivo_churn_2 <- motivo_churn %>% 
           filter(county %in% c('Los Angeles', 'San Bernardino', 'Riverside', 'Orange', 'San Diego')) %>% 
    group_by(churn_category) %>% 
    summarise(qtd_clientes = sum(qtd_clientes)) %>% 
    mutate(taxa_clientes = qtd_clientes / sum(qtd_clientes)) %>% 
    arrange(desc(qtd_clientes))

tc <- tc %>% mutate(flg_churn = as.factor(flg_churn))

# Distribuicao das variaveis
to_plot <- tc %>% 
    select(flg_churn, where(is.numeric)) %>% 
    select(-c(latitude, longitude)) %>% 
    gather(var, value, -flg_churn)

ggplot(to_plot %>% 
           mutate(flg_churn = ifelse(flg_churn == '1', 'Sim', 'Não')),
       aes(flg_churn, value, fill = flg_churn)) +
    geom_boxplot(outlier.size = -1, color = 'black', alpha = .7) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    # scale_fill_viridis_d() +
    facet_wrap(~ var, scales = 'free') +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(x = NULL, y = NULL, fill = 'Churn ?')


# ggplot(to_plot %>% 
#            mutate(flg_churn = ifelse(flg_churn == '1', 1, 0)),
#        aes(value, flg_churn)) +
#     geom_point(alpha = .05) +
#     facet_wrap(~ var, scales = 'free') +
#     geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = T) +
#     theme_light() +
#     labs(x = NULL, y = NULL)


ggplot(tc, aes(tx_contrib_cobrancas_extras_cobranca_geral, 
               ifelse(flg_churn == '1', 1, 0))) +
    geom_point(alpha = .05) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = F) +
    theme_light()


ggplot(tc %>% 
           select(!where(is.numeric)) %>% 
           select(-c(customer_id, city, zip_code, county, churn_reason, churn_category, customer_status)) %>%
           mutate(across(everything(), as.character)) %>% 
           gather(var, value, -flg_churn) %>% 
           mutate(across(all_of(c('flg_churn', 'value')), ~ case_when(.x == '1' ~ 'Sim',
                                                                      .x == '0' ~ 'Não',
                                                                      T ~ .x))),
       aes(value, color = flg_churn, fill = flg_churn)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c('#d5dce0', '#ffc000')) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    facet_wrap(~ var, scales = 'free_y') +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = NULL, y = NULL, color = 'Churn ?', fill = 'Churn ?') +
    coord_flip()


###
to_plot_2 <- tc %>% 
    select(!where(is.numeric), -customer_id) %>% 
    select(flg_churn, where(~ nlevels(.x) == 2)) %>%
    select(-gender) %>% 
    mutate(across(everything(), as.character)) %>% 
    gather(var, value, -flg_churn) %>% 
    mutate(across(all_of(c('flg_churn', 'value')), ~ case_when(.x == '1' ~ 'Sim',
                                                               .x == '0' ~ 'Não',
                                                               T ~ .x)))

ggplot(to_plot_2,
       aes(var, color = value, fill = value)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_light() +
    labs(x = NULL, y = NULL, color = '', fill = '',
         title = 'Share na companhia, por características categóricas de seus clientes') +
    coord_flip()


ggplot(to_plot_2 %>% 
           mutate(flg_churn = ifelse(flg_churn == 'Sim', 'Cancelaram (churn)', 'Não cancelaram')),
       aes(var, color = value, fill = value)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    # scale_color_brewer(palette = 'Set3') +
    # scale_fill_brewer(palette = 'Set3') +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_light() +
    labs(x = NULL, y = NULL, color = '', fill = '',
         title = 'Share na companhia, por características categóricas de seus clientes',
         subtitle = 'Segmentado por clientes que cancelaram ou não os serviços') +
    coord_flip() +
    facet_wrap(~ flg_churn)


# variaveis categoricas
to_plot_3 <- tc %>% 
    select(flg_churn, gender, offer, contract) %>% 
    mutate(across(everything(), as.character)) %>% 
    gather(var, value, -flg_churn) %>% 
    mutate(across(all_of(c('flg_churn', 'value')), ~ case_when(.x == '1' ~ 'Sim',
                                                               .x == '0' ~ 'Não',
                                                               T ~ .x)))

ggplot(to_plot_3,
       aes(var, color = value, fill = value)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_brewer(palette = 'Set3') +
    scale_fill_brewer(palette = 'Set3') +
    theme_light() +
    labs(x = NULL, y = NULL, color = '', fill = '',
         title = 'Share na companhia, por características categóricas de seus clientes') +
    coord_flip()


ggplot(to_plot_3 %>% 
           mutate(flg_churn = ifelse(flg_churn == 'Sim', 'Cancelaram (churn)', 'Não cancelaram')),
       aes(var, color = value, fill = value)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_brewer(palette = 'Set3') +
    scale_fill_brewer(palette = 'Set3') +
    theme_light() +
    labs(x = NULL, y = NULL, color = '', fill = '',
         title = 'Share na companhia, por características categóricas de seus clientes',
         subtitle = 'Segmentado por clientes que cancelaram ou não os serviços') +
    coord_flip() +
    facet_wrap(~ flg_churn)

##
to_plot_4 <- tc %>% 
    select(flg_churn, payment_method, internet_type) %>% 
    mutate(across(everything(), as.character)) %>% 
    gather(var, value, -flg_churn) %>% 
    mutate(across(all_of(c('flg_churn', 'value')), ~ case_when(.x == '1' ~ 'Sim',
                                                               .x == '0' ~ 'Não',
                                                               T ~ .x)))

ggplot(to_plot_4,
       aes(var, color = value, fill = value)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_brewer(palette = 'Set3') +
    scale_fill_brewer(palette = 'Set3') +
    theme_light() +
    labs(x = NULL, y = NULL, color = '', fill = '',
         title = 'Share na companhia, por características categóricas de seus clientes') +
    coord_flip()


ggplot(to_plot_4 %>% 
           mutate(flg_churn = ifelse(flg_churn == 'Sim', 'Cancelaram (churn)', 'Não cancelaram')),
       aes(var, color = value, fill = value)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_brewer(palette = 'Set3') +
    scale_fill_brewer(palette = 'Set3') +
    theme_light() +
    labs(x = NULL, y = NULL, color = '', fill = '',
         title = 'Share na companhia, por características categóricas de seus clientes',
         subtitle = 'Segmentado por clientes que cancelaram ou não os serviços') +
    coord_flip() +
    facet_wrap(~ flg_churn)


# county
tc_std <- tc %>% 
    select(-c(customer_id, city, zip_code, population, latitude, longitude, churn_category, churn_reason, satisfaction_score, customer_status)) %>% 
    mutate(across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x)))

tc_std %>% select(where(is.numeric)) %>% glimpse()

# Amostragem
set.seed(1)
train_idx <- createDataPartition(tc_std$flg_churn, p = .7, list = F)
tc_train <- tc_std[train_idx,]
tc_test <- tc_std[-train_idx,]
# rm(tc)

dim(tc_train)
glimpse(tc_train)

tc_train$flg_churn %>% table()
tc_train$flg_churn %>% table() %>% prop.table()
tc_test$flg_churn %>% table() %>% prop.table()

# formula_str <- paste('flg_churn ~ county*pop + (', tc_train %>% select(-c(county, pop)) %>% colnames() %>% paste(collapse = ' + '), ') ^ 2')

## Treinamento
modelo_vazio <- glm(flg_churn ~ 1,
                    data = tc_train,
                    family = binomial)

modelo_tudo <- glm(flg_churn ~ . + I(age^2) + I(tenure_in_months^2) + I(number_of_referrals^2)
                    + I(qtd_servicos^2) + I(qtd_servicos_adicionais^2)
                    + I(qtd_streamings_utilizados^2),
                   data = tc_train,
                   family = binomial)

summary(modelo_tudo)

modelo_step <- MASS::stepAIC(modelo_vazio,
                             direction = 'both',
                             scope = list(lower = modelo_vazio, upper = modelo_tudo),
                             trace = 1)

summary(modelo_step)
saveRDS(modelo_step, 'r_objects/modelo_step.rds')

tmp <- modelo_step$coefficients %>% 
    as.data.frame() %>%
    rownames_to_column()

tmp[-1,1] %>% paste(collapse = ' + ')

glm_vazio <- glmmTMB(flg_churn ~ 1 + (1 | county),
                     data = tc_train,
                     family = binomial,
                     REML = T)

glm_outro <- glmmTMB(flg_churn ~ contract + internet_type + number_of_referrals
                        + referred_a_friend + tx_concentracao_cobranca_mes_q3 + dependents
                        + payment_method + senior_citizen + tenure_in_months +
                        + paperless_billing + valor_mensal_medio_cobrancas_totais + phone_service
                        + qtd_servicos_adicionais + tem_servicos_adicionais + pop + age
                        + streaming_movies + streaming_music
                        + relacao_valor_cobranca_atual_valor_medio_mensal
                        + unlimited_data + device_protection_plan
                        + (1 | county),
                     data = tc_train,
                     family = binomial,
                     REML = T)

AIC(glm_vazio)
AIC(glm_outro)

lrtest(modelo_step, glm_outro)
