library(tigris) #ok
library(tidyverse) # ok
library(stringr) #ok
library(glue) #ok
library(readxl) #ok
# library(ggpubr) #ok
library(ggrepel) #ok
library(caret) # ok
library(glmmTMB) #ok
library(lmtest) #ok
library(tidycensus) #ok
library(zipcodeR) #ok
# library(StepReg)
library(buildmer) #ok
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
           flg_churn_numeric = ifelse(flg_churn == '1', 1, 0)) %>% 
    select(-c(flg_internet_service))

colnames(tc) %>% paste(collapse = '\n') %>% cat()

tc %>% select(matches('tx_')) %>% summary()

rm(list = c('tc_demographics', 'tc_location', 'tc_population', 'tc_services', 'tc_status'))


tc$flg_churn %>% table() %>% prop.table()
tc$customer_status %>% table() %>% prop.table()
tc$churn_category %>% table() %>% prop.table()
tc$churn_reason %>% table() %>% prop.table()

tc %>% 
    group_by(satisfaction_score, flg_churn) %>% 
    summarise(qtd_clientes = n()) %>% 
    summarise(taxa_churn = sum(ifelse(flg_churn == 1, qtd_clientes, 0)) / sum(qtd_clientes),
              qtd_clientes = sum(qtd_clientes)) %>% 
    select(satisfaction_score, qtd_clientes, taxa_churn) %>% 
    mutate(taxa_churn = round(100 * taxa_churn, 1)) %>% 
    write_csv('csvs/taxa_churn_por_satisfaction_score.csv')

quantile(filter(tc, flg_churn_numeric == 1)$number_of_referrals,
         seq(0, 1, .05))

quantile(filter(tc, flg_churn_numeric == 1)$satisfaction_score,
         seq(0, 1, .1))

quantile(filter(tc, flg_churn_numeric == 1)$tenure_in_months,
         seq(0, 1, .1))

tc %>%
    group_by(flg_churn) %>% 
    summarise(across(all_of(c("tenure_in_months",
                              "number_of_referrals",
                              "cltv")),
              list(min = ~ min(.x, na.rm = T), max = ~ max(.x, na.rm = T),
                   mean = ~ mean(.x, na.rm = T), sd = ~ sd(.x, na.rm = T)),
              .names = "{.fn}_{.col}")) %>% 
    View()

## Graficos

# distribuicao de variaveis interessantes
ggplot(tc %>% 
           select(flg_churn_numeric, tenure_in_months, number_of_referrals) %>% 
           mutate(churn_descricao = ifelse(flg_churn_numeric == 1, 'Sim', 'Não')) %>% 
           gather(var, value, -c(flg_churn_numeric, churn_descricao)),
       aes(churn_descricao, value, fill = churn_descricao)) +
    geom_boxplot(color = 'black', alpha = 0.7) +
    scale_fill_viridis_d(option = 'D') +
    facet_wrap(~ var, scales = 'free_y', nrow = 1) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom') +
    labs(x = NULL, y = NULL, fill = 'Clientes perdidos ?')

ggsave("plots/box_plots_condado.png", width = 9, height = 5)



# qtd_clientes_condado <- tc %>% 
#     group_by(county) %>% 
#     summarise(qtd_clientes = n()) %>% 
#     mutate(faixa_qtd_clientes = cut(qtd_clientes,
#                                     quantile(qtd_clientes, probs = seq(0, 1, 0.2)),
#                                     include.lowest = T, right = F,
#                                     dig.lab = 5, ordered_result = T))
# 
# sf_condados_1 <- tigris::counties(state = 'CA', year = 2010) %>% 
#     left_join(qtd_clientes_condado, by = c('NAMELSAD10' = 'county'))
# 
# top_3_maiores <- slice_max(sf_condados_1, qtd_clientes, n = 3)
# top_3_menores <- slice_min(sf_condados_1, qtd_clientes, n = 3)
# top_6_condados <- top_3_maiores %>% 
#     rbind(top_3_menores)
# 
# g1_clientes <- ggplot() +
#     geom_sf(data = sf_condados_1, mapping = aes(fill = faixa_qtd_clientes), size = 0.3) +
#     geom_label_repel(
#         data = top_6_condados,
#         aes(label = NAME10, geometry = geometry),
#         size = 2,
#         stat = "sf_coordinates",
#         min.segment.length = 0) +
#     scale_fill_viridis_d() +
#     theme_light() +
#     labs(x = NULL, y = NULL, fill = NULL, title = 'Qtd clientes')

# Distribuicao dos clientes da companhia, por condado
tx_contrib_clientes_condado <- tc %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(tx_contrib_clientes = qtd_clientes / sum(qtd_clientes))

sf_condados_2 <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(tx_contrib_clientes_condado, by = c('NAMELSAD10' = 'county'))

top_3_maiores <- slice_max(sf_condados_2, tx_contrib_clientes, n = 3)
top_3_menores <- slice_min(sf_condados_2, tx_contrib_clientes, n = 3)
top_6_condados <- top_3_maiores %>% 
    rbind(top_3_menores) %>% 
    mutate(rounded_tx_contrib_clientes = round(100 * tx_contrib_clientes, 1),
           rounded_tx_contrib_clientes = str_replace(paste(rounded_tx_contrib_clientes), '[.]', ','),
           nome_com_percentual = glue("{NAME10}: {rounded_tx_contrib_clientes}%"))

sf_condados_2 <- sf_condados_2 %>% 
    mutate(tx_contrib_clientes = round(100 * tx_contrib_clientes, 1),
           faixas_tx_contrib_clientes = cut(tx_contrib_clientes,
                                            seq(0, ceiling(max(tx_contrib_clientes)), 1),
                                            include.lowest = T, right = F,
                                            dig.lab = 5, ordered_result = T))

g2_clientes <- ggplot() +
    geom_sf(data = sf_condados_2,
            mapping = aes(fill = faixas_tx_contrib_clientes), size = 0.3) +
    geom_label_repel(
        data = top_6_condados,
        aes(label = nome_com_percentual, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_d() +
    # scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL)

# g3_clientes <- ggarrange(g1_clientes, g2_clientes, common.legend = F, legend = 'right', align = 'hv')

ggsave("plots/distribuicao_clientes_por_condado.png", plot = g2_clientes, width = 9, height = 5)



proporcao_hab_clientes_condado <- variaveis_censo_condado %>% 
    left_join(tx_contrib_clientes_condado, by = c('NAME' = 'county')) %>% 
    mutate(proporcao_habitantes_clientes = qtd_clientes / condado_qtd_habitantes)

sf_condados_3 <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(proporcao_hab_clientes_condado, by = c('NAMELSAD10' = 'NAME'))

top_3_maiores <- slice_max(sf_condados_3, proporcao_habitantes_clientes, n = 3)
top_3_menores <- slice_min(sf_condados_3, proporcao_habitantes_clientes, n = 3)
top_6_condados <- top_3_maiores %>% 
    rbind(top_3_menores) %>% 
    mutate(rounded_proporcao_habitantes_clientes = round(100 * proporcao_habitantes_clientes, 2),
           # rounded_proporcao_habitantes_clientes = str_replace(paste(rounded_proporcao_habitantes_clientes), '[.]', ','),
           nome_com_percentual = glue("{NAME10}: {rounded_proporcao_habitantes_clientes}%"))

sf_condados_3 <- sf_condados_3 %>% 
    mutate(proporcao_habitantes_clientes = round(100 * proporcao_habitantes_clientes, 2),
           faixas_proporcao_habitantes_clientes = cut(proporcao_habitantes_clientes,
                                                      seq(0, 1, 0.1),
                                                      include.lowest = T, right = F,
                                                      dig.lab = 5, ordered_result = T))

tmp <- sf_condados_3 %>% 
    sf::st_drop_geometry() %>% 
    filter(NAME10 %in% c("Sierra", "Alpine", "Trinity"))


ggplot() +
    geom_sf(data = sf_condados_3,
            mapping = aes(fill = faixas_proporcao_habitantes_clientes), size = 0.3) +
    geom_label_repel(
        data = top_6_condados,
        aes(label = nome_com_percentual, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_d() +
    # scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL)

#title = 'Prop. habitantes que foram ou são clientes'
ggsave("plots/prop_hab_clientes_condado.png", width = 9, height = 5)

prep_motivo_churn <- tc %>% 
    filter(flg_churn_numeric == 1) %>% 
    group_by(churn_category, churn_reason) %>% 
    summarise(qtd_clientes = n()) %>% 
    ungroup() %>% 
    mutate(churn_category = case_when(churn_category == 'Attitude' ~ 'Atitude',
                                      churn_category == 'Competitor' ~ 'Concorrente',
                                      churn_category == 'Dissatisfaction' ~ 'Insatisfação',
                                      churn_category == 'Other' ~ 'Outro',
                                      churn_category == 'Price' ~ 'Preço',
                                      T ~ 'Não identificado'),
           churn_reason = case_when(churn_reason == "Attitude of service provider" ~ "Atitude do prestador de serviço",
                                    churn_reason == "Attitude of support person" ~ "Atitude de pessoa do suporte",
                                    churn_reason == "Competitor had better devices" ~ "Concorrente tinha melhores dispositivos",
                                    churn_reason == "Competitor made better offer" ~ "Concorrente fez oferta melhor",
                                    churn_reason == "Competitor offered higher download speeds" ~ "Concorrente ofereceu maiores velocidades de download",
                                    churn_reason == "Competitor offered more data" ~ "Concorrente ofereceu mais dados",
                                    churn_reason == "Lack of self-service on Website" ~ "Falta de autoatendimento no website",
                                    churn_reason == "Limited range of services" ~ "Conjunto limitado de serviços",
                                    churn_reason == "Network reliability" ~ "Confiabilidade da rede",
                                    churn_reason == "Poor expertise of online support" ~ "Baixa competência do suporte online",
                                    churn_reason == "Poor expertise of phone support" ~ "Baixa competência do suporte telefônico",
                                    churn_reason == "Product dissatisfaction" ~ "Insatisfação com o produto",
                                    churn_reason == "Service dissatisfaction" ~ "Insatisfação com o serviço",
                                    churn_reason == "Deceased" ~ "Falecido",
                                    churn_reason == "Don't know" ~ "Não identificado",
                                    churn_reason == "Moved" ~ "Mudou-se",
                                    churn_reason == "Extra data charges" ~ "Cobranças adicionais por dados",
                                    churn_reason == "Lack of affordable download/upload speed" ~ "Ausência de velocidade de download/upload econômica",
                                    churn_reason == "Long distance charges" ~ "Cobranças por chamadas de longa distância",
                                    churn_reason == "Price too high" ~ "Preço muito elevado")) %>% # Traducao pra portugues
    mutate(tx_contrib_clientes_reason = qtd_clientes / sum(qtd_clientes),
           tx_contrib_clientes_reason = round(tx_contrib_clientes_reason * 100, 1)) %>% 
    group_by(churn_category) %>% 
    mutate(qtd_clientes_category = sum(qtd_clientes)) %>% 
    ungroup() %>% 
    mutate(tx_contrib_category = qtd_clientes_category / sum(qtd_clientes),
           tx_contrib_category = round(tx_contrib_category * 100, 1),
           churn_category = paste(churn_category, tx_contrib_category, sep = '; '))

g1_motivo_churn <- ggplot(prep_motivo_churn,
       aes(reorder(churn_reason, tx_contrib_clientes_reason), tx_contrib_clientes_reason,
           fill = churn_category, label = tx_contrib_clientes_reason)) +
    geom_col(position = 'dodge') +
    geom_text(hjust = -0.15, size = 2.5, position = position_dodge(width = 1)) +
    scale_fill_viridis_d() +
    labs(x = NULL, y = NULL, fill = NULL) +
    coord_flip() +
    theme_light() +
    labs(x = 'Motivo específico',
         y = 'Percentual de concentração do churn',
         fill = 'Motivo geral')
    # labs(title = 'Distribuição do motivo de churn')

ggsave("plots/distribuicao_motivo_churn.png", plot = g1_motivo_churn, width = 12, height = 7)


txs_churn_condado <- tc %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn_numeric)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes,
           taxa_contrib_qtd_churn = qtd_clientes_churn / sum(qtd_clientes_churn, na.rm = T)) %>%
    arrange(desc(taxa_churn))

sf_condados <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(txs_churn_condado, by = c('NAMELSAD10' = 'county'))

top_6_condados_taxa_churn <- slice_max(sf_condados, taxa_churn, n = 3) %>% 
    rbind(slice_min(sf_condados, taxa_churn, n = 3)) %>% 
    mutate(nome_com_percentual = glue("{NAME10}: {round(100 * taxa_churn, 1)}%"))


top_6_condados_taxa_contrib_qtd_churn <- slice_max(sf_condados, taxa_contrib_qtd_churn, n = 3) %>% 
    rbind(slice_min(sf_condados, taxa_contrib_qtd_churn, n = 3)) %>% 
    mutate(nome_com_percentual = glue("{NAME10}: {round(100 * taxa_contrib_qtd_churn, 1)}%"))

sf_condados <- sf_condados %>% 
    mutate(taxa_churn = round(100 * taxa_churn, 1),
           faixas_taxa_churn = cut(taxa_churn,
                                   seq(0, ceiling(max(taxa_churn)), 10),
                                   include.lowest = T, right = F,
                                   dig.lab = 5, ordered_result = T),
           taxa_contrib_qtd_churn = round(100 * taxa_contrib_qtd_churn, 1),
           faixas_taxa_contrib_qtd_churn = cut(taxa_contrib_qtd_churn,
                                               seq(0, 20, 2.5),
                                               include.lowest = T, right = F,
                                               dig.lab = 5, ordered_result = T))

g1_condado <- ggplot() +
    geom_sf(data = sf_condados, mapping = aes(fill = faixas_taxa_churn), size = 0.3) +
    geom_label_repel(
        data = top_6_condados_taxa_churn,
        aes(label = nome_com_percentual, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_d() +
    # scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL)
# title = '% Churn'

ggsave("plots/taxa_churn_por_condado.png", plot = g1_condado, width = 9, height = 5)

g2_condado <- ggplot() +
    geom_sf(data = sf_condados, mapping = aes(fill = faixas_taxa_contrib_qtd_churn), size = 0.3) +
    geom_label_repel(
        data = top_6_condados_taxa_contrib_qtd_churn,
        aes(label = nome_com_percentual, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_d() +
    theme_light() +
    # theme(axis.title.y = element_blank(),
    #       axis.text.y = element_blank(),
    #       axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL, fill = NULL)
# title = 'Concentração do churn'

# g3_condado <- ggarrange(g1_condado, g2_condado, common.legend = F, legend = 'right', align = 'hv')

ggsave("plots/distribuicao_churn_por_condado.png", plot = g2_condado, width = 9, height = 5)


motivo_churn <- tc %>%
    mutate(churn_category = case_when(churn_category == 'Attitude' ~ 'Atitude',
                                      churn_category == 'Competitor' ~ 'Concorrente',
                                      churn_category == 'Dissatisfaction' ~ 'Insatisfação',
                                      churn_category == 'Other' ~ 'Outro',
                                      churn_category == 'Price' ~ 'Preço',
                                      T ~ 'Não identificado')) %>% 
    filter(flg_churn_numeric == 1) %>%
    group_by(county, churn_category) %>%
    summarise(qtd_clientes = n()) %>%
    mutate(tx_clientes = qtd_clientes / sum(qtd_clientes))

ggplot(motivo_churn, aes(county, tx_clientes, fill = churn_category)) +
    geom_col() +
    scale_fill_viridis_d() +
    coord_flip()

principais_motivos_churn <- motivo_churn %>% 
    filter(qtd_clientes == max(qtd_clientes, na.rm = T)) %>%
    summarise(tx_clientes_motivo = sum(tx_clientes, na.rm = T),
              churn_category = paste(churn_category, collapse = ' & ')) %>% 
    mutate(tx_clientes_motivo = round(100 * tx_clientes_motivo, 1),
           principais_motivos_com_percentual = glue('{churn_category}: {tx_clientes_motivo}'))


write_csv(principais_motivos_churn %>% 
              mutate(county = str_remove(county, ' County')) %>% 
              select(county, churn_category, tx_clientes_motivo) %>% 
              arrange(churn_category), 'csvs/tabela_principais_motivos_churn.csv')


sf_condados <- tigris::counties(state = 'CA', year = 2010) %>% 
    left_join(principais_motivos_churn, by = c('NAMELSAD10' = 'county'))

# tmp <- sf_condados %>%
#     sf::st_drop_geometry()


ggplot() +
    geom_sf(data = sf_condados, mapping = aes(fill = churn_category), size = 0.3) +
    # geom_label_repel(
    #     data = sf_condados,
    #     aes(label = tx_clientes_motivo, geometry = geometry),
    #     size = 2,
    #     stat = "sf_coordinates",
    #     min.segment.length = 0) +
    scale_fill_brewer(palette = 'Set3') +
    # scale_fill_viridis_d(option = 'D') +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL)
# title = 'Principais motivos de churn'

ggsave('plots/principais_motivos_churn_por_condado.png', width = 9, height = 5)

# Modelagem

## Padronizacao das variaveis numericas
glimpse(tc)

tc_std <- tc %>% 
    select(-c(customer_id, 
              city, latitude, longitude,
              # satisfaction_score, 
              customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
    select(-matches('zip_code')) %>% 
    mutate(across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x)))

# polinomios <- tc_std %>% 
#     select(where(is.numeric)) %>%
#     select(-qtd_servicos_principais) %>% 
#     colnames() %>% 
#     as_tibble() %>% 
#     rename(var = 1) %>% 
#     mutate(termo = glue("poly({var}, 3)"))


## Amostragem
set.seed(1)
train_idx <- createDataPartition(tc_std$flg_churn, p = .7, list = F)
tc_train <- tc_std[train_idx,]
tc_test <- tc_std[-train_idx,]

dim(tc_train)
# formula_parte_1 <- tc_train %>% 
#     select(-c(flg_churn, county)) %>%
#     colnames() %>% 
#     as_tibble() %>% 
#     rename(var = 1) %>% 
#     mutate(var = ifelse(var %in% c('age',
#                                    'number_of_dependents',
#                                    'number_of_referrals',
#                                    'tenure_in_months',
#                                    'qtd_servicos_adicionais',
#                                    'qtd_streamings',
#                                    'cltv'), glue("poly({var}, 3)"), var))
# 
# formula_parte_1 <- formula_parte_1$var %>% paste(collapse = ' + ')
# formula_parte_1 <- glue("( {formula_parte_1} ) ^ 2")
# formula_final <- glue("flg_churn ~ county + {formula_parte_1}")

## Treinamento
modelo_vazio <- glm(flg_churn ~ 1,
                    data = tc_train,
                    family = binomial)

modelo_tudo <- glm(flg_churn ~ .^2,
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

# formula_glmm <- glue("flg_churn ~ {formula_parte_1} + (1 | county)")


glmm_step <- buildglmmTMB(flg_churn ~ ,
                          data = tc_train,
                          family = binomial,
                          buildmerControl = buildmerControl(crit = 'AIC', REML = T))
