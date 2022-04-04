library(sf)
library(tidyverse)
library(stringr)
library(readxl)
library(ggpubr)
library(ggrepel)
library(caret)

# fonte da base com as associacoes entre cep e condado: https://worldpopulationreview.com/zips

# a variavel revenue nao sera utilizada, por nao aparecer na documentacao da ibm

read_xlsx_custom <- function(file_name) {
    dados <- read_xlsx(file_name) %>% 
        rename_with(~ str_to_lower(str_replace_all(.x, ' ', '_')))
    
    return(dados)
}

descricao <- read_delim('csvs/descricao_variaveis.csv', '|')

tc_demographics <- read_xlsx_custom('csvs/Telco_customer_churn_demographics.xlsx') %>% 
    select(customer_id, gender, age, under_30, senior_citizen, married, dependents, number_of_dependents)

tc_location <- read_xlsx_custom('csvs/Telco_customer_churn_location.xlsx') %>% 
    select(customer_id, city, zip_code, latitude, longitude)

tc_population <- read_xlsx_custom('csvs/Telco_customer_churn_population.xlsx') %>% 
    select(zip_code, population)

tc_services <- read_xlsx_custom('csvs/Telco_customer_churn_services.xlsx') %>% 
    select(customer_id, referred_a_friend, number_of_referrals, tenure_in_months, offer, 
           phone_service, avg_monthly_long_distance_charges, multiple_lines, internet_service, 
           internet_type, avg_monthly_gb_download, online_security, online_backup, device_protection_plan, 
           premium_tech_support, streaming_tv, streaming_movies, streaming_music, unlimited_data,
           contract, paperless_billing, payment_method, monthly_charge, total_charges, total_refunds, 
           total_extra_data_charges, total_long_distance_charges)


tc_status <- read_xlsx_custom('csvs/Telco_customer_churn_status.xlsx') %>% 
    select(customer_id, satisfaction_score, customer_status, churn_value, cltv, churn_category, churn_reason)


zip_county <- read_csv('csvs/world_pop_review_zip.csv') %>% 
    select(zip, county)

tc <- tc_demographics %>% 
    left_join(tc_location, by = 'customer_id') %>% 
    left_join(tc_population, by = 'zip_code') %>% 
    left_join(tc_services, by = 'customer_id') %>% 
    left_join(tc_status, by = 'customer_id') %>% 
    left_join(zip_county, by = c('zip_code' = 'zip')) %>% 
    rename(flg_churn = churn_value) %>% 
    mutate(across(all_of(c('device_protection_plan', 'internet_service', 'online_backup',
                           'online_security', 'phone_service', 'premium_tech_support',
                           'referred_a_friend', 'multiple_lines',
                           'streaming_tv', 'streaming_movies', 'streaming_music',
                           'unlimited_data', 'paperless_billing', 'under_30',
                           'senior_citizen', 'married', 'dependents')), ~ ifelse(.x == 'Yes', 1, 0))) %>% 
    mutate(valor_cobranca_geral = total_charges + total_long_distance_charges + total_extra_data_charges,
           tx_valores_reembolsados_1 = total_refunds / valor_cobranca_geral,
           tx_valores_reembolsados_2 = ifelse((total_long_distance_charges + total_extra_data_charges) == 0, 0, total_refunds / (total_long_distance_charges + total_extra_data_charges)),
           tx_concentracao_cobranca_mes_q3 = monthly_charge / total_charges, # possivel indicador da quantidade de meses que o cliente esta com a companhia, e se o valor da mensalidade atual eh superior ao das mensalidades anteriores
           tx_contrib_cobrancas_extras_cobranca_geral = (total_long_distance_charges + total_extra_data_charges) / valor_cobranca_geral,
           total_gb_downloaded = tenure_in_months * avg_monthly_gb_download,
           qtd_mensal_media_indicacoes  = number_of_referrals / tenure_in_months,
           valor_mensal_medio_cobrancas_extras = (total_long_distance_charges + total_extra_data_charges) / tenure_in_months,
           valor_mensal_medio_cobrancas_totais = total_charges / tenure_in_months,
           valor_mensal_medio_cobranca_geral = valor_cobranca_geral / tenure_in_months,
           relacao_valor_cobranca_atual_valor_medio_mensal = monthly_charge / valor_mensal_medio_cobrancas_totais, # possivel forma de identificar que o cliente teve aumento na fatura
           qtd_servicos_adicionais = device_protection_plan + internet_service + online_backup + online_security + phone_service + premium_tech_support + unlimited_data,
           qtd_streamings_utilizados = streaming_movies + streaming_music + streaming_tv,
           tx_contrib_cobrancas_cliente_base_total_1 = total_charges / sum(total_charges),
           tx_contrib_cobrancas_cliente_base_total_2 = valor_cobranca_geral / sum(valor_cobranca_geral)) %>% 
    mutate(across(all_of(c('device_protection_plan', 'internet_service', 'online_backup',
                           'online_security', 'phone_service', 'premium_tech_support',
                           'referred_a_friend', 'multiple_lines',
                           'streaming_tv', 'streaming_movies', 'streaming_music',
                           'unlimited_data', 'paperless_billing', 'under_30',
                           'senior_citizen', 'married', 'dependents')), as.factor))

# Caracteristicas estranhas observadas nos dados:
#   filter(tc, total_long_distance_charges > total_charges) %>% select(total_long_distance_charges, total_charges) %>% View()
#   filter(tc, total_extra_data_charges > total_charges) %>% select(total_extra_data_charges, total_charges) %>% View()
# Por isso, entende-se que a variavel 'total_charges', nao compreende as cobrancas extras

# interacoes para teste: tenure_in_months * cltv

tc$tx_concentracao_cobranca_mes_q3 %>% summary()

rm(tc_demographics)
rm(tc_location)
rm(tc_population)
rm(tc_services)
rm(tc_status)

dados_geo <- tc %>% 
    select(customer_id, churn_category, flg_churn, zip_code, latitude, longitude, county)


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


shp_counties <- read_sf("ca_counties/CA_Counties_TIGER2016.shp") %>% 
    left_join(churn_county, by = c('NAME' = 'county'))


sf_long_lat <- churn_long_lat %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


p1 <- ggplot() +
    geom_sf(data = shp_counties, fill = NA, size = 0.3) +
    geom_sf(data = sf_long_lat, mapping = aes(color = taxa_churn, size = taxa_churn, alpha = taxa_churn)) +
    scale_size(guide = 'none') +
    scale_alpha(guide = 'none') +
    scale_color_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, color = NULL,
         title = '% Churn')

p2 <- ggplot() +
    geom_sf(data = shp_counties, fill = NA, size = 0.3) +
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
    geom_sf(data = shp_counties, mapping = aes(fill = taxa_churn), size = 0.3) +
    geom_label_repel(
        data = head(arrange(shp_counties, desc(taxa_churn)), 5),
        aes(label = NAME, geometry = geometry),
        size = 2,
        stat = "sf_coordinates",
        min.segment.length = 0) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = '',
         title = '% Churn')


p4 <- ggplot() +
    geom_sf(data = shp_counties, mapping = aes(fill = taxa_contrib_qtd_churn), size = 0.3) +
    geom_label_repel(
        data = head(arrange(shp_counties, desc(taxa_contrib_qtd_churn)), 5),
        aes(label = NAME, geometry = geometry),
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
    geom_sf(data = shp_counties, mapping = aes(fill = churn_category), size = 0.3) +
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
    select(-c(latitude, longitude, zip_code)) %>% 
    gather(var, value, -flg_churn)

ggplot(to_plot, aes(flg_churn, value, fill = flg_churn)) +
    geom_boxplot(outlier.size = -1, color = 'black', alpha = .7) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    # scale_fill_viridis_d() +
    facet_wrap(~ var, scales = 'free') +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(x = NULL, y = NULL, fill = 'Churn ?')


ggplot(to_plot, aes(value, fill = flg_churn)) +
    geom_density(alpha = 0.7, color = NA) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    facet_wrap(~ var, scales = 'free') +
    theme_light() +
    labs(x = NULL, y = NULL)

ggplot(to_plot %>% 
           mutate(flg_churn = ifelse(flg_churn == '1', 1, 0)),
       aes(value, flg_churn)) +
    geom_point(alpha = .05) +
    facet_wrap(~ var, scales = 'free') +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = T) +
    theme_light() +
    labs(x = NULL, y = NULL)


ggplot(tc, aes(tx_contrib_cobrancas_extras_cobranca_geral, 
               ifelse(flg_churn == '1', 1, 0))) +
    geom_point(alpha = .05) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = F) +
    theme_light()


to_plot_2 <- tc %>% 
    select(!where(is.numeric), -customer_id) %>% 
    mutate(across(everything(), as.character)) %>% 
    gather(var, value, -flg_churn)

ggplot(to_plot_2 %>% filter(!var %in% c('city', 'zip_code', 'churn_reason', 'churn_category', 'country', 'state', 'customer_status', 'quarter')),
       aes(value, color = flg_churn, fill = flg_churn)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
    scale_color_manual(values = c('#d5dce0', '#ffc000')) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    facet_wrap(~ var, scales = 'free_y') +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = NULL, y = NULL, color = 'Churn ?', fill = 'Churn ?') +
    coord_flip()


# Amostragem
set.seed(1)
train_idx <- createDataPartition(tc$flg_churn, p = .7, list = F)
tc_train <- tc[train_idx,]
tc_test <- tc[-train_idx,]
rm(tc)

## Treinamento
modelo_vazio <- glm(flg_churn ~ 1,
                        data = select(tc_train, -c(customer_id, city, churn_category, churn_reason, satisfaction_score, customer_status, zip_code)),
                        family = 'binomial')

modelo_logistico <- glm(flg_churn ~ . ^2,
        data = select(tc_train, -c(customer_id, city, churn_category, churn_reason, satisfaction_score, customer_status, zip_code)),
        family = 'binomial')

summary(modelo_logistico)

tmp <- MASS::stepAIC(modelo_logistico,
            scope = list(lower = modelo_vazio, upper = modelo_logistico),
            direction = 'both',
            trace = 1)

summary(tmp)

# modelo_vazio <- glmmTMB(flg_churn ~ 1,
#                         data = tc,
#                         family = binomial, 
#                         REML = F)
