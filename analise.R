library(sf)
library(ggrepel)
library(tidyverse)
library(stringr)
library(readxl)
library(ggpubr)

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
           total_extra_data_charges, total_long_distance_charges, total_revenue)


tc_status <- read_xlsx_custom('csvs/Telco_customer_churn_status.xlsx') %>% 
    select(customer_id, satisfaction_score, customer_status, churn_value, cltv, churn_category, churn_reason)


tc <- tc_demographics %>% 
    left_join(tc_location, by = 'customer_id') %>% 
    left_join(tc_population, by = 'zip_code') %>% 
    left_join(tc_services, by = 'customer_id') %>% 
    left_join(tc_status, by = 'customer_id') %>% 
    rename(flg_churn = churn_value) %>% 
    mutate(across(all_of(c('device_protection_plan', 'internet_service', 'online_backup',
                           'online_security', 'phone_service', 'premium_tech_support',
                           'referred_a_friend', 'multiple_lines',
                           'streaming_tv', 'streaming_movies', 'streaming_music',
                           'unlimited_data', 'paperless_billing', 'under_30',
                           'senior_citizen', 'married', 'dependents')), ~ ifelse(.x == 'Yes', 1, 0))) %>% 
    mutate(taxa_valores_reembolsados = total_refunds / total_charges,
           taxa_concentracao_cobranca_mes_q3 = monthly_charge / total_charges,
           v1 = avg_monthly_long_distance_charges / monthly_charge,
           v2 = total_extra_data_charges / total_charges,
           v3 = total_long_distance_charges / total_charges,
           v4 = tenure_in_months * avg_monthly_gb_download,
           v5 = tenure_in_months * cltv,
           v6  = number_of_referrals / tenure_in_months,
           v7 = total_extra_data_charges / tenure_in_months,
           v8 = total_charges / tenure_in_months,
           v9 = total_extra_data_charges / tenure_in_months,
           qtd_servicos_assinados = device_protection_plan + internet_service + online_backup + online_security + phone_service + premium_tech_support + unlimited_data,
           qtd_streamings_utilizados = streaming_movies + streaming_music + streaming_tv,
           v100 = tenure_in_months * avg_monthly_long_distance_charges # igual a total_long_distance_charges
           )


tc$taxa_concentracao_cobranca_mes_q3 %>% summary()

rm(tc_demographics)
rm(tc_location)
rm(tc_population)
rm(tc_services)
rm(tc_status)


zip_county <- read_csv('csvs/world_pop_review_zip.csv')

dados_geo <- tc %>% 
    select(flg_churn, zip_code, latitude, longitude) %>% 
    left_join(zip_county, by = c('zip_code' = 'zip'))

churn_county <- dados_geo %>% 
    group_by(county) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes) %>%
    arrange(desc(taxa_churn))


churn_city <- dados_geo %>% 
    group_by(city) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes) %>%
    arrange(desc(taxa_churn))

churn_long_lat <- dados_geo %>% 
    group_by(longitude, latitude) %>% 
    summarise(qtd_clientes = n(),
              qtd_clientes_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_clientes_churn / qtd_clientes) %>%
    arrange(desc(taxa_churn))


shp_counties <- read_sf("ca_counties/CA_Counties_TIGER2016.shp") %>% 
    left_join(churn_county, by = c('NAME' = 'county'))

p1 <- ggplot(shp_counties) +
    geom_sf(mapping = aes(fill = taxa_churn)) +
    geom_sf_text(mapping = aes(label = NAME), size = 1.5, color = 'white') +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = 'Churn')

p1

sf_long_lat <- churn_long_lat %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


p2 <- ggplot() +
    geom_sf(data = shp_counties) +
    geom_sf(data = sf_long_lat, mapping = aes(color = taxa_churn, size = taxa_churn, alpha = taxa_churn)) +
    scale_size(guide = 'none') +
    scale_alpha(guide = 'none') +
    scale_color_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL, color = 'Churn')


ggarrange(p1, p2, nrow = 1, common.legend = T, legend = 'right')

ggsave("plots/taxa_churn.png", width = 9, height = 5)

shp_places <- read_sf("ca_places_boundaries/CA_Places_TIGER2016.shp") %>% 
    left_join(churn_city, by = c('NAME' = 'city'))

ggplot() +
    geom_sf(data = shp_counties) +
    geom_sf(data = shp_places, mapping = aes(fill = taxa_churn)) +
    scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = 'Churn')


# tmap_arrange(map_counties, map_places, map_long_lat, nrow = 3)

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
    theme(axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL)


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


ggplot(tc %>% 
           mutate(flg_churn = ifelse(flg_churn == '1', 1, 0),
                  taxa_valores_reembolsados = total_refunds / total_charges,
                  taxa_c_cobranca_1m_q3 = monthly_charge / total_charges),
       aes(v5, flg_churn, color = payment_method)) +
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
    labs(x = NULL, y = NULL) +
    coord_flip()
