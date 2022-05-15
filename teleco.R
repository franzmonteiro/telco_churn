library(tidyverse) # ok
library(stringr)
library(glue)
library(readxl)

dados <- read_excel("csvs/teleco.xlsx", sheet = "churn_operadoras") %>% 
    rename(operadora = 1) %>% 
    gather(trimestre, taxa_churn, matches('[0-9]T')) %>% 
    mutate(trimestre = as_factor(trimestre),
           taxa_churn = round(100 * taxa_churn, 1),
           operadora = ifelse(str_detect(operadora, 'Claro'), 'Claro (inclusive Nextel)', operadora)) %>% 
    filter(operadora != 'Churn Brasil')

ggplot(dados, aes(trimestre, taxa_churn, color = operadora)) +
    geom_line(aes(group = operadora)) +
    geom_point(aes(shape = operadora)) +
    scale_color_viridis_d(option = 'D') +
    theme_light() +
    labs(x = NULL, y = NULL, color = NULL, shape = NULL)

# title = '% Churn trimestral, por operadora de celular, no Brasil'

ggsave("plots/teleco_churn_trimestral_operadoras_celular.png", width = 9, height = 5)



dados <- read_excel("csvs/teleco.xlsx", sheet = "market_share_operadoras") %>% 
    rename(operadora = 1) %>% 
    filter(operadora != 'Celulares') %>% 
    gather(periodo, mshare, -operadora) %>% 
    mutate(periodo = as_factor(periodo),
           mshare = round(100 * mshare, 2)) %>% 
    filter(!str_detect(periodo, '2018|2019|22'))

ggplot(dados, aes(periodo, mshare, fill = operadora)) +
    geom_col() +
    scale_fill_viridis_d() +
    theme_light() +
    labs(x = NULL, y = NULL, fill = NULL)
# title = 'Market share, por operadora de celular, no Brasil',

ggsave("plots/teleco_mshare_operadora_celular_brasil.png", width = 9, height = 5)
