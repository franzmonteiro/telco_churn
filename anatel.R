library(tidyverse) # ok
library(stringr)
library(glue)
library(readxl)

dados <- read_excel('csvs/anatel - indicadores internacionais - assinantes por pais servico e ano.xlsx') %>% 
    filter(`País` == 'Brasil') %>% 
    rename_with(~ str_replace_all(str_to_lower(.x), ' ', '_')) %>% 
    mutate(assinantes = assinantes / 1000000)

ggplot(dados, aes(ano, assinantes, color = categoria_de_assinantes, shape = categoria_de_assinantes)) +
    geom_line(aes(group = categoria_de_assinantes)) +
    geom_point() +
    scale_color_viridis_d() +
    theme_light() +
    labs(x = NULL, y = NULL, color = NULL, shape = NULL)


dados <- read_excel("csvs/teleco.xlsx", sheet = "churn_operadoras")
