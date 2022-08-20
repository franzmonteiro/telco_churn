library(tidyverse)
library(glue)
theme_set(theme_light())


aic_modelo_rl_forward <- read_delim('new_modelagem/reducao_dos_aics/modelo_rl_forward.csv', delim = ';') %>% 
    mutate(criterio = 'forward',
           step = row_number())

aic_modelo_rl_backward <- read_delim('new_modelagem/reducao_dos_aics/modelo_rl_backward.csv', delim = ';') %>% 
    mutate(criterio = 'backward',
           step = row_number())

aic_modelo_rl_both <- read_delim('new_modelagem/reducao_dos_aics/modelo_rl_both.csv', delim = ';') %>% 
    mutate(criterio = 'both',
           step = row_number())

aic_modelos_rl <- aic_modelo_rl_forward %>% 
    rbind(aic_modelo_rl_backward) %>% 
    rbind(aic_modelo_rl_both) %>% 
    mutate(qtd_variaveis = ifelse(formula_modelo == 'flg_churn ~ 1',
                                  0,
                                  str_count(formula_modelo, '[+]') + 1))

to_plot_aic_modelos_rl <- aic_modelos_rl %>% 
    pivot_longer(c(aic, qtd_variaveis), names_to = 'var', values_to = 'value') %>% 
    mutate(var = case_when(var == 'aic' ~ 'AIC',
                           var == 'qtd_variaveis' ~ 'Quantidade de variáveis',
                           T ~ 'Outro'))

ggplot(to_plot_aic_modelos_rl, aes(step, value, color = criterio)) +
    geom_line() +
    # geom_point() +
    facet_wrap(~ var, ncol = 2, scales = 'free_y') +
    scale_color_viridis_d() +
    labs(x = 'Iteração', y = NULL, color = 'Critério')


ggsave("new_modelagem/reducao_dos_aics/reducao_dos_aics.png", width = 9, height = 5)


ggplot(to_plot_aic_modelos_rl %>% 
           filter(var == 'AIC'), aes(step, value, color = criterio)) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = 'Iteração', y = 'AIC', color = 'Critério')

ggsave("new_modelagem/reducao_dos_aics/apenas_reducao_dos_aics.png", width = 9, height = 5)


ggplot(to_plot_aic_modelos_rl %>% 
           filter(var == 'Quantidade de variáveis'), aes(step, value, color = criterio)) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = 'Iteração', y = 'Quantidade de variáveis', color = 'Critério')

ggsave("new_modelagem/reducao_dos_aics/apenas_qtd_variaveis.png", width = 9, height = 5)


resumo_modelos_rl <- aic_modelos_rl %>% 
    group_by(criterio) %>% 
    summarise(across(everything(), list(first = first, last = last), .names = '{.fn}_{.col}')) %>% 
    pivot_longer(matches('first_|last_'),
                 names_to = c('set', '.value'),
                 names_pattern = '(first|last)_(.+)') %>% 
    arrange(criterio, set) %>% 
    select(criterio, set, step, aic, qtd_variaveis) %>% 
    mutate(set = ifelse(set == 'first', 'primeira', 'última')) %>% 
    rename(iteracao = step)

write_delim(resumo_modelos_rl, 'new_modelagem/reducao_dos_aics/resumo_modelos_rl.csv', delim = ';')    



resumo_modelos_rl <- aic_modelos_rl %>% 
    group_by(criterio) %>% 
    summarise(across(everything(), list(first = first, last = last), .names = '{.fn}_{.col}')) %>% 
    pivot_longer(matches('first_|last_'),
                 names_to = c('set', '.value'),
                 names_pattern = '(first|last)_(.+)') %>% 
    arrange(criterio, set) %>% 
    select(criterio, set, step, aic, qtd_variaveis, formula_modelo) %>% 
    mutate(set = ifelse(set == 'first', 'primeira', 'última')) %>% 
    rename(iteracao = step)
