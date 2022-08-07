library(tidyverse)
tc_test <- readRDS('csvs/tc_test.rds')
tc_train <- readRDS('csvs/tc_train.rds')
tc <- tc_train %>% 
    rbind(tc_test)

corr_m <- tc %>%
    select(where(is.double)) %>%
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
    mutate(correlacao_abs = abs(correlacao))


top10_maiores_correlacoes <- slice_max(corr_m, correlacao_abs, n = 10) %>% 
    select(-c(correlacao_abs))

# write_delim(top10_maiores_correlacoes, 'csvs/top10_maiores_correlacoes.csv', delim = ';')
