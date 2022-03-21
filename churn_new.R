library(tidyverse)
library(stringr)
library(glue)
library(readxl)
library(fastDummies)
library(GGally)
library(caret)
# library(MASS)

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
    mutate(across(all_of(c('zip_code', 'satisfaction_score', 'flg_churn')), as.factor),
           across(where(is.character), as.factor))

# 26.5% dos clientes cancelaram o servico.
# Taxa de churn: 26.5%
table(tc$flg_churn) %>% prop.table()

ggcorr(tc,
       # legend.position = 'bottom',
       # legend.size = 12,
       # geom = "circle",
       label = T,
       label_round = 2,
       label_size = 3,
       label_alpha = T,
       hjust = 1,
       size = 3,
       color = 'grey50',
       layout.exp = 3)

to_plot <- tc %>% 
    select(flg_churn, where(is.numeric)) %>% 
    gather(var, value, -flg_churn)

ggplot(to_plot, aes(flg_churn, value, fill = flg_churn)) +
    geom_boxplot() +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    facet_wrap(~ var, scales = 'free') +
    theme_light() +
    labs(x = NULL, y = NULL)

ggplot(tc, aes(flg_churn, total_refunds, fill = flg_churn)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 1)) +
    theme_light()


ggplot(tc, aes(number_of_referrals, age, color = flg_churn)) +
    geom_jitter() +
    # geom_jitter(shape = 1) +
    theme_light()

ggplot(tc, aes(contract, zip_code, color = flg_churn)) +
    geom_jitter() +
    # geom_jitter(shape = 1) +
    theme_light()

to_plot_2 <- tc %>% 
    select(!where(is.numeric)) %>% 
    gather(var, value, -flg_churn)

ggplot(to_plot_2 %>% filter(!var %in% c('city', 'zip_code')),
       aes(value, color = flg_churn, fill = flg_churn)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
    scale_color_manual(values = c('#d5dce0', '#ffc000')) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    facet_wrap(~ var, scales = 'free_x') +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = NULL, y = NULL)


# ggpairs(select(tc, where(is.numeric)), columns = 1:7, lower = list(continuous = "smooth")) +
#     scale_color_viridis_c() +
#     theme_light()
# 
# ggpairs(select(tc, where(is.numeric))[,7:14])

tc_dummies <- tc %>% 
    dummy_cols(select_columns = c('internet_type'),
               remove_selected_columns = T,
               remove_first_dummy = T)

set.seed(1)
train_idx <- createDataPartition(tc_dummies$flg_churn, p = .7, list = F)
tc_train <- tc_dummies[train_idx,]
tc_test <- tc_dummies[-train_idx,]


# modelo.fwd.time <- system.time(
#     modelo.fwd <- regsubsets(flg_churn ~ . -city -zip_code -total_revenue -internet_type_None,
#                              data = tc_train,
#                              nvmax = 8,
#                              method = "forward")
# )

modelo <- glm(flg_churn ~ . -city -zip_code + offer:contract + age:gender + I(age^2),
              data = tc_train,
              family = 'binomial')

# modelo.both <- MASS::stepAIC(modelo, direction = 'both')

resumo <- summary(modelo)
resumo

# confint(modelo)
resumo$coefficients %>% 
    as.data.frame() %>% 
    mutate(across(where(is.numeric), round, 2)) %>% View()

modelo_2 <- update(modelo, ~ . -total_revenue -internet_type_None)

summary(modelo_2)

modelo$aic
modelo_2$aic

tc_test$churn_prob <- predict(modelo_2, newdata = select(tc_test, -flg_churn), type = 'response')

tc_test <- tc_test %>% 
    mutate(flg_churn_predicted = as.factor(ifelse(churn_prob > 0.5, 1, 0)))

# acuracia do modelo
mean(tc_test$flg_churn == tc_test$flg_churn_predicted)

table(tc_test$flg_churn, tc_test$flg_churn_predicted) %>% caret::confusionMatrix()

# caret::twoClassSummary()
