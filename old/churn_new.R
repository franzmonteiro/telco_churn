library(tidyverse)
library(stringr)
library(glue)
library(readxl)
library(fastDummies)
library(GGally)
library(caret)
# library(MASS)

descricao <- read_delim('csvs/descricao_variaveis.csv', ';')

tc <- list.files('csvs/', pattern = '*.xlsx', full.names = T) %>%
    map(read_xlsx) %>%
    map(rename_with, ~ str_to_lower(str_replace_all(.x, ' ', '_'))) %>%
    map(select, !any_of(c('count'))) %>%
    reduce(left_join) %>%
    rename(flg_churn = churn_value,
           zip_code_population = population) %>%
    mutate(across(all_of(c('zip_code', 'satisfaction_score', 'flg_churn')), as.factor)) %>% 
    select(-c(id, customer_id, lat_long, latitude, longitude, churn_score, churn_label))


colnames(tc) %>% paste(collapse = '\n') %>% cat()

model <- glm(flg_churn ~ .,
             data = select(tc, -c(country, state, city, quarter, churn_category, churn_reason, satisfaction_score, customer_status, zip_code)),
             family = 'binomial')

empty_model <- glm(flg_churn ~ 1,
                   data = select(tc, -c(country, state, city, quarter, churn_category, churn_reason, satisfaction_score, customer_status, zip_code)),
                   family = 'binomial')

step(model, scope = list(lower = empty_model, upper = ~ .^2), direction = 'both')


modelo_vazio <- glmmTMB(flg_churn ~ 1,
                        data = tc,
                        family = binomial, 
                        REML = F)

modelo <- glmmTMB(flg_churn ~ gender + age + under_30 + senior_citizen + married + dependents 
                  + (1 | zip_code),
                  data = mutate(tc, across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x))),
                  family = binomial, 
                  REML = T)

step(modelo, scope = list(lower = modelo_vazio, upper = ~ .^2), direction = 'both')

glimpse(tc)


tc_numerics <- tc %>% 
    select(flg_churn, where(is.numeric)) %>% 
    select(-c(latitude, longitude, id, churn_score))

selected_vars <- c()
preditoras <- colnames(tc_numerics)
preditoras <- preditoras[preditoras != 'flg_churn']
# forward selection

for(i in 1:length(preditoras)) {
    mf <- glue("flg_churn ~ {preditoras[i]}") %>% as.formula()
    modelo <- glm(mf, data = tc_numerics, family = 'binomial')
    modelo_summary <- summary(modelo)
    
    modelos <- tibble(iteracao = i,
                      preditoras = preditoras)
    
    best_model <- filter(modelos, iteracao == i) %>% 
        filter(aic == max(aic))2
    
    best_model_preditoras <- str_split(best_mode$preditoras, ' [+] ') %>% unlist()
    preditoras <- preditoras[!preditoras %in% best_model_preditoras]
}


# 26.5% dos clientes cancelaram o servico.
# Taxa de churn: 26.5%
tc %>% 
    group_by(flg_churn) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(freq = qtd_clientes / sum(qtd_clientes)) %>% 
    arrange(desc(qtd_clientes))

tc %>% 
    filter(!is.na(churn_reason)) %>% 
    group_by(churn_reason) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(freq = qtd_clientes / sum(qtd_clientes)) %>% 
    arrange(desc(qtd_clientes))


tc %>% 
    filter(!is.na(churn_category)) %>% 
    group_by(churn_category) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(freq = qtd_clientes / sum(qtd_clientes)) %>% 
    arrange(desc(qtd_clientes))


churn_idx_city <- tc %>% 
    mutate(flg_churn = ifelse(flg_churn == '1', 1, 0)) %>% 
    group_by(city) %>% 
    summarise(qtd_clientes = n(),
              qtd_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_churn / qtd_clientes) %>%
    arrange(desc(taxa_churn))

quantile(tmp$taxa_churn)

ceps <- tmp %>% filter(taxa_churn >= .75)

tc %>%
    filter(zip_code %in% ceps$zip_code) %>% 
    filter(flg_churn == '1') %>% 
    group_by(churn_category) %>% 
    summarise(qtd_clientes = n()) %>% 
    mutate(freq = qtd_clientes / sum(qtd_clientes)) %>% 
    arrange(desc(qtd_clientes))

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


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                               data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                               grp <- data[1, "group"]
                               newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                               newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                               newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                               
                               if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                   stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                               1))
                                   quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                   aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                   aesthetics$alpha <- rep(1, nrow(quantiles))
                                   both <- cbind(quantiles, aesthetics)
                                   quantile_grob <- GeomPath$draw_panel(both, ...)
                                   ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                               }
                               else {
                                   ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                               }
                           })


geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
          position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
          params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


to_plot <- tc %>% 
    select(flg_churn, where(is.numeric)) %>% 
    gather(var, value, -flg_churn)

ggplot(to_plot, aes(flg_churn, value, fill = flg_churn)) +
    geom_boxplot(outlier.size = -1, color = 'black', alpha = .7) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
    facet_wrap(~ var, scales = 'free') +
    theme_light() +
    theme(axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL)


ggplot(to_plot, aes(var, value, fill = flg_churn)) +
    geom_split_violin(color = NA, alpha = .7) +
    scale_fill_manual(values = c('#d5dce0', '#ffc000')) +
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
                  v1 = total_refunds / total_charges,
                  v5 = monthly_charge / total_charges),
       aes(v5, flg_churn, color = payment_method)) +
    geom_point(alpha = .05) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = F) +
    theme_light()


to_plot_2 <- tc %>% 
    select(!where(is.numeric)) %>% 
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


test <- glm(flg_churn ~ .,
            data = tc_train,
            family = 'binomial')

summary(test)

plot(effects::allEffects(test))


# lmtest::lrtest(glm(flg_churn ~ 1, data = tc_train, family = 'binomial'), test)

# modelo.fwd.time <- system.time(
#     modelo.fwd <- regsubsets(flg_churn ~ . -city -zip_code -total_revenue -internet_type_None,
#                              data = tc_train,
#                              nvmax = 8,
#                              method = "forward")
# )

modelo <- glm(flg_churn ~ . -city -zip_code, # + offer:contract + age:gender,# + I(age^2),
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
