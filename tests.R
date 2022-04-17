library(tidyverse)
library(ISLR2)
library(leaps)
library(caret)

Hitters <- na.omit(Hitters)
sum(is.na(Hitters))

regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)


regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)

names(regfit.full)

reg.summary$rsq

par(mfrow = c(2, 2))
plot(reg.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2 [11] , col = "red", cex = 2, pch = 20)


plot(regfit.full , scale = "adjr2")

coef(regfit.full, 6)


regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
regfit.seqrep <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "seqrep")

summary(regfit.seqrep)

regfit.seqrep.summary <- summary(regfit.seqrep)
regfit.seqrep.summary$adjr2

tp_plot <- tibble(number_variables = 1:length(reg.summary$adjr2),
                  adjr2 = reg.summary$adjr2)

ggplot(tp_plot, aes(number_variables, adjr2)) +
    geom_line() +
    geom_point() +
    theme_light()




library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs
(default <- as_tibble(ISLR2::Default))

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]

model1 <- glm(default ~ balance, family = "binomial", data = train)

default %>%
    mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
    ggplot(aes(balance, prob, color = student)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle("Logistic regression model fit") +
    xlab("Balance") +
    ylab("Probability of Default")


install.packages('tidycensus')
??tidycensus

ggplot(tidycensus::county_laea) + geom_sf()



tarr <- tidycensus::get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)
ggplot(tarr, aes(fill = estimate, color = estimate)) +
    geom_sf() +
    coord_sf(crs = 26914) +
    scale_fill_viridis(option = "magma") +
    scale_color_viridis(option = "magma")

library(choroplethr)

data(df_county_demographics)



library(tidycensus)
library(tidyverse)
library(viridis)

vars10 <- c("P005003", "P005004", "P005006", "P004003")
il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "CA", geometry = F) %>%
    mutate(pct = 100 * (value / summary_value))


?get_acs
tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)

ggplot(tarr, aes(fill = estimate, color = estimate)) +
    geom_sf() +
    coord_sf(crs = 26914) +
    scale_fill_viridis(option = "magma") +
    scale_color_viridis(option = "magma")


v_dez <- load_variables(2010, 'sf1', cache = T)


dados <- get_decennial(geography = "county",
                       variables = c('P001001', 'P012002', 'P002002',
                                     'P013001', 'Total'),
                       state = "CA",
                       geometry = F) %>% 
    left_join(v_dez, by = c('variable' = 'name')) %>% 
    mutate(nome = case_when(variable == 'P001001' ~ 'qtd_populacao_total',
                            variable == 'P012002' ~ 'qtd_populacao_masculina',
                            variable == 'P002002' ~ 'qtd_populacao_urbana',
                            variable == 'P013001' ~ 'idade_mediana')) %>% 
    select(NAME, nome, value) %>%
    spread(nome, value)

dados





# pagsr::salvar_planilha_excel(v19, 'v19')



get_acs(geography = "county",
        variables = c('B19083_001'),
        state = "CA",
        year = 2017,
        geometry = T) %>% 
    ggplot() +
    geom_sf(mapping = aes(fill = estimate)) +
    scale_fill_viridis_c() +
    facet_wrap(~ variable) +
    theme_light()

# internet subscriptions in household, computers in household, gini index of income inequality, 'median age by sex', 'total population', 'nativity and citizenship status in the united states', 'household size by vehicles available', 'household size by number of workers in household', 'timearriving at work from home for workplace geography', worker population for workplace geography, population under 18 years by age, living arrangements of adults 18 years and over by age, household type (including living alone), family type by presence and age of own children under 18 years, median age at first marriage, marriages in the last year by sex by marital status for the population 15 years and over, divorces in the last year by sex by marital status for the population 15 years and over, number of times married by sex by marital status for the population 15 years and over, school enrollment by level of school for the population 3 years and over, poverty status in the past 12 months by school enrollment by level of school for the population 3 years and over, total fields of bachelor's degrees reported, poverty status in the past 12 months by sex by age, poverty status in the past 12 months by age, earnings in the past 12 months for households, wage or salary income in the past 12 months for households, family income in the past 12 months (in 2018 inflation-adjusted dollars), median family income in the past 12 months (in 2018 inflation-adjusted dollars), number of earners in family,employment status for the population 16 years and over,  housing units, occupancy status, tenure, median number of rooms, mortgage status by age of householder, median year structure built, median value (dollars)

v19 %>%
    mutate(concept = str_to_lower(concept)) %>% 
    View()
    
dados <- c('B19013', 'B19001', 'B14001', 'B28003', 'B28002', 'B01001') %>% 
    map_dfr(~ get_acs(geography = "county",
                      table = .x,
                      state = "CA",
                      year = 2017,
                      geometry = F)) %>% 
    left_join(v19, by = c('variable' = 'name'))


v19_tmp %>% select(concept) %>% distinct() %>% View()

test <- get_decennial(geography = "county", 
                variables = c("P013001", ),
                state = "CA",
                year = 2010,
                geometry = F)


test %>%
    ggplot(aes(x = value, y = reorder(NAME, value))) + 
    geom_point()

ggplot(test) +
    geom_sf(aes(fill = value)) +
    scale_fill_viridis_c() +
    theme_light() +
    labs(title = 'Median age, by zip code')
    # labs(title = 'Housing units, by zip code')

plotly::ggplotly()


test <- blocks(state = "AS", year = 2010)




dados <- read_csv("csvs/syriatel.csv") %>% 
    rename_with(~ str_to_lower(str_replace_all(.x, ' ', '_'))) %>% 
    mutate(churn = as.factor(ifelse(churn, 1, 0)),
           area_code = as.factor(area_code))

dados_std <- dados %>% 
    mutate(across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x)))

dados_std %>% select(where(is.numeric)) %>% glimpse()

# Amostragem
set.seed(1)
train_idx <- createDataPartition(dados_std$churn, p = .7, list = F)
dados_train <- dados_std[train_idx,]
dados_test <- dados_std[-train_idx,]
# rm(tc)

dim(dados_train)
glimpse(dados_train)

dados_train$churn %>% table()
dados_train$churn %>% table() %>% prop.table()
dados_test$churn %>% table() %>% prop.table()


## Treinamento
modelo_vazio <- glm(churn ~ 1,
                    data = dados_train,
                    family = binomial)

modelo_tudo <- glm(churn ~ . -phone_number,
                   data = dados_train,
                   family = binomial)

summary(modelo_tudo)

AIC(modelo_tudo)

modelo_step <- MASS::stepAIC(modelo_vazio,
                             direction = 'both',
                             scope = list(lower = modelo_vazio, upper = modelo_tudo),
                             trace = 1)

summary(modelo_step)

library(glmmTMB)

glmm_vazio <- glmmTMB(churn ~ 1 + (1 | state),
                      data = dados_train,
                      family = binomial,
                      REML = T)

AIC(glmm_vazio)

formula_str <- paste(colnames(dados_train)[!colnames(dados_train) %in% c('state', 'phone_number')], collapse = ' + ')
(formula_str <- glue::glue("churn ~ {formula_str} + (1 | state)"))

library(buildmer)

glmm_outro <- buildglmmTMB(as.formula(formula_str),
                           data = dados_train,
                           family = binomial,
                           buildmerControl = buildmerControl(crit = 'AIC', REML = T))

AIC(glmm_outro)
