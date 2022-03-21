library(tidyverse)
library(ISLR2)
library(leaps)

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

