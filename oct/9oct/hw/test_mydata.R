library(lmtest)
library(binom)
require(readxl)

data <- read.table('./9oct/hw/Kurtaev.txt')

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3

m <- lm(y ~ x1 + x2 + x3)
sm <- summary(m)
sm

# 1
correl <- cor(data)
correl

rx <- correl[1, ]

rx1 <- cor(x1, y)
rx2 <- cor(x2, y)
rx3 <- cor(x3, y)

cor.test(y, x1+x2+x3, method = "pearson") # 0.3326248 => очень слабая взаимосвязь между переменными

cor.test(y, x1+x2+x3, method = "spearman")

Hmisc::rcorr(as.matrix(data))

plot(y, x1) # Слабая положительная корреляция
plot(y, x2) # Отсутствие корреляции
plot(y, x3) # Слабая отрицательная корреляция

#2
lm1 <- lm(y ~ x1)
slm1 <- summary(lm1)
slm1

lm2 <- lm(y ~ x2)
slm2 <- summary(lm2)
slm2

lm3 <- lm(y ~ x3)
slm3 <- summary(lm3)
slm3

mlm <- lm(y ~ x1 + x2 + x3)
smlm <- summary(mlm)
smlm



#3

smlm_r_squared <- sm$r.squared
smlm_r_squared

smlm_std_error <- sm$sigma
smlm_std_error

smlm_approx_error <- sum(abs(sm$residuals/y)) / length(y) * 100
smlm_approx_error


#4
# t-критерий Стьюдента для проверки значимости коэффициента корреляции

t1 <- c(
  sqrt((rx1 ^ 2) / (1 - rx1 ^ 2) * (length(y) - sm$fstatistic[["numdf"]])),
  sqrt((rx2 ^ 2) / (1 - rx2 ^ 2) * (length(y) - sm$fstatistic[["numdf"]])),
  sqrt((rx3 ^ 2) / (1 - rx3 ^ 2) * (length(y) - sm$fstatistic[["numdf"]]))
)
t_table1 <- qt(0.975, df = sm$fstatistic[["numdf"]])
t1 < t_table1 # корреляции между каждым фактором и зависимой переменной значимо отличаются от 0

# t-критерий Стьюдента для оценки значимости параметров модели линейной регрессии
t2 <- sm$coefficients[, 1] / sm$coefficients[, 2]
t_table2 <- qt(0.975, df = sm$fstatistic[["dendf"]])
t2 < t_table2 # Все коэфы незначимы

# Тест Фишера
F_statistic <- sm$fstatistic["value"][[1]]
F_table <- 2.64

F_statistic < F_table # Модель в целом значима


#5
B <- sm$coefficients[, 1]
di_lower <- B - sm$coefficients[, 2] * t_table2
di_upper <- B + sm$coefficients[, 2] * t_table2


#6

elastichnost <- c(
  sm$coefficients[2] * mean(x1) / mean(y),
  sm$coefficients[3] * mean(x2) / mean(y),
  sm$coefficients[4] * mean(x3) / mean(y)
) # зависимая переменная изменится на 58% при изменении первого фактора на 1%, на 27% - второго.
# Зависимая переменная неэластична по обеим факторам т.к. каждая эластичность меньше 1.
# Наибольшая эластичность по 1 фактору

# Бета-коэф.
Sdy <- sd(y)
Sdx1 <- sd(x1)
Sdx2 <- sd(x2)
Sdx3 <- sd(x3)
beta <- c(sm$coefficients[2] * Sdx1/Sdy, sm$coefficients[3] * Sdx2/Sdy, sm$coefficients[4] * Sdx3/Sdy) #

# Дельта-коэф.
delta <- c(rx1 * sm$coefficients[2] / sm$r.squared,
           rx2 * sm$coefficients[3] / sm$r.squared,
           rx3 * sm$coefficients[4] / sm$r.squared)

