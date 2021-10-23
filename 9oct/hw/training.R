library("lmtest")

x1 <- c(1, 2, 3, 5)
x2 <- c(0, 1, 3, 4)
y <- c(6, 11, 19, 28)
data <- data.frame(y, x1, x2)

m <- lm(y ~ x1 + x2)
sm <- summary(m)

B <- sm$coefficients[, 1]

# t-критерий Стьюдента для проверки значимости коэффициента корреляции
rx1 <- cor(x1, y)
rx2 <- cor(x2, y)

t1 <- c(
  sqrt((rx1 ^ 2) / (1 - rx1 ^ 2) * (length(y) - sm$fstatistic[["numdf"]])),
  sqrt((rx2 ^ 2) / (1 - rx2 ^ 2) * (length(y) - sm$fstatistic[["numdf"]]))
)
t_table1 <- qt(0.975, df = sm$fstatistic[["numdf"]])
t1 < t_table1 # корреляции между каждым фактором и зависимой переменной значимо отличаются от 0

# t-критерий Стьюдента для оценки значимости параметров модели линейной регрессии
t2 <- sm$coefficients[, 1] / sm$coefficients[, 2]
t_table2 <- qt(0.975, df = sm$fstatistic[["dendf"]])
t2 < t_table2 # Все коэфы незначимы

#Доверительные интервалы
di_lower <- B - sm$coefficients[, 2] * t_table2
di_upper <- B + sm$coefficients[, 2] * t_table2

#Коэф. детерминации
r_squared <- sm$r.squared
r_squared_adj <- sm$adj.r.squared
# r_squared_adj <- 1 - (1 - r_squared^2) * (sm$fstatistic[["numdf"]] / sm$fstatistic[["dendf"]])

#Коэф. множественной корреляции
r <- sqrt(r_squared) # Связь между зависимой переменной и факторами очень сильная


# Тест Фишера
F_statistic <- sm$fstatistic["value"][[1]]
F_table <- 199.5

F_statistic < F_table # Модель в целом значима


# Средняя ошибка аппроксимации
Ap <- (sum(abs(sm$residuals/y))/length(y))*100 # находится в допустимых пределах

#Коэф. эластичности
elastichnost <- c(
  sm$coefficients[2] * mean(x1) / mean(y),
  sm$coefficients[3] * mean(x2) / mean(y)
) # зависимая переменная изменится на 58% при изменении первого фактора на 1%, на 27% - второго.
# Зависимая переменная неэластична по обеим факторам т.к. каждая эластичность меньше 1.
# Наибольшая эластичность по 1 фактору

# Бета-коэф.
Sdy <- sd(y)
Sdx1 <- sd(x1)
Sdx2 <- sd(x2)
beta <- c(sm$coefficients[2] * Sdx1/Sdy, sm$coefficients[3] * Sdx2/Sdy) #

# Дельта-коэф.
delta <- c(rx1 * sm$coefficients[2] / sm$r.squared,
           rx2 * sm$coefficients[3] / sm$r.squared)




#Тест Дарбина-Уотсона
s1 <- c(0.47, 0.28, 1.19, 1, 2.41, 2.72, 2.23, 4.04, 3.35, 5.26, 6.47, 5.28, 5.59, 9.1, 9.51, 7.12)
Ydi <- c(4.013, 5.12, 7.327, 10.133, 13.44, 18.147, 19.553, 22.46, 29.167, 30.973, 39.18, 43.687, 50.393, 55.1, 61.207, 63.213)

dw <- dwtest(m) # автокорреляция первого порядка отсутствует
bg <- bgtest(m, order = 1)
bgF <- bgtest(m, order = 1, type = "F")


#GQtest
# gq <- gqtest(y ~ x1 + x2)
# pv<a => присутствует проблема гетероскедастичности

#BPtest
bp = bptest(m, data=data);bp
# pv<a => присутствует проблема гетероскедастичности

#White_test
wt = bptest(m, data=data, varformula=~Yd + I(Yd^2), studentize=TRUE);wt


