library(lmtest)
require(readxl)

data <- read.table('./9oct/hw/Kurtaev.txt')

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3

sumx <- (x1 + x2 + x3)


#1

correl <- (round(cor(data), 2))
correl

plot(y, x1)
plot(y, x2)
plot(y, x3)


#2
lm1 <- lm(y ~ x1)
slm1 <- summary(lm1)

lm2 <- lm(y ~ x2)
slm2 <- summary(lm2)

lm3 <- lm(y ~ x3)
slm3 <- summary(lm3)

mlm <- lm(y ~ x1 + x2 + x3)
smlm <- summary(mlm)


#3

smlm_r_squared <- slm1$r.squared
smlm_r_squared

smlm_std_error <- smlm$coefficients[[5]]
smlm_std_error

smlm_approx_error <- (sum(abs(slm1$residuals/y))/length(y)) * 100
smlm_approx_error



smlm_r_squared <- smlm$r.squared
smlm_r_squared

smlm_std_error <- smlm$coefficients[[5]]
smlm_std_error

smlm_approx_error <- (sum(abs(smlm$residuals/y))/length(y))*100
smlm_approx_error

#4 Значимость параметров

smlm_coefficients <- smlm$coefficients
smlm_coefficients
B <- c(smlm$coefficients[2] , smlm$coefficients[3], smlm$coefficients[4])

B[1]



#5

Se <- c(smlm$coefficients[6], smlm$coefficients[7], smlm$coefficients[8])

# Доверительный интервал
ci_lower <- B - Se * qt(0.975, df = smlm$fstatistic[["dendf"]])
ci_upper <- B + Se * qt(0.975, df = smlm$fstatistic[["dendf"]])


#6
Sdy <- sd(y)
Sdx1 <- sd(x1)
Sdx2 <- sd(x2)
Sdx3 <- sd(x3)

beta <- c(smlm$coefficients[2] * Sdx1/Sdy, smlm$coefficients[3] * Sdx2/Sdy, smlm$coefficients[4] * Sdx3/Sdy)

rx1 <- cor(x1, y)
rx2 <- cor(x2, y)
rx3 <- cor(x3, y)

delta <- c(rx1 * smlm$coefficients[2] / smlm$r.squared,
           rx2 * smlm$coefficients[3] / smlm$r.squared,
           rx3 * smlm$coefficients[4] / smlm$r.squared)

elastichnost <- c(
  smlm$coefficients[2] * mean(x1) / mean(y),
  smlm$coefficients[3] * mean(x2) / mean(y),
  smlm$coefficients[4] * mean(x3) / mean(y)
)

beta_coefficient <- c(smlm$coefficients[2] , smlm$coefficients[3], smlm$coefficients[4])


#Student t для проверки корелляций
t_table <- qt(0.975, df = smlm$fstatistic[["dendf"]])

t_see_x1 <- sqrt((rx1 ^ 2) / (1 - rx1 ^ 2))
t_see_x1 < t_table # Корреляция значима
t_see_x1 == 0 # H0 отвергаем
t_see_x1 != 0 # H1 принмаем

t_see_x2 <- sqrt((rx2 ^ 2) / (1 - rx2 ^ 2))
t_see_x2 < t_table # Корреляция значима
t_see_x2 == 0 # H0 отвергаем
t_see_x2 != 0 # H1 принмаем

t_see_x3 <- sqrt((rx3 ^ 2) / (1 - rx3 ^ 2))
t_see_x3 < t_table # Корреляция значима
t_see_x3 == 0 # H0 отвергаем
t_see_x3 != 0 # H1 принмаем





#Тест на длинную и короткую регрессию
