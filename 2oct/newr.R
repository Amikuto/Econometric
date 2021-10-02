# install.packages('lmtest')

library(lmtest)
require(readxl)

# data <- read.table('./2oct/data/data.txt', dec=',', header=TRUE)
data <- read_xlsx("./2oct/data/data.xlsx")
y <- data$Y
x <- data$X

m <- lm(Y~X, data = data)
sm <- summary(m)

#Остатки
e <- sm$residuals
e

#Ошибка апроксимации
A <- (sum(abs(e/y))/length(y))*100 # 24 - мода (length(y))

# Показательная функция. У был под логарифмом, поэтому надо заменить
Y <- log10(y)
mp <- lm(Y~x)
smp <- summary(mp)
smp # Линеанизированые функции, нам нужно их анлинеализировать

# Показательная модель (функция)
a <- 10^smp$coefficients[1]#2.297
b <- 10^smp$coefficients[2]#0.0004663


#Степенная модель
X <- log10(x)
ms <- lm(Y~X)
sms <- summary(ms)
as <- 10^sms$coefficients[1]




#pvalue мал, Н0 отвергаем