# install.packages('lmtest')

library(lmtest)
require(readxl)

# data <- read.table('./2oct/data/data.txt', dec=',', header=TRUE)
data <- read_xlsx("./2oct/data/data.xlsx")
y <- data$Y
x <- data$X

m <- lm(Y~X, data = data)
sm <- summary(m)
sm


#1
cor(data$X, data$Y, method = "pearson")
plot(x, y)



#2
mydata <- round(data.frame(x, y), 1)
res <- lm(y ~ x, data = mydata)
smres <- summary(res)
coefres <- coef(res)
residuals(res)
fitted.values(res)
names(res)
vcov(res)
plot(res)



#3
plot(x, y)
# par(new=TRUE)
plot(res, add=TRUE)



#4
anova(res)
A <- (sum(abs(e/y))/length(y))*100
A



#5
# Показательная функция. У был под логарифмом, поэтому надо заменить
Y <- log10(y)
mp <- lm(Y~x)
smp <- summary(mp)
smp # Линеанизированые функции, нам нужно их анлинеализировать

# Показательная модель (функция)
Ap <- smp$coefficients[1] #2.297094
Bp <- smp$coefficients[2] #0.0004663088

ap <- 10^smp$coefficients[1]  # 198.1955
bp <- 10^smp$coefficients[2]  # 1.001074

yp <- ap * bp^x
plot(ap * bp^x)

#Степенная модель
X <- log10(x)
ms <- lm(Y~X)
sms <- summary(ms)

As <- sms$coefficients[1]  # 0.8783031
as <- 10^sms$coefficients[1]  # 7.556195
bs <- sms$coefficients[2]  # 0.6145747

ys <- as * x^bs
plot(ys)


#Гиперболическая модель

Xg <- 1/x
# Y <- log10(y)
mg <- lm(y~Xg)
smg <- summary(mg)

ag <- smg$coefficients[1]
bg <- smg$coefficients[2]

yg <- ag + (bg/x)
plot(yg)
