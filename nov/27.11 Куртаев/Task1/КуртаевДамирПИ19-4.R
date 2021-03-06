library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)
library(gap)
if(!require(tseries)){install.packages('tseries')}

# data <- read.table('./nov/20.11/tema7.csv', dec=',', header=TRUE)
data <- read_xlsx("./nov/20.11/tema7.xlsx")

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3
x4 <- data$x4


y_c <- y[1:20]
y_l <- y[21:22]

x1_c <- x1[1:20]
x1_l <- x1[21:22]

x2_c <- x2[1:20]
x2_l <- x2[21:22]

x3_c <- x3[1:20]
x3_l <- x3[21:22]

x4_c <- x4[1:20]
x4_l <- x4[21:22]






# ----------------------------------------------------------------------------------------------------------------------
data_control <- data[1:20, ]
data_learning <- data[21:22, ]

lm_control <- lm(y ~ x1+x2+x3+x4, data=data_control)
summary(lm_control)

#Среднее
predict(lm_control, newdata = data_learning, interval = "confidence")
#Индивидуально
predict(lm_control, newdata = data_learning, interval = "prediction")


cor(data_control)


#БЕЗ Х3
lm_without_x3 <- lm(y ~ x1+x2+x4)
summary(lm_without_x3)

#Среднее
predict(lm_without_x3, newdata = data_learning, interval = "confidence")
#Индивидуально
predict(lm_without_x3, newdata = data_learning, interval = "prediction")

# ----------------------------------------------------------------------------------------------------------------------





lm_c <- lm(y_c ~ x1_c + x2_c + x3_c + x4_c)
slm_c <- summary(lm_c); slm_c


# train <- data[1:20,]
# test <- data[21:22, ]
#
# m <- lm(y ~ x1 + x2 + x3 + x4, data=train)

c <- data.frame(y_l, x1_l, x2_l, x3_l, x4_l)
colnames(c) <- c("y", "x1_c", "x2_c", "x3_c", "x4_c"); c

#Среднее
predict(lm_c, newdata = c, interval = "confidence")
#Индивидуально
predict(lm_c, newdata = c, interval = "prediction")


#Корреляция
model_control <- data[1:20,]; model_control
cor(model_control)


# model_control$x3 <- NULL; model_control
# summary(lm(y_c~x1_c+x2_c+x4_c))


#БЕЗ Х3
wox3 <- data.frame(y_c,x1_c,x2_c,x4_c)
lm_wox3 <- lm(wox3)
summary(lm(wox3))

#Среднее
predict(lm_wox3, newdata = c, interval = "confidence")
#Индивидуально
predict(lm_wox3, newdata = c, interval = "prediction")


#БЕЗ Х2 и Х3
wox2 <- data.frame(y_c,x1_c,x4_c)
lm_wox2 <- lm(wox2)
summary(lm_wox2)

#Среднее
predict(lm_wox2, newdata = c, interval = "confidence")
#Индивидуально
predict(lm_wox2, newdata = c, interval = "prediction")


#Парная модель
parn <- data.frame(y_c, x4_c)
lm_parn <- lm(parn)
summary(lm_parn)

#Среднее
predict(lm_parn, newdata = c, interval = "confidence")
#Индивидуально
predict(lm_parn, newdata = c, interval = "prediction")


#Тест Чоу
# library(qpcR)
o <- data[1:20,]
o1 <- o[1:10, ]
o2 <- o[11:20, ]
m <- lm(y~x1+x2+x3+x4, data = o)
m1 <- lm(y~x1+x2+x3+x4, data = o1)
m2 <- lm(y~x1+x2+x3+x4, data = o2)

RSS <- deviance(m)
RSS_1 <- deviance(m1)
RSS_2 <- deviance(m2)

n <- 22
n1 <- 10
n2 <- 10
k <- 4

F <- ((RSS - (RSS_1 + RSS_2)) / (RSS_1 + RSS_2)) * ((n1 + n2 - 2*(k+1)) / (k + 1)); F
qf(0.95,k+1,n1+n2-2*(k+1))

# H0 - верна
# выборки однородны




# p-value близится к 1 = принимаем Н0, остатки нормально распределены
jarque.bera.test(slm_c$residuals)




cor(data)


parn <- data.frame(y_c, x4_c)
lm_parn <- lm(parn)
summary(lm_parn)

# Если нормально распределены, то F
# иначе - Chi
waldtest(lm_parn, lm_c, test='F')
n <- 20
q <- 3 # кол-во удаляемых факторов
k <- 4 # кол-во иксов
m <- k+1

# F_набл > F_табл => регрессия без ограничений предпочтительнее, т.е. гипотеза Н0 отвергается
# Гипотеза Н0 - Бетта коэфициенты модели равны нулю
# Гипотеза Н1 - сумма квадратов Бетта коэфициентов модели больше нуля
# Бетта коэфициенты - различающиеся коэфициенты модели (т.е. в нашем случае х1, х2, х3)
qf(0.95,q,n-m)


resettest(lm_c)

y_predict <- predict(lm_c)
yp2 <- y_predict^2
yp3 <- y_predict^3
m4 <- lm(y_c~x1_c+x2_c+x3_c+x4_c+yp2+yp3)
sm4 <- summary(m4); sm4
residuals4 <- m4$residuals; residuals4
RSS4 <- sum(residuals4^2); RSS4

n <- 20
q2 <- 2 #кол-во удаляемых факторов
k <- 4
k2 <- k+2
m2 <- k2+1
F <- (RSS-RSS4)*(n-m2)/q2/RSS4; F


