library(lmtest)
require(readxl)

# data <- read.table('./2oct/data/data.txt', dec=',', header=TRUE)
data <- read_xlsx("./2oct/test/data.xlsx")
y <- data$Y
x <- data$X



#Диаграмма рассеяния
plot(x, y)

#
sumY <- sum(data$Y)
mean_Y <- mean(data$Y)
sumX <- sum(data$X)
mean_X <- mean(data$X)



XminusXmean <- x - mean_X  # X-Xср
XminusXmeanInSquare <- XminusXmean ^ 2
sumXminusXmeanInSquare <- sum(XminusXmeanInSquare)

YminusYmean <- y - mean_Y  # Y-Yср

XminusXmeanMultiYminusYmean <- XminusXmean*YminusYmean # (Х - Хср) * (У - Уср)
sumXminusXmeanMultiYminusYmean <- sum(XminusXmeanMultiYminusYmean)

beta <- sumXminusXmeanMultiYminusYmean / sumXminusXmeanInSquare
alpha <- mean_Y - beta*mean_X

Y_predicted <- alpha + beta*x
ostatki <- y - Y_predicted

plot(Yip)
lines(y)


m <- lm(y~x, data=data)
m

sm <- summary(m)
sm

e <- sm$residuals
e

A <- (sum(abs(e/y))/length(y))*100 # 24 - мода (length(y))
A




