library(lmtest)
library(binom)
library(forecast)
library(tseries)
require(readxl)

data <- read_xlsx('./Task2/data.xlsx')

y <- data$IP

t <- c(1:40)
d1 <- rep(c(1, 0, 0, 0), times = 10)
d2 <- rep(c(0, 1, 0, 0), times = 10)
d3 <- rep(c(0, 0, 1, 0), times = 10)

lm <- lm(y~t+d1+d2+d3)
slm <- summary(lm)


ip <- ts(data = y, start = c(2007), frequency = 4, name = "IP")
ar_ip <- arima(ip, seasonal = list(order = c(0, 1, 2)))
pred_arrima <- forecast(ar_ip)
pred_arrima$mean


pred <- data.frame(c(41 : 48), c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0))
colnames(pred) <- c("t", "d1", "d2", "d3")
predicted <- predict(lm, newdata = pred); predicted

