library(lmtest)
library(forecast)
library(tseries)
require(readxl)
library(orcutt)

data <- read_xlsx('./nov/6nov/стр92/data_7and8_page92.xlsx')
data

y <- data$Y; y
x1 <- data$X1; x1
x2 <- data$X2; x2
m <- lm(y ~ x1+x2); m

cochrane.orcutt(m)