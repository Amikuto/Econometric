library(lmtest)
library(binom)
library(forecast)
library(tseries)
require(readxl)

data <- read_xlsx('./oct/30oct/Task3/data.xlsx')

x <- data$Year
y <- data$GDP
GDP <- data$GDP

t <- c(1:40)
d1 <- rep(c(1, 0, 0, 0), times = 10)
d2 <- rep(c(0, 1, 0, 0), times = 10)
d3 <- rep(c(0, 0, 1, 0), times = 10)

lm <- lm(y~t+d1+d2+d3)
slm <- summary(lm)

plot(x, y)


Se <- c(slm$coefficients[7], slm$coefficients[8], slm$coefficients[9], slm$coefficients[10])
B <- c(slm$coefficients[2] , slm$coefficients[3], slm$coefficients[4], slm$coefficients[5])
ci_lower <- B - Se * qt(0.975, df = slm$fstatistic[["dendf"]])
ci_upper <- B + Se * qt(0.975, df = slm$fstatistic[["dendf"]])


pred <- data.frame(c(41 : 43), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
colnames(pred) <- c("t", "d1", "d2", "d3")
predicted <- predict(lm, newdata = pred); predicted

gq <- gqtest(lm, fraction=0.5, data=data);gq
bp <-  bptest(lm, data=data, studentize = TRUE);bp

dw <- dwtest(lm);dw
bg <- bgtest(lm, order = 1, order.by = NULL, type = c("Chisq", "F")); bg

GDP1 <- ts(data = GDP, start = c(2009), frequency = 4, name = "GDP")
acf_data <- acf(GDP1, plot=FALSE); acf_data
pacf_data <- pacf(GDP1, plot=FALSE, lag.max = 10); pacf_data
tsdisplay(GDP1)


adf.test(GDP1, alternative = "stationary") # стационарый
PP.test(GDP1) # стационарынй
Box.test(GDP1, lag = 12,type = "Box-Pierce")
kpss.test(GDP1)