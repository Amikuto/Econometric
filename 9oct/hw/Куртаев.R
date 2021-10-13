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

smlm
smlm_r_squared <- smlm$r.squared

smlm_std_error <- smlm$coefficients[[5]]
smlm_std_error

smlm_approx_error <- (sum(abs(smlm$residuals/y))/length(y))*100
smlm_approx_error

#4
smlm_coefficients <- smlm$coefficients
smlm_coefficients


#5

beta <- c(smlm$coefficients[2], smlm$coefficients[3], smlm$coefficients[4])
SE <- c(smlm$coefficients[6], smlm$coefficients[7], smlm$coefficients[8])

ci.lower <- beta - qt(0.975, df = smlm$fstatistic[["dendf"]]) * SE
ci.upper <- beta + qt(0.975, df = smlm$fstatistic[["dendf"]]) * SE




