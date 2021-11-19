library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)
library(AER)

data <- read.table("./nov/13.11 Куртаев/Task3/task3.txt", header = TRUE)

# x <- data$"Площадь"
# y <- data$"Цена"

y <- data$"Площадь"
x <- data$"Цена"

m <- lm(y ~ x)
sm <- summary(m); sm

t_table2 <- qt(0.975, df = sm$fstatistic[["dendf"]])
B <- sm$coefficients[, 1]
di_lower <- B - sm$coefficients[, 2] * t_table2
di_upper <- B + sm$coefficients[, 2] * t_table2


approx_err <- sum(abs(sm$residuals/y)) / length(y) * 100


dw <- dwtest(m); dw

bg <- bgtest(m, order = 1, order.by = NULL); bg
# qchisq(p = 0.95, df = 2)

gq <- gqtest(m, order.by = x, fraction = 0.25); gq

bp <- bptest(m, studentize = TRUE); bp


# HC<-vcovHC(m)
# HC

HAC <-vcovHAC(m); HAC

cochrane.orcutt(m)

NW <- NeweyWest(m); NW

p <- 1 - dw$statistic/2; p

x2 <- x[2:20] - p*x[1:19]; x2
y2 <- y[2:20] - p*y[1:19]; y2

m2<-lm(y2~x2);m2
s2<-summary(m2);s2

b <- m2$coefficients[2];b
a <- m2$coefficients[1]/(1-p);a

# y3 <- 81.33074 + 0.02369024 * x
# m3 <- lm(y3~x)
# sm3 <- summary(m3); sm3

dw <- dwtest(m2); dw
bg <- bgtest(m2, order = 1, order.by = NULL);bg
bg <- bgtest(m2, order = 2, order.by = NULL);bg

gq <- gqtest(m2, fraction=0.25);gq
bp <- bptest(m2);bp

