library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)

data <- read.table("./nov/13.11 Куртаев/Task3/task3.txt", header = TRUE)

x <- data$"Площадь"
y <- data$"Цена"

m <- lm(y ~ x)
sm <- summary(m)

t_table2 <- qt(0.975, df = sm$fstatistic[["dendf"]])
B <- sm$coefficients[, 1]
di_lower <- B - sm$coefficients[, 2] * t_table2
di_upper <- B + sm$coefficients[, 2] * t_table2


approx_err <- sum(abs(sm$residuals/y)) / length(y) * 100


dw <- dwtest(m); dw

bg <- bgtest(m, order = 1, order.by = NULL); bg
# qchisq(p = 0.95, df = 2)

gq <- gqtest(m, order.by = x, fraction = 0.5); gq

bp <- bptest(m, studentize = TRUE); bp

cochrane.orcutt(m)

# HC<-vcovHC(m)
# HC

HAC<-vcovHAC(m)
HAC

p <- dw$statistic/2 - 1; p

