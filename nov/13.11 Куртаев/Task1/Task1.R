library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)

# data <- read.table('./nov/13.11 Куртаев/Task1/task1.txt', dec=',', header=TRUE)
data <- read_xlsx("./nov/13.11 Куртаев/Task1/task1.xlsx")
y <- data$Y
x <- data$X

m <- lm(y ~ x)

cochrane.orcutt(m)

# HC<-vcovHC(m)
# HC

HAC<-vcovHAC(m)
HAC

bg <- bgtest(m, order = 1, order.by = NULL);bg
bg <- bgtest(m, order = 2, order.by = NULL);bg

gq <- gqtest(m, fraction=0.25);gq
bp <- bptest(m);bp

dw <- dwtest(m);dw

p <- 1 - dw$statistic/2; p

y2 <- y[2:55] - p*y[1:54]
y2

x2 <- x[2:55] - p*x[1:54]
x2

m2<-lm(y2~x2);m2
s2<-summary(m2);s2

b <- m2$coefficients[2];b
a <- m2$coefficients[1]/(1-p);a

y3 <- 8.050687 + 0.26825 * x
m3 <- lm(y3~x)

dw <- dwtest(m2); dw
bg <- bgtest(m2, order = 1, order.by = NULL);bg
bg <- bgtest(m2, order = 2, order.by = NULL);bg

gq <- gqtest(m2, fraction=0.25);gq
bp <- bptest(m2);bp