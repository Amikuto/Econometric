
library(lmtest)
require(readxl)

# data <- read.csv("./9oct/data/GAM.csv", sep = "\t")
# data <- read.table("./9oct/data/GAM.txt", header = TRUE, sep = ",")
data <- read_xlsx("./9oct/data/GAM.xlsx")

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3
x4 <- data$x4

m <- lm(y ~ x1+x2+x3+x4)
sm <- summary(m)

#DWtest
dw <- dwtest(m)


#BGtest
bg <- bgtest(m)
bgF <- bgtest(m, type = "F")

#GQtest
gq <- gqtest(m, order.by = x1+x2+x3+x4, fraction = 0)

#BPtest
bp <- bptest(m)

#White_test
wt <- bptest(m, varformula = ~(x1+x2+x3+x4) + I((x1+x2+x3+x4) ^ 2), studentize = TRUE)

# install.packages("sandwich")
library(sandwich)
HC <- vcovHC(m)

HAC <- vcovHAC(m)


#ДВМНК
y2 <- y / x1+x2+x3+x4
x2 <- 1/(x1+x2+x3+x4)


m2 <- lm(y2 ~ x2)
s2 <- summary(m2)


#устранение автокорреляции, если известно значение р
DW <- dw$statistic
p <- 1-DW/2
# y4 <-

