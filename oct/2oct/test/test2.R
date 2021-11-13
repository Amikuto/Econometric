library(lmtest)
require(readxl)

# data <- read.table('./2oct/data/data.txt', dec=',', header=TRUE)
data <- read_xlsx("./2oct/test/data2.xlsx")
y <- data$y
x <- data$x

X <- log10(x)
Y <- log10(y)

m <- lm(y~x, data = data)
m

sm <- summary(m)
sm