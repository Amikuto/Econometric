library(lmtest)
require(readxl)

data <- read.table('./9oct/hw/Kurtaev.txt')

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3

sumx <- (x1 + x2 + x3)


#1

correl <- cor(y ~ x1 ~ x2 ~ x3)