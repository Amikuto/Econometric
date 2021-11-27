library(lmtest)
require(readxl)

data <- read_xlsx('./nov/6.11 Куртаев/стр72/data_page72.xlsx')
data <- data[order(data$Y), ]

y <- data$Y
x <- data$X

m <- lm(data$Y ~ data$X, data = data)
sm <- summary(m);sm
e <- sm$residuals;e
bp <- bptest(m, data=data);bp


YN <- y / x
XN <- 1/x
fmn <- lm(YN~XN)
sfmn <- summary(fmn); sfmn
bp2 <- bptest(fmn, data=data);bp2


Y2 <- log(y)
X2 <- log(x)
fm2 <- lm(Y2 ~ X2)
sfm2 <- summary(fm2); sfm2
bp3 <- bptest(sfm2, data=data);bp3

vcov(m)



#Задание 11
ESS1 <- (1-0.531)*5.721
ESS2 <- (1-0.985)*26572.9

F <- ESS2/ESS1
Fcrit <- 2.942957268

F>Fcrit


#Задание 12
RSS <- 50585.66
ESS <- 11726.42

TSS <- RSS + ESS
R2 <- RSS / TSS

n <- 34
F <- R2 * n

Fcrit <- qf(p=.975, df1=3, df2=4)
F > Fcrit



