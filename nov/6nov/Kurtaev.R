library(lmtest)
require(readxl)

data <- read_xlsx('./nov/6nov/data.xlsx')

m <- lm(data$Y ~ data$X, data = data)
sm <- summary(m);sm
e <- sm$residuals;e


bp <- bptest(m, data=data);bp