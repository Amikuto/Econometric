library(lmtest)
library(readxl)

data <- read_xlsx("./dec/18.12/data.xlsx")

data_learning <- data[1:13, ]
data_control <- data[14:14, ]

m <- lm(data_learning)
sm <- summary(m); sm

#DWtest
dw <- dwtest(m);dw
# pv>a => H0 принимается, автокорреляция первого порядка отсутствует

#BGtest
bg <- bgtest(m,order = 1);bg
# pv>a => H0 принимается, автокорреляция первого порядка отсутствует

#GQtest
gq <- gqtest(m);gq
# pv<a => присутствует проблема гетероскедастичности

#BPtest
bp <- bptest(m);bp
# pv<a => присутствует проблема гетероскедастичности

short_multi <- sm$coefficients[2:4, 1]
long_multi <- sum(short_multi)

vklad <- short_multi / long_multi
lag <- (vklad[[1]] * 1) + (vklad[[2]] * 2)

lm_short <- lm('yi ~ x3', data=data_learning)
waldtest(lm_short, m, test = "F")

#Среднее
predict(m, newdata = data_control, interval = "confidence")
#Индивидуально
predict(m, newdata = data_control, interval = "prediction")

