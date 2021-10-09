library(lmtest)

setwd('/Users/elena/Desktop/')
data = read.table('AG.txt', dec=',', header=TRUE)
data

m = lm(S~Yd, data = data)
sm = summary(m);sm
e=sm$residuals;e

#DWtest
dw = dwtest(m);dw
# pv>a => H0 принимается, автокорреляция первого порядка отсутствует

#BGtest
bg = bgtest(m,order = 1);bg
bgF = bgtest(m,order = 1,type = "F");bgF
# pv>a => H0 принимается, автокорреляция первого порядка отсутствует

#GQtest
gq = gqtest(m, order.by= ~Yd, fraction=0, data=data);gq
# pv<a => присутствует проблема гетероскедастичности

#BPtest
bp = bptest(m, data=data);bp
# pv<a => присутствует проблема гетероскедастичности

#White_test
wt = bptest(m, data=data, varformula=~Yd + I(Yd^2), studentize=TRUE);wt

install.packages("sandwich")
library(sandwich)
HC<-vcovHC(m)
HC

HAC<-vcovHAC(m)
HAC


#ДВМНК
y<-data$S
y

x<-data$Yd
x

y2<-y/x
y2

x2<-1/x
x2

m2<-lm(y2~x2)
s2<-summary(m2)
s2

#ДВМНК 2 вариант
y3<-y/predict(m)
x3<-x/predict(m)
m3<-lm(y3~x3)
s3<-summary(m3)
m31<-lm(y3~0+x3)
s31<-summary(m31)


#устранение автокорреляции, если известно значение р
DW<-dw$statistic
DW
p<-1-DW/2
y4<-y[2:16]-p*y[1:15]
y4
x4<-x[2:16]-p*x[1:15]
x4
m4<-lm(y4~x4);m4
s4<-summary(m4);s4
b<-m4$coefficients[2];b
a<-m4$coefficients[1]/(1-p);a
