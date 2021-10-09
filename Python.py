import os
os.chdir('/Users/elena/Desktop/')
import numpy as np
import pandas as pd
d = pd.read_csv('AG.csv', sep = ';', decimal = ',')
d
import statsmodels.formula.api as sm
m = sm.ols(formula='S ~ Yd', data=d)
results = m.fit()
s = results.summary()
s

import statsmodels.stats as smd

import statsmodels.stats.api as sms
#чтобы делать тесты

from statsmodels.compat import lzip
#чтобы делать подписи

residuals = results.resid

#DWtest
dw = smd.stattools.durbin_watson(residuals)
dw

#BGtest
smd.diagnostic.acorr_breusch_godfrey(results)
#или
name=['Chisq statistic','p-value', 'F statistic','p-value(F)']
bg=sms.acorr_breusch_godfrey(results,nlags=1)
lzip(name,bg)

#GQtest
name=['F statistic','p-value']
gq=sms.het_goldfeldquandt(residuals,results.model.exog)
lzip(name,gq)
#или GQtest
gq=sms.het_goldfeldquandt(residuals,results.model.exog)
gq

#BPtest
name=['BP','p-value']
bp=sms.het_breuschpagan(residuals,results.model.exog)
lzip(name,bp)

#White_test
name=['WT','p-value']
wt=sms.het_white(residuals,results.model.exog)
lzip(name,wt)

#HC
smd.sandwich_covariance.cov_hc3(results)
#или
results.cov_HC3

#HAC
smd.sandwich_covariance.cov_hac(results)

#ДВМНК
Yd2=1/d.Yd
Yd2
S2=d.S/d.Yd
S2
import statsmodels.api as st
#добавим в наш массив экзогенной переменной Yd столбец х0, который дает нам константу в модели
Yd3 = st.add_constant(Yd2)
m2 = st.OLS(S2, Yd3)
results2 = m2.fit()
s2 = results2.summary()
s2

#ДВМНК 2 вариант
Ypred=results.fittedvalues
Yd4=d.Yd/Ypred
Yd4
S4=d.S/Ypred
S4
Ypred
Yd5 = st.add_constant(Yd4)
m3 = st.OLS(S4, Yd5)
results3 = m3.fit()
s3 = results3.summary()
s3
#низкое качество модели, перестроим модель без константы
m31 = st.OLS(S4, Yd4)
results31 = m31.fit()
s31 = results31.summary()
s31

#устранение автокорреляции, если известно значение р 
#находим примерное значение p из статистики DW или из вспомогательной модели регрессии: e_t^=p*e_(t-1)
p=1-dw/2
p
#проводим замену переменных
St=np.array(d.S[1:16])
Sp=np.array(d.S[0:15])
S6=list(St-p*Sp)
S6
Yd6=list(np.array(d.Yd[1:16])-p*np.array(d.Yd[0:15]))
Yd6
Yd7 = st.add_constant(Yd6)
m4 = st.OLS(S6, Yd7)
results4 = m4.fit()
s4 = results4.summary()
s4
b=results4.params[1]
b
a=results4.params[0]/(1-p)
a
print(f'Модель имеет вид: S^={a}+{b}*Yd')