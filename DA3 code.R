### csv style 
library(readr)
library(xts)
library(csv)

data = read.csv("Documents/JMU Fall 19/ECON 483/Data Assignments/DJI.csv", header = T)
data = na.omit(data)
data$time =strptime(data$time, format = "%m/%d/%Y")
yl = xts(data$Adj.Close, order.by = data[, "time"])
View(yl)
plot.xts(yl)

library(urca)

yl=diff(yl)
yl=na.omit(yl)
acf(yl)


#data = read_csv("Documents/JMU Fall 19/ECON 483/Data Assignments/DJI.csv", header = T)
#y = ts(DJI$`Adj Close`, start =c(2011,06,01))
#yl = log(lag(y))-log(y)


library(quantmod)
#s ='2011-06-01'
#e = '2019-11-12' 
#dji_c <- getSymbols.yahoo("DJI", from = s, to = e, auto.assign=F)
#dji_a = getSymbols.yahoo("DJI", from = s, to = e, auto.assign=F)[,6]
#dji <- dailyReturn(dji_a, type = 'log')


library(urca)
x = ur.df(yl, type= 'drift', lags = 10, selectlags = "AIC")
summary(x)

yl=diff(yl)
yl=na.omit(yl)

x1=ur.df(yl,type="drift", lags=10, selectlags ="AIC")
summary(x1)

n = ur.df(diff(yl),type="drift", lags =10, selectlags ="AIC")
summary(n)

suppressMessages(library(xts))

pars = expand.grid(ar = 1:6, diff = 1, ma = 1:6)


aic <- rep(0, nrow(pars))
bic <- rep(0, nrow(pars))
for (i in seq(along = bic)) {
  aic[i] = AIC(arima(yl, unlist(pars[i, 1:3]),optim.control = list(maxit=1000))) 
  bic[i] = AIC(arima(yl, unlist(pars[i, 1:3]),optim.control = list(maxit=1000)), 
               k = log(length(dji)))
}
p = seq(1,6,1)
q = seq(1,6,1)
group = expand.grid(p=p, q=q)
cbind(group, aic, bic)

## the table shows AIC optimal structure is p = 

library(forecast)
yl = xts(data$Adj.Close, order.by = data[,'time'])

f = arima(yl, order=c(1,1,1), include.mean  = T)
summary(f)


library(lmtest)
### obtain residuals as lm object for leter use in bgtest
fe = lm(residuals(f) ~ 1) 
a=rep(0,12)
b=rep(0,12)

bptest(fe)
### look for serial correlation from order 1 to 12 
for (p in seq(along = rep(1,12))) { 
  a[p]=bgtest(fe,order=p)$statistic
  b[p]=bgtest(fe,order=p)$p.value
}

results=data.frame(cbind(1:12, a,b)) 

results

#### forcast

yf=forecast(f, h=3) 
yf
plot(yf, include=30, xlab = " Time", ylab="log-returns",  main = "DOW JONES forecast", )

accuracy(yf)


