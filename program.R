source("arima.R")

#trend logistyczny, a liniowy
#+błędy dobrze dopasowac z uzwgl trednu (mnk)
#excel
data2 <- data.zeros(data)
d <- dummies(data2)

wym <- dim(data2)
okno <- 50
n.pred <- 52

s <- seq(7, dim(data2)[2], by = 6)

s <- sort(c(s, s+1, s+2))

for(x in s) 
{
  selfarima(data2, oo = okno, nr = x, n = n.pred)
  selfarima(data2, oo = okno, nr = x, n = n.pred, type = "xreg", xr = d)
  selfarima(data2, oo = okno, nr = x, n = n.pred, mnk = TRUE)
  selfarima(data2, oo = okno, nr = x, n = n.pred, type = "xreg", mnk = TRUE, xr = d)
  
  selfarima(data2, oo = okno, nr = x, n = n.pred, log = TRUE)
  selfarima(data2, oo = okno, nr = x, n = n.pred, type = "xreg", xr = d, log = TRUE)
  selfarima(data2, oo = okno, nr = x, n = n.pred, mnk = TRUE, log = TRUE)
  selfarima(data2, oo = okno, nr = x, n = n.pred, type = "xreg", mnk = TRUE, xr = d, log = TRUE)
}

#xreg NA dla zwykłego dnia do naprawienia

a <- selfarima(data2, oo = 50, nr = 15, n = 52, result.save = FALSE)
cft(a, data2)

a <- selfarima(data2, oo = 50, nr = 8, n = 50, type = "xreg", xr = d, result.save = FALSE)
plot(data2$`88209_v`, type = 'l')


data2$data[1]
str(data2$data[1])

which(as.POSIXct("2012-01-02", tz = "UTC") == data2$data)

library(car)

coef(lm(logit(data2$`88209_v`/100)~data2$key))
wilson<-nls(data2$`88209_v`~phi1/(1+exp(-(phi2+phi3*data2$key))), 
            start=list(phi1=100, phi2=-5.0494311096, phi3=0.0004155473), data = data2, trace=TRUE)
summary(wilson)








