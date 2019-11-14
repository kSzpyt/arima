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

coef(lm(logit(data2$`88209_v`/204)~data2$key))
wilson<-nls(data2$`88209_v`~phi1/(1+exp(-(phi2+phi3*data2$key))), 
            start=list(phi1=204, phi2=-0.6723832825, phi3=0.0006990561), data = data2, trace=TRUE)
summary(wilson)



trend <- 1.024e+02/(1+exp(-(3.533e-01+3.618e-03*data2$key)))

plot(trend, type = "l")
lines(data2[, 9], col = "red")

mean(abs(trend - data2$`88209_v`))
plot(abs(trend - data2$`88209_v`), type = "l")


phi1<-coef(wilson)[1]
phi2<-coef(wilson)[2]
phi3<-coef(wilson)[3]
y<-phi1/(1+exp(-(phi2+phi3*data2$key))) #predicted mass
predict<-data.frame(x = data2$key,y = y)

data2 %>%
  ggplot(aes(x=data2$key,y=data2$`88209_v`))+
  geom_line(color='blue')+theme_bw()+
  geom_line(data=predict,aes(x=x,y=y), size=2)









a <- selfarima(data2, nr = 7, n = 52, mnk = TRUE, result.save = FALSE, start.date = "2012-01-01", end.date = "2012-12-31")

a[[5]]
a[[4]]
plot(a[[4]], type = "l")


plot.logistic.trend(a[[6]])
a[[6]]





a$pp

infcrit <- t(data.frame(a$model$aic, a$model$aicc, a$model$bic))

errors.res.fitted <- t(as.data.frame(err(a$model$fitted, a$res)))

errors.predicted <- t(as.data.frame(err(a$dat, a$fcast)))

ct <- cft(a$model, data2)

a$model$coef <- a$model$coef[complete.cases(ct)]
ct <- ct[complete.cases(ct), ]

coef.pval <- data.frame(a$model$coef, ct[, 2])
colnames(coef.pval) <- c(paste0("coefs_", "nam"), paste0("p.val_)", "nam"))

df.rest <- data.frame(rep(0, 10), rep(0, 10))
df.rest[5:10, 1] <- 9
df2 <- data.frame(4:6)
df.rest[8:10, 2] <- df2
df.rest[]
df.rest <- data.frame(as.numeric(a$res), as.numeric(a$model$fitted))
colnames(df.real) <- c(paste0("real_", nam), paste0("fitted_", nam))


df.rest.pred <- data.frame(rep(0, dim(data2)[1]), rep(0, dim(data2)[1]))
df.rest.pred[a$wind, ] <- data.frame(as.numeric(a$res), as.numeric(a$model$fitted))
df.rest.pred[foo.list$pred.wind, ] <- data.frame(as.numeric(foo.list$res), as.numeric(foo.list$model$fitted))
colnames(df.real) <- c(paste0("real_", nam), paste0("fitted_", nam))

df.whole <- data.frame(as.numeric(a$dat), as.numeric(a$model$fitted+a$trend))
colnames(df.rest) <- c(paste0("real_", nam), paste0("fitted_", nam))#tu dopisać nazwę zmiennej

a <- selfarima2(data2, start.date = "2011-01-01", end.date = "2012-01-01", nr = 7, result.save = FALSE, i = 1)
b <- selfarima2(data2, start.date = "2011-01-01", end.date = "2012-01-01", nr = 8, result.save = FALSE, i = 1)
a$pp

df.xl.write2(a, data2, typ = "test", i = 1)
df.xl.write2(b, data2, typ = "test", i = 2)

b.xreg <- selfarima2(data2, start.date = "2010-01-01", end.date = "2011-12-31", nr = 7, 
                     result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(ddd), n = 14, seas = FALSE)
b.xreg$pp
df.xl.write2(b.xreg, data2, typ = "test_xreg", i = 1)

c <- selfarima2(data2, start.date = "2010-01-01", end.date = "2011-12-31", nr = 7, 
                     result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE)
c$pp
df.xl.write2(c, data2, typ = "sez_ds", i = 1)


b.xreg <- selfarima2(data2, start.date = "2011-01-01", end.date = "2012-01-01", nr = 7, result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(), n = 52)
df.xl.write2(a, data2, typ = "test_xreg", i = 1)


c.xreg <- selfarima2(data2, start.date = "2011-01-01", end.date = "2012-01-01", nr = 7, result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 52)
c.xreg$pp
df.xl.write2(c.xreg, data2, typ = "test_xreg", i = 2)
