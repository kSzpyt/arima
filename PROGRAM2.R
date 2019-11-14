data2 <- data.zeros(data)

# ddd <- dummies(data2)
# #robimy dummy variable takie że są 0-1 dla dni normalnych oraz cbind z dniami specjlanymi (1-5)
# #pon wt sr czw pt sb nd ds1 ds2 ds3 ds4 ds5
# 
d <- as.factor(data2$dni_specjalne)
d <- dummy_cols(d)
d <- d[, -c(1:2)]


c <- selfarima2(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE)
c$pp
df.xl.write2(c, data2, typ = "sez_ds", i = 1)

d <- selfarima2(data2, start.date = "2011-03-01", end.date = "2012-02-29", nr = 7, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE)
d$pp
df.xl.write2(d, data2, typ = "sez_ds", i = 2)

e <- selfarima2(data2, start.date = "2011-03-01", end.date = "2012-02-29", nr = 8, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE)
e$pp
df.xl.write2(e, data2, typ = "sez_ds", i = 3)


a <- selfarima2(data2, start.date = "2011-03-01", end.date = "2012-02-29", nr = 8, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE)
a$pp
df.xl.write2(a, data2, typ = "sez_ds", i = 1)
a$pred.trend


a.log <- selfarima2(data2, start.date = "2011-09-01", end.date = "2012-02-29", nr = 8, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE, log = TRUE)

a.log$fcast
df.xl.write2(a.log, data2, typ = "sez_ds", i = 1)
