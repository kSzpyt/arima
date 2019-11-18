data2 <- data.zeros(data)

# ddd <- dummies(data2)
# #robimy dummy variable takie że są 0-1 dla dni normalnych oraz cbind z dniami specjlanymi (1-5)
# #pon wt sr czw pt sb nd ds1 ds2 ds3 ds4 ds5
# 
dns <- dummies(data2, type = "dns")
ds <- dummies(data2, type = "ds")


c <- selfarima2(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, 
                result.save = FALSE, i = 1, type = "simple", n = 14, seas = TRUE, log = TRUE)
# c$pp
# df.xl.write2(c, data2, typ = "sez_ds", i = 1)

d <- selfarima2(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(ds), n = 14, seas = TRUE, log = TRUE)
# d$pp
# df.xl.write2(d, data2, typ = "sez_ds", i = 2)

e <- selfarima2(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(dns), n = 14, seas = FALSE, log = TRUE)
# e$pp
# df.xl.write2(e, data2, typ = "sez_ds", i = 3)

plot(data$`88209_v`[c$pred.wind], type = "l", main = "log")
lines(c$fcast, col = "red")
lines(d$fcast, col = "blue")
lines(e$fcast, col = "green")






a <- selfarima2(data2, start.date = "2011-03-01", end.date = "2012-02-29", nr = 8, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(d), n = 14, seas = TRUE)
a$pp
df.xl.write2(a, data2, typ = "sez_ds", i = 1)
a$pred.trend

dns <- dummies(data2, type = "dns")
ds <- dummies(data2, type = "ds")


a.log <- selfarima2(data2, start.date = "2011-04-01", end.date = "2012-03-30", nr = 8, 
                result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(ds), n = 14, seas = TRUE, log = TRUE)

a.log$fcast
a.log$pp
df.xl.write2(a.log, data2, typ = "sez_ds", i = 1, log = TRUE)

plot(data$`88209_k`[a.log$pred.wind], type = "l")
lines(a.log$fcast, col = "red")

