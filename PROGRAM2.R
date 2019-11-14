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
