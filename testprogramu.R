source("arima.R")

a <- selfarima(data2, oo = 50, nr = 7, n = 52)
a[[5]]

selfarima(data2, oo = 50, nr = 7,  n = 52, type = "xreg", xr = d)
b[[5]]

selfarima(data2, oo = 50, nr = 7, n = 52, mnk = TRUE)
c[[5]]

selfarima(data2, oo = 50, nr = 7, n = 52, type = "xreg", mnk = TRUE, xr = d)
