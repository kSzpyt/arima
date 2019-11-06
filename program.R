source("arima.R")

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
