source("arima.R")

okno <- 365

a <- selfarima.simple(data2, oo = okno, nr = 8, n = 52)
a.log <- selfarima.simple(data2, oo = okno, nr = 8, n = 52, log = TRUE)# selfarima.autoplot(a, 7)

cft(a.log, data2)

a[[4]]$x
a.log[[2]]$fitted

tsdisplay(residuals(a[[2]]), lag.max = okno)
tsdisplay(residuals(a.log[[2]]), lag.max = okno)
sqrt(diag(a.log[[2]]$var.coef))

mean(abs(a[[4]]$mean - a.log[[4]]$mean))

selfarima.autoplot(a)
selfarima.autoplot(a.log)



max <- dim(data2)[1]
windo <- ((max - 50):max)
plot(data2[windo, 7], type = "l")
plot(log(data2[((max - 50):max), 7]), type = "l")


startW <- as.numeric(strftime(head(data2$data[windo], 1), format = "%W"))
startD <- as.numeric(strftime(head(data2$data[windo], 1) + 1, format =" %w")) 
ts2 <- ts(log(data2[windo, 7]), start = c(startW, startD), frequency = 7)

auto.arima(ts2, seasonal = TRUE, approximation = FALSE, stepwise = FALSE )
