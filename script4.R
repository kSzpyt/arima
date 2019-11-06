inds <- seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "day")
aaa <- c(2010, as.numeric(format(inds[1], "%j")))

ts1 <- ts(data2$`88209_v`[(dim(data2)[1] - 50):dim(data2)[1]], start = aaa, frequency = 7)
ts2 <- ts(data2$`88209_k`, start = aaa, frequency = 7)
ts3 <- ts(data2$`88209_s`, start = aaa, frequency = 7)

arima(ts2, order = c(1, 1, 0), seasonal = c(1, 1, 0))

model111 <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
tsdisplay(residuals(model111), lag.max=750)

fcast111 <- forecast(model111, h = 52)
plot(fcast111)

autoplot(ts1, series = "Data") +
  autolayer(model111$fitted, series = "Arima") +
  autolayer(fcast111, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")



#paczki do progrnozowania atm

model.xreg <- auto.arima(ts1, xreg = m, approximation = FALSE, stepwise = FALSE)

fcast.xreg <- forecast(model3, h = 52)
plot(fcast.xreg)

summary(data2[, 6])
summary(data2[, 7])
summary(data2[, 8])
