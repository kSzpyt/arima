library(forecast)
library(ggplot2)
library(tseries)
inds <- seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "day")
aaa <- c(2010, as.numeric(format(inds[1], "%j")))

ts4 <- ts(data2$`88212_v`, start = aaa, frequency = 7)
ts5 <- ts(data2$`88212_k`, start = aaa, frequency = 7)
ts6 <- ts(data2$`88212_s`, start = aaa, frequency = 7)

model4 <- auto.arima(ts4, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
model5 <- auto.arima(ts5, seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
# model5.2 <- Arima(ts5, c(0, 0, 4), c(1, 0 , 0))
model56 <- auto.arima(ts6, seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
# model56.2 <- Arima(ts6, c(2, 0, 0), c(1, 0 , 0))


tsdisplay(residuals(model4), lag.max=750)
tsdisplay(residuals(model5), lag.max = 750)
# tsdisplay(residuals(model5.2), lag.max = 750)
tsdisplay(residuals(model5), lag.max = 750)

fcast4 <- forecast(model4, h = 52)
plot(fcast4)

fcast5 <- forecast(model5, h = 52)
plot(fcast5)

# fcast5.2 <- forecast(model5.2, h = 52)
# plot(fcast5.2)

fcast6 <- forecast(model56, h = 52)
plot(fcast6)

# fcast6.2 <- forecast(model56.2, h = 52)
# plot(fcast6.2) #arima(2, 0, 0)

autoplot(ts4, series = "Data") +
  autolayer(model4$fitted, series = "Arima") +
  autolayer(fcast4, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

autoplot(ts5, series = "Data") +
  autolayer(model5$fitted, series = "Arima") +
  autolayer(fcast5, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

autoplot(ts6, series = "Data") +
  autolayer(model56$fitted, series = "Arima") +
  autolayer(fcast6, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")


# aaa <- as.matrix(dm[, 2:3])
# colnames(aaa) <- c("D0", "D1")
model.xreg <- auto.arima(ts4, xreg=m, stepwise=FALSE, approximation = FALSE)
tsdisplay(residuals(model.xreg), lag.max=750)
fcast.xreg <- forecast(model.xreg, xreg = m, h = 52)
plot(fcast.xreg)
autoplot(ts4, series = "Data") +
  autolayer(model.xreg$fitted, series = "Arima") +
  autolayer(fcast.xreg, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

fit <- lm(data2$`88209_v` ~ data2$data)
res <- fit$residuals
ts.res <- ts(res, start = aaa, frequency = 7)
model.res <- auto.arima(ts.res, seasonal = FALSE, approximation = FALSE, stepwise=FALSE)

tsdisplay(residuals(model.res), lag.max=750)

fcast.res <- forecast(model.res, h = 52)

plot(fcast.res)

autoplot(ts.res, series = "Data") +
  autolayer(model.res$fitted, series = "Arima") +
  autolayer(fcast.res, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")
