library(forecast)
library(ggplot2)
library(tseries)
inds <- seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "day")
aaa <- c(2010, as.numeric(format(inds[1], "%j")))

ts1 <- ts(data2$`88209_v`, start = inds[1], frequency = 7)
ts2 <- ts(data2$`88209_k`[(max -365):max], start = aaa, frequency = 7)
ts3 <- ts(data2$`88209_s`, start = aaa, frequency = 7)

model1 <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
model2 <- auto.arima(ts2, seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
model2.2 <- Arima(ts2, c(0, 0, 4), c(1, 0 , 0))
model3 <- auto.arima(ts3, seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
model3.2 <- Arima(ts3, c(2, 0, 0), c(1, 0 , 0))


str(model1)
arimaorder(model1)
  
  
  
tsdisplay(residuals(model1), lag.max=750)
tsdisplay(residuals(model2), lag.max = 750)
tsdisplay(residuals(model2.2), lag.max = 750)
tsdisplay(residuals(model2), lag.max = 750)

fcast1 <- forecast(model1, h = 52)
plot(fcast1)

fcast2 <- forecast(model2, h = 52)
plot(fcast2)

fcast2.2 <- forecast(model2.2, h = 52)
plot(fcast2.2)

fcast3 <- forecast(model3, h = 52)
plot(fcast3)

fcast3.2 <- forecast(model3.2, h = 52)
plot(fcast3.2) #arima(2, 0, 0)

autoplot(ts1, series = "Data") +
  autolayer(model1$fitted, series = "Arima") +
  autolayer(fcast1, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

autoplot(ts2, series = "Data") +
  autolayer(model2$fitted, series = "Arima") +
  autolayer(fcast2, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

autoplot(ts3, series = "Data") +
  autolayer(model3$fitted, series = "Arima") +
  autolayer(fcast3, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")


# aaa <- as.matrix(dm[, 2:3])
# colnames(aaa) <- c("D0", "D1")
windo <- (max -20):max

startW <- as.numeric(strftime(head(data2$data[windo], 1), format = "%W"))
startD <- as.numeric(strftime(head(data2$data[windo], 1) + 1, format =" %w")) 
ts2 <- ts(data2$`88209_v`[windo], start = c(startW, startD), frequency = 7)


model.xreg <- auto.arima(ts2, xreg=d[windo, ], stepwise=FALSE, approximation = FALSE)
# tsdisplay(residuals(model.xreg), lag.max=750)
fcast.xreg <- forecast(model.xreg, xreg = d[(max -20):max, ], h = 600)
plot(fcast.xreg)
autoplot(ts2, series = "Data") +
  autolayer(model.xreg$fitted, series = "Arima") +
  autolayer(fcast.xreg, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")










fit <- lm(data2$`88209_v` ~ data2$data)
res <- fit$residuals
ts.res <- ts(res[(max - 50):max], start = aaa, frequency = 7)
model.res <- auto.arima(ts.res, seasonal = TRUE, approximation = FALSE, stepwise=FALSE)

tsdisplay(residuals(model.res), lag.max=750)

fcast.res <- forecast(model.res, h = 52)

plot(fcast.res)

autoplot(ts.res, series = "Data") +
  autolayer(model.res$fitted, series = "Arima") +
  autolayer(fcast.res, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")




startW <- as.numeric(strftime(head(data2$data, 1), format = "%W"))
startD <- as.numeric(strftime(head(data2$data, 1) + 1, format =" %w")) 
wind <- (dim(data2)[1] - 50):(dim(data2)[1])
ts1 <- ts(data2[wind, 6], start = c(startW, startD), frequency = 7)  
model <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 

fcast1 <- forecast(model1, h = h)

pp <- autoplot(ts1, series = "Data") +
  autolayer(model$fitted, series = "Arima") +
  autolayer(fcast1, series = "Prediction") +
  xlab("Week") + ylab("Value") + 
  ggtitle(as.character(names(data.row))) + 
  theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")
