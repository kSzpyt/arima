#periodogram
#średnia na każdy dzień tygodnia
library(forecast)
library(ggplot2)

inds <- seq(as.Date("2010-01-01"), as.Date("2012-12-31"), by = "day")
aaa <- c(2010, as.numeric(format(inds[1], "%j")))

ts1 <- ts(data2$`88209_v1`[(max - 356):max], start = aaa, frequency = 7)
ts2 <- ts(data2$`88212_v`, start = aaa, frequency = 7)

decom <- stl(ts1, s.window = "periodic")
plot(decom)

decom2 <- stl(ts2, s.window = "periodic")
plot(decom)

adjdecom <- seasadj(decom)
# lines(adjdecom, col = "red")

# 
# ts2 <- ts(adjdecom, start = aaa, frequency = 12)
# plot(stl(ts2, s.window = "periodic"))

model  <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) #, approximation = FALSE, stepwise = FALSE
model2 <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE)

tsdisplay(residuals(model2), lag.max=750)
tsdisplay(residuals(model), lag.max = 750)

fcast <- forecast(model, h = 7)
plot(fcast)

fcast2 <- forecast(model, h = 52)
plot(fcast2)

# fcast2 <- forecast(model, h = 365)


autoplot(ts1, series = "Data") +
  autolayer(model$fitted, series = "Arima") +
  autolayer(fcast, series = "prediction") +
  xlab("Week") + ylab("Value") + ggtitle("xyz") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")

# autoplot(ts1, series = "Data") +
#   autolayer(model$fitted, series = "Arima") +
#   autolayer(fcast2, series = "prediction") +
#   xlab("Day") + ylab("Value") + ggtitle("xyz2") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")





fit <- auto.arima(ts1, xreg = fourier(ts1, K = 3), lambda = 0)
aaa <- as.matrix(dm[, 2:3])
colnames(aaa) <- c("D0", "D1")
fit <- auto.arima(ts1, lambda = 0, xreg=aaa, stepwise=FALSE, approximation = FALSE)

autoplot(forecast(fit, xreg = fourier(ts1, K = 3, h = 240)))+
  xlab(paste("K = 3","   AICC=",round(fit[["aicc"]],2))) +
  ylab("") 
