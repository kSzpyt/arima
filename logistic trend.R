library(car)
logistic.trend <- function(data, nr = 7, wind = NULL, n = 7)
{
  if(is.null(wind)) wind <- data$key
  
  dat <- as.numeric(data[wind, nr])
  coefs <- coef(lm(logit(dat/max(dat)) ~ wind))
  
  model <- nlsLM(dat ~ phi1/(1+exp(-(phi2+phi3*wind))), 
              start = list(phi1 = max(dat), phi2 = coefs[1], phi3 = coefs[2]), data = data, trace=TRUE)
  
  phi1 <- coef(model)[1]
  phi2 <- coef(model)[2]
  phi3 <- coef(model)[3]
  
  trend <- phi1/(1 + exp(-(phi2 + phi3 * wind))) 
  
  res <- data.frame(x = wind, fit = trend)
  
  len <- wind[length(wind)] + 1
  pred.trend <- phi1/(1 + exp(-(phi2 + phi3 * (len:(len+n-1) ))))
  
  return(list(res, dat, pred.trend))
}

plot.logistic.trend <- function(logistic.list)
{
  dat <- logistic.list[[2]]
  predict <- logistic.list[[1]]
  
  cbind(data.frame(data = dat), predict) %>%
    ggplot(aes(x = x)) + 
    geom_line(aes(y = data), color = 'blue') + 
    geom_line(aes(y = fit), color = 'red', size = 2)
}


t <- logistic.trend(data2, nr = 7, n = 52)
plot.logistic.trend(a[[6]])

xd <- t[[3]] + a[[4]]

start.date <- as.POSIXct(start.date, tz = "UTC")
end.date <- as.POSIXct(end.date, tz = "UTC")
wind <- which(data$data == start.date):which(data$data == end.date)
ex <- which(!is.na(data[wind, nr]))
wind <- wind[ex]

startW <- as.numeric(strftime(head(data$data, 1), format = "%W"))
startD <- as.numeric(strftime(head(data$data, 1) + 1, format =" %w")) 

lt <- logistic.trend(data = data2, nr = 7, n = 52, wind = (731:1096))
trend <- lt[[1]][, 2]
res <- abs(data$`88209_v`[731:1096] - trend)
ts1 <- ts(res, start = c(startW, startD), frequency = 7)

fitt <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
fc <- forecast(fitt, h = 52)

fc$mean

plot(fc$mean, type = "l")
