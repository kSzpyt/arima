selfarima2 <- function(data, start.date = "2010-01-01", end.date = "2012-12-31", nr = 7, n = 7, log = FALSE,
                       type = "simple", xr = NULL, result.save = TRUE, typ = NULL, i, seas = TRUE, data.nas)
{
  ###################################################################################
  start.date <- as.POSIXct(start.date, tz = "UTC", format = c("%Y-%m-%d"))
  end.date <- as.POSIXct(end.date, tz = "UTC", format = c("%Y-%m-%d"))
  wind <- which(data$data == start.date):which(data$data == end.date)
  ex <- which(0 != data[wind, nr])
  wind <- wind[ex]
  
  startW <- as.numeric(strftime(head(data$data[wind], 1), format = "%W"))
  startD <- as.numeric(strftime(head(data$data[wind], 1) + 1, format =" %w"))
  
  dat <- data[wind, nr]
  
  if(log == TRUE)
  {
    dat <- log(dat)
  }
  ###################################################################################
  lt <- logistic.trend(data = data, nr = nr, wind = wind, n = n, log = log)
  trend <- lt$trend
  res <- dat - trend
  ts1 <- ts(res, start = c(startW, startD), frequency = 7)
  ###################################################################################
  if(type == "simple")
  {
    model <- auto.arima(ts1, seasonal = seas, approximation = FALSE, stepwise = FALSE) 
    fcast.res <- forecast(model, h = n)
  }
  else if (type == "xreg")
  {
    model <- auto.arima(ts1, xreg = xr[wind, ], seasonal = seas, stepwise=FALSE, approximation = FALSE)
    fcast.res <- forecast(model, xreg = xr[(wind[length(wind)] + 1):(wind[length(wind)] + n), ])
  }
  ###################################################################################
  if(log == TRUE)
  {
    # fcast.res$mean <- exp(fcast.res$mean)
    # fcast.res$upper <- exp(fcast$upper)
    # fcast.res$lower <- exp(fcast$lower)
    # fcast.res$x <- exp(fcast$x)
    # model$fitted <- exp(model$fitted)
    # dat <- exp(dat)
    # trend <- exp(trend)
    # lt$pred.trend <- exp(lt$pred.trend)
  }
  ###################################################################################
  fcast <- lt$pred.trend + as.numeric(fcast.res$mean)
  if(log == TRUE)
  {
    fcast <- exp(fcast)
  }
  
  ddff <- data.frame(x = c(data[wind, nr], fcast), y = c(rep("a", length(wind)), rep("b", length(fcast))))
  
  pp <- xyplot(x ~ 1:length(x), data = ddff, group = y, type = "b", col = c("black", "red"))
  ###################################################################################
  pred.trend <- lt$pred.trend
  li <- list(res = res, fcast.res = fcast.res$mean, 
             dat= dat, fcast = fcast, 
             nr = nr, model = model, pp = pp, 
             wind = wind, pred.wind = (wind[length(wind)]+1):(wind[length(wind)]+n),
             trend = trend, pred.trend = pred.trend)
  
  if(result.save == TRUE)
  {
    df.xl.write2(li, data, typ= typ, i = i, log = log, data.nas = data.nas)
    
  }
  else
  {
    return(li)
  }
}
