library(forecast)
library(openxlsx)
library(plyr)
library(dplyr)
library(lattice)
source("testy.R")
source("errors.R")
source("script1.R")
source("writedata.R")

selfarima2 <- function(data, start.date = "2010-01-01", end.date = "2012-12-31", nr = 7, n = 7, log = FALSE,
                      type = "simple", xr = NULL, result.save = TRUE, typ = "AAA", i)
{
  ###################################################################################
  start.date <- as.POSIXct(start.date, tz = "UTC", format = c("%Y-%m-%d"))
  end.date <- as.POSIXct(end.date, tz = "UTC", format = c("%Y-%m-%d"))
  wind <- which(data$data == start.date):which(data$data == end.date)
  ex <- which(!is.na(data[wind, nr]))
  wind <- wind[ex]
  
  startW <- as.numeric(strftime(head(data$data[wind], 1), format = "%W"))
  startD <- as.numeric(strftime(head(data$data[wind], 1) + 1, format =" %w")) 
  
  dat <- data[wind, nr]
  if(log == TRUE)
  {
    dat <- log(dat)
  }
  ###################################################################################
  lt <- logistic.trend(data = data, nr = nr, wind = wind, n = n)
  trend <- lt[[1]][, 2]
  res <- dat - trend
  ts1 <- ts(res, start = c(startW, startD), frequency = 7)
  ###################################################################################
  if(type == "simple")
  {
    model <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
    fcast.res <- forecast(model, h = n)
  }
  else if (type == "xreg")
  {
    model <- auto.arima(ts1, xreg=xr[wind, ], seasonal = TRUE, stepwise=FALSE, approximation = FALSE)
    fcast.res <- forecast(model, xreg = xr[1:n, ])
  }
  ###################################################################################
  if(log ==TRUE)
  {
    fcast$mean <- exp(fcast$mean)
    fcast$upper <- exp(fcast$upper)
    fcast$lower <- exp(fcast$lower)
    fcast$x <- exp(fcast$x)
    model$fitted <- exp(model$fitted)
    dat <- exp(dat)
  }
  ###################################################################################
  fcast <- lt[[3]] + as.numeric(fcast.res$mean)
  
  ddff <- data.frame(x = c(data[wind, nr], fcast), y = c(rep("a", length(wind)), rep("b", length(fcast))))
  
  pp <- xyplot(x ~ 1:length(x), data = ddff, group = y, type = "b", col = c("black", "red"))
  ###################################################################################
  li <- list(res = res, fcast.res = fcast.res, 
             dat= dat, fcast = fcast, 
             nr = nr, wind = wind, model = model, pp = pp, 
             wind = wind, pred.wind = (wind[length(wind)]+1):(wind[length(wind)]+n),
             trend = trend)
  
  if(result.save == TRUE)
  {
    df.xl.write(li, data, typ= typ, i = i)
    
  }
  else
  {
    return(li)
  }
  
  
  
  
  
  
}

