# library(car)
logistic.trend <- function(data, nr = 7, wind = NULL, n = 7, log = FALSE, log.rev = FALSE)
{
  if(is.null(wind)) wind <- data$key
  
  dat <- as.numeric(data[wind, nr])
  
  if(log == TRUE)
  {
    dat <- log(dat)
  }
  dat2 <- na.omit(dat)
  if(!is.null(na.action(dat2)))
  {
    ex <- na.action(dat2)
    wind2 <- wind[-ex]
    coefs <- coef(lm(logit(dat2/max(dat2)) ~ wind2))
  }
  else
  {
    wind2 <- wind
    coefs <- coef(lm(logit(dat2/max(dat2)) ~ wind2))
  }
  
  model <- nlsLM(dat ~ phi1/(1+exp(-(phi2+phi3*wind))), 
              start = list(phi1 = max(dat, na.rm = TRUE), phi2 = coefs[1], phi3 = coefs[2]), data = data, trace=TRUE)
  
  phi1 <- coef(model)[1]
  phi2 <- coef(model)[2]
  phi3 <- coef(model)[3]
  
  # index <- which(is.na(dat))
  # 
  # if (index[1] == 1) {
  #   i <- 2
  #   ind2 <- c(1, 2)
  #   
  #   while (index[i] - index[i-1] == 1 & i <= length(index)) 
  #   {
  #     i <- i + 1
  #     ind2 <- c(ind2, i)
  #   }
  #   #ciąg zer -> zamiana na NA
  #   ind2 <- ind2[-length(ind2)]
  #   # df2[ind2, n] <- NA
  #   #index- index zer niepoczątkowych
  #   index <- index[!(index%in%ind2)]
  #   # df2[index, n] <- NA
  # }
  # wind3 <- wind[-ind2]
  wind3 <- wind
  
  trend <- phi1/(1 + exp(-(phi2 + phi3 * wind3))) 
  if(log.rev == TRUE)
  {
    trend <- exp(trend)
  }
  
  res <- data.frame(x = wind3, fit = trend)
  
  len <- wind3[length(wind3)] + 1
  pred.trend <- phi1/(1 + exp(-(phi2 + phi3 * (len:(len+n-1) ))))
  
  if(log.rev == TRUE)
  {
    pred.trend <- exp(pred.trend)
    dat <- exp(dat)
  }
  
  return(list(res = res, dat = dat, pred.trend = pred.trend, trend = trend))
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

# 
# t <- logistic.trend(data2, nr = 8, n = 52, log = TRUE)
# plot.logistic.trend(t)
# 
# xd <- t[[3]] + a[[4]]
# 
# start.date <- as.POSIXct(start.date, tz = "UTC")
# end.date <- as.POSIXct(end.date, tz = "UTC")
# wind <- which(data$data == start.date):which(data$data == end.date)
# ex <- which(!is.na(data[wind, nr]))
# wind <- wind[ex]
# 
# startW <- as.numeric(strftime(head(data$data, 1), format = "%W"))
# startD <- as.numeric(strftime(head(data$data, 1) + 1, format =" %w")) 
# 
# lt <- logistic.trend(data = data2, nr = 7, n = 52, wind = (731:1096))
# trend <- lt[[1]][, 2]
# res <- abs(data$`88209_v`[731:1096] - trend)
# ts1 <- ts(res, start = c(startW, startD), frequency = 7)
# 
# fitt <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
# fc <- forecast(fitt, h = 52)
# 
# fc$mean
# 
# plot(fc$mean, type = "l")
