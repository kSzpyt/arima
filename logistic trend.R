library(car)
logistic.trend <- function(data, nr = 7, wind = NULL)
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
  
  return(list(res, dat))
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


t <- logistic.trend(data2, 10)
plot.logistic.trend(t)
