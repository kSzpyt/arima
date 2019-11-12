library(car)
logistic.trend <- function(data, nr = 7)
{
  dat <- data[, nr]
  coefs <- coef(lm(logit(dat/max(dat))~data$key))
  
  model <- nls(dat~phi1/(1+exp(-(phi2+phi3*data2$key))), 
              start=list(phi1 = max(dat), phi2 = (coefs[1]), phi3 = (coefs[2])), data = data, trace = TRUE, algorithm = "plinear")
  
  phi1 <- coef(model)[1]
  phi2 <- coef(model)[2]
  phi3 <- coef(model)[3]
  trend <- phi1/(1 + exp(-(phi2 + phi3 * data2$key))) 
  predict <- data.frame(x = data2$key, y = trend)
  
  return(list(predict, dat))
}

plot.logistic.trend <- function(logistic.list)
{
  dat <- logistic.list[[2]]
  predict <- logistic.list[[1]]
  
  data %>%
    ggplot(aes(x=predict[, 1],y=dat))+
    geom_line(color='blue')+theme_bw()+
    geom_line(data=predict,aes(x=x,y=y), size=2)
}


t <- logistic.trend(data2, 7)
plot.logistic.trend(t)
