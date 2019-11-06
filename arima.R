library(forecast)
library(ggplot2)
library(lmtest)
library(gridExtra)
library(openxlsx)
library(plyr)
library(dplyr)
library(lattice)
source("testy.R")
source("errors.R")
source("script1.R")
source("writedata.R")
# source("prognozy.R")

selfarima <- function(data, oo = NULL, nr = 7, n = 7, log = FALSE, type = "simple", xr = NULL, mnk = FALSE, result.save = TRUE)
{
  ###################################################################################
  max <- dim(data)[1]
  if(is.null(oo))
  {
    oo <- max
  }
  
  wind <- (max - (oo - 1)):max
  
  startW <- as.numeric(strftime(head(data$data[wind], 1), format = "%W"))
  startD <- as.numeric(strftime(head(data$data[wind], 1) + 1, format =" %w")) 
  
  dat <- data[wind, nr]
  if(log == TRUE)
  {
    dat <- log(dat)
  }
  ###################################################################################
  if(mnk == TRUE)
  {
    fit <- lm(data[wind, nr] ~ data$key[wind])
    res <- fit$residuals
    ts1 <- ts(res, start = c(startW, startD), frequency = 7)
    
    int <- fit$coefficients[1]
    beta <- fit$coefficients[2]
    h.pred <- (dim(data)[1] + 1):((dim(data)[1]  + n))
  }
  else
  {
    ts1 <- ts(dat, start = c(startW, startD), frequency = 7) 
  }
  ###################################################################################
  if(type == "simple")
  {
    model <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
    fcast <- forecast(model, h = n)
  }
  else if (type == "xreg")
  {
    model <- auto.arima(ts1, xreg=xr[wind, ], stepwise=FALSE, approximation = FALSE)
    fcast <- forecast(model, xreg = xr[1:n, ])
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
  if(mnk == TRUE)
  {
    fcast <- int + (beta * h.pred) + as.numeric(fcast$mean)
    
    ddff <- data.frame(x = c(data[wind, nr], fcast), y = c(rep("a", oo), rep("b", length(h.pred))))
    
    pp <- xyplot(x ~ 1:length(x), data = ddff, group = y, type = "b", col = c("black", "red"))
  }
  else
  {
    xxx <- arimaorder(model)
    a <- paste0("(", xxx[1], ", ", paste0(xxx[2:3], collapse = ", "), ")")
    b <- NULL
    c <- NULL
    
    if(length(xxx) == 7)
    {
      b <- paste0("(", xxx[4], ", ", paste0(xxx[5:6], collapse = ", "), ")")
      c <- paste0("[", xxx[7], "]")
    }
    
    
    pp <- autoplot(ts1, series = "Data") +
      autolayer(model$fitted, series = "Arima") +
      autolayer(fcast, series = "prediction") +
      xlab("Week") + ylab("Value") + 
      ggtitle(paste0(as.character(names(data)[nr]), paste0(" ", as.character(type), " ARIMA ", a, b, c))) + 
      theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")
  }
  ###################################################################################  
  
  
  li <- list(dat, model, nr, fcast, pp)
  
  if(result.save == TRUE)
  {
    tt <- as.character(type)
    
    if(log == TRUE)
    {
      tt <- paste0(tt, "_log")
    }
    if(mnk == TRUE)
    {
      tt <- paste0(tt, "_mnk")
      df.xl.write(li, data, tt, mnk = TRUE)
    }
    else
    {
      df.xl.write(li, data, tt, mnk = FALSE)
    }
  }
  else
  {
    return(li)
  }
  
  
}






# okno <- 50




#gdy xreg jest użyty "h" jest ustawaine na liczbę wierszy xreg
# b <- selfarima.xreg(data2, oo = okno, nr = 6, xr = d)
# selfarima.autoplot(b, n = 7)
# 
# c <- selfarima.mnk(data2, oo = okno, nr = 6)
# selfarima.autoplot(c, 7)


#istotnośc współczynników
#3 prognozy

#prognoza liczby wypłat jak dizen zwykly 
#jeżeli zwykły piątek licze średnią ze wszystkich piątkóW
#progonozwanie na samych wypłatach, tak ja kwyżej, i na malych srednich duzych 

# write.csv(data.frame(1:10, 1:10), file.path(getwd(), "plots", "fitted", paste0(as.character(names(data)[1]), ".csv")))



#logarytm
#mnk - trend potem dodamy
#pakiet do trendów
#arsima
#dummy dla dni tygodnia 