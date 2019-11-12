selfarima.xreg <- function(data, oo = 150, nr = 7, n = 7, xr)
{
  max <- dim(data)[1]
  wind <- (max - (oo - 1)):max
  
  startW <- as.numeric(strftime(head(data$data[wind], 1), format = "%W"))
  startD <- as.numeric(strftime(head(data$data[wind], 1) + 1, format =" %w")) 
  
  ts1 <- ts(data[wind, nr], start = c(startW, startD), frequency = 7)
  
  model <- auto.arima(ts1, xreg=xr[wind, ], stepwise=FALSE, approximation = FALSE)
  
  fcast <- forecast(model, xreg = xr[1:n, ])
  
  #######################################plot
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
    ggtitle(paste0(as.character(names(data)[nr]), paste0("  ARIMA ", a, b, c))) + 
    theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")
  
  
  li <- list(ts1, model, nr, fcast, pp)
  
  # df.xl.write(li, data, dat, "simple")
  
  
  return(li)
}



selfarima.autoplot <- function(model.list)
{
  # ts <- model.list[[1]]
  model <- model.list[[2]]
  nr <- model.list[[3]]
  fcast <- model.list[[4]]
  ts <- fcast$x
  xxx <- arimaorder(model)
  a <- paste0("(", xxx[1], ", ", paste0(xxx[2:3], collapse = ", "), ")")
  b <- NULL
  c <- NULL
  
  if(length(xxx) == 7)
  {
    b <- paste0("(", xxx[4], ", ", paste0(xxx[5:6], collapse = ", "), ")")
    c <- paste0("[", xxx[7], "]")
  }
  
  pp <- autoplot(ts, series = "Data") +
    autolayer(model$fitted, series = "Arima") +
    autolayer(fcast, series = "prediction") +
    xlab("Week") + ylab("Value") + 
    ggtitle(paste0(as.character(names(data)[nr]), paste0("  ARIMA ", a, b, c))) + 
    theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")
  
  return(pp)
}

selfarima.mnk <- function(data, oo = NULL, nr = 7, n = 7, type = "simple", xr = NULL)
{
  max <- dim(data)[1]
  if(is.null(oo))
  {
    oo <- max
  }
  
  wind <- (max - (oo - 1)):max
  
  startW <- as.numeric(strftime(head(data$data[wind], 1), format = "%W"))
  startD <- as.numeric(strftime(head(data$data[wind], 1) + 1, format =" %w")) 
  
  fit <- lm(data[wind, nr] ~ data$key[wind])
  res <- fit$residuals
  ts1 <- ts(res, start = c(startW, startD), frequency = 7)
  
  # if(type == "simple")
  # {
  #   model <- auto.arima(ts1, seasonal = TRUE, approximation = FALSE, stepwise = FALSE) 
  #   fcast <- forecast(model, h = n)
  # }
  # else if (type == "xreg")
  # {
  #   model <- auto.arima(ts1, xreg=xr[wind, ], stepwise=FALSE, approximation = FALSE)
  #   fcast <- forecast(model, xreg = xr[1:n, ])
  # }
  
  int <- fit$coefficients[1]
  beta <- fit$coefficients[2]
  h.pred <- (dim(data)[1] + 1):((dim(data)[1]  + n))
  
  prediction <- int + (beta * h.pred) + as.numeric(fcast$mean)
  
  ddff <- data.frame(x = c(data[wind, nr], prediction), y = c(rep("a", oo), rep("b", length(h.pred))))
  
  xda <- xyplot(x ~ 1:length(x), data = ddff, group = y, type = "b", col = c("black", "red"))
  
  li <- list(ts1, model, nr, fcast, xda, fit)
  
  # df.xl.write(li, data, dat, "mnk")
  
  return(li)
}
