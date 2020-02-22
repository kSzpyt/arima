err <- function(raw, fit)
{
  x.raw <- raw
  x.fit <- fit

  
  ME <- mean(x.raw - x.fit, na.rm = TRUE)
  
  MAE <- mean(abs(x.raw - x.fit), na.rm = TRUE)
  
  MSE <- mean((x.raw - x.fit)^2, na.rm = TRUE)
  
  SDE <- sqrt((sum(x.raw - x.fit, na.rm = TRUE)^2)/length(x.raw))
  
  PE <- (x.raw - x.fit)*100/x.raw
  
  MPE <- mean(PE, na.rm = TRUE)
  
  MAPE <- mean(abs((x.raw - x.fit)*100/x.raw), na.rm = TRUE)
  
  SMAPE <- mean(abs((x.raw - x.fit)*100/((x.raw + x.fit)/2)), na.rm = TRUE)
  
  return(list(ME = ME, MAE = MAE, MSE = MSE, SDE = SDE, MPE = MPE, MAPE = MAPE, SMAPE = SMAPE))
}


