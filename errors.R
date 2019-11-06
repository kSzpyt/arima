err <- function(li, custom = FALSE)
{
  if(custom == FALSE)
  {
    x.raw <- as.numeric(li[[1]])
    x.fit <- as.numeric(li[[2]]$fitted)
  }
  else
  {
    x.raw <- as.numeric(li[[1]])
    x.fit <- as.numeric(li[[2]])
  }
  
  ME <- mean(x.raw - x.fit)
  
  MAE <- mean(abs(x.raw - x.fit))
  
  MSE <- mean((x.raw - x.fit)^2)
  
  SDE <- sqrt((sum(x.raw - x.fit)^2)/length(x.raw))
  
  PE <- (x.raw - x.fit)*100/x.raw
  
  MPE <- mean(PE)
  
  MAPE <- mean(abs((x.raw - x.fit)*100/x.raw))
  
  SMAPE <- mean(abs((x.raw - x.fit)*100/((x.raw + x.fit)/2)))
  
  return(list(ME = ME, MAE = MAE, MSE = MSE, SDE = SDE, PE = PE, MPE = MPE, MAPE = MAPE, SMAPE = SMAPE))
}


