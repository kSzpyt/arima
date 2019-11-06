library(readxl)
library(dplyr)
library(fastDummies)
library(forecast)
library(ggplot2)
library(lmtest)
library(gridExtra)

data <- read_xlsx("dane.xlsx")

#funkcja do usuwania zer
data.zeros <- function(df, n.start = 6, n.stop = dim(df)[2])
{
  unlink(file.path(getwd(), "files"), recursive = TRUE)
  unlink(file.path(getwd(), "plots"), recursive = TRUE)
  df2 <- df
  
  a <- seq(n.start, n.stop, 6)
  b <- seq((n.start + 1), n.stop, 6)
  c <- seq((n.start + 2), n.stop, 6)
  
  d <- c(a, b, c)
  sort(d)
  
  for(n in d)
  {
    index <- which(df[, n] == 0)
    
    zeros <- df$nrdnia[index]
    
    dd <- df[-index, ] %>%
      select(nrdnia, names(.)[n]) %>%
      group_by(nrdnia) %>%
      summarise_at(.vars = colnames(.)[2] , mean)
    
    means <- pull(dd[, 2])
    
    df2[index, n] <- means[zeros]
  }
  df2 <- cbind("key" = as.numeric(rownames(df2)), df2)
  return(df2)
}

dummies <- function(df)
{
  d <- (df[, 5])
  d <- as.factor(d)
  d <- dummy_cols(d)
  dim(d)
  d <- d[, -c(1:2)]
  names(d) <- paste0("ds", 1:5)
  d <- as.matrix(d)
  return(d)
}







