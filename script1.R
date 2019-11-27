# library(readxl)
# library(dplyr)
# library(fastDummies)
# library(forecast)
# library(ggplot2)
# library(lmtest)
# library(gridExtra)
# source("packages.R")

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
  df3 <- ds78(df2)
  
  return(df3)
}

dummies <- function(data, type = "")
{
  if(type == "dns")
  {
    aa <- data %>%
      select(dni_specjalne)
    
    d.norm <- which(aa == 0)
    d.spec <- which(aa != 0)
    
    d <- as.factor(data$nrdnia)
    d <- dummy_cols(d)
    
    aaa <- cbind(d, ds1 = NA, ds2 = NA, ds3 = NA, ds4 = NA, ds5 = NA, ds6 = NA, ds7 = NA, ds8 = NA)
    
    ds <- as.factor(data$dni_specjalne)
    ds <- dummy_cols(ds)
    ds <- ds[, -c(1:2)]
    
    aaa[which(data2$key %in% d.norm), 9:length(aaa)] <- 0
    aaa[which(data2$key %in% d.spec), 9:length(aaa)] <- ds[which(data2$key %in% d.spec), ]
    aaa[which(data2$key %in% d.spec), 2:8] <- 0
    aaa <- aaa[, -c(1:2)]
    colnames(aaa) <- c("wt", "sr", "czw", "pt", "sb", "nd", paste0("ds", 1:8))
  }
  else if (type == "ds")
  {
    aaa <- as.factor(data$dni_specjalne)
    aaa <- dummy_cols(aaa)
    aaa <- aaa[, -c(1:2)]
    colnames(aaa) <- paste0("ds", 1:8)
  }
  return(aaa)
}

ds78 <- function(data)
{
  foo <- function(data, nr)
  {
    days.10 <- data %>%
      select(key, data) %>%
      filter(day(data) == nr) %>%
      pull(key)
    
    if(nr == 10)
    {
      p <- 8
    }
    else if(nr == 1)
    {
      p <- 7
    }
    
    for (x in days.10)
    {
      day <- as.POSIXlt(data$data[x])$wday
      
      if(day == 0)
      {
        data$dni_specjalne[x-2] <- p
      }
      else if (day == 6)
      {
        data$dni_specjalne[x-1] <- p
      }
      else
      {
        data$dni_specjalne[x] <- p
      }
    }
    return(data)
  }
  
  data <- foo(data, 10)
  data <- foo(data, 1)
  return(data)
}
data2 <- data.zeros(data)
dns <- dummies(data2, type = "dns")
ds <- dummies(data2, type = "ds")
plots(data2)
# a <- 10
# day <- as.POSIXlt(data2$data[a])$wday
# 
# deys.10 <- data2 %>%
#   select(key, data) %>%
#   filter(day(data) == 10) %>%
#   pull(key)
