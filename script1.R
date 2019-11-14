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
  return(df2)
}

dummies <- function(data)
{
  # d <- (df[, 5])
  # d <- as.factor(d)
  # d <- dummy_cols(d)
  # dim(d)
  # d <- d[, -c(1:2)]
  # names(d) <- paste0("ds", 1:5)
  # d <- as.matrix(d)
  # return(d)
  aa <- data %>%
    select(dni_specjalne)
  
  d.norm <- which(aa == 0)
  d.spec <- which(aa != 0)
  
  d <- as.factor(data$nrdnia)
  d <- dummy_cols(d)
  
  aaa <- cbind(d, ds1 = NA, ds2 = NA, ds3 = NA, ds4 = NA, ds5 = NA)
  
  ds <- as.factor(data$dni_specjalne)
  ds <- dummy_cols(ds)
  ds <- ds[, -c(1:2)]
  
  aaa[which(data2$key %in% d.norm), 9:13] <- 0
  aaa[which(data2$key %in% d.spec), 9:13] <- ds[which(data2$key %in% d.spec), ]
  aaa[which(data2$key %in% d.spec), 2:8] <- 0
  aaa <- aaa[, -c(1:2)]
  colnames(aaa) <- c("wt", "sr", "czw", "pt", "sb", "nd", "ds1", "ds2", "ds3", "ds4", "ds5")
  
  return(aaa)
}

# ddd <- dummies(data2)
# #robimy dummy variable takie że są 0-1 dla dni normalnych oraz cbind z dniami specjlanymi (1-5)
# #pon wt sr czw pt sb nd ds1 ds2 ds3 ds4 ds5
# 
# d <- as.factor(data2$dni_specjalne)
# d <- dummy_cols(d)
# d <- d[, -c(1:2)]
