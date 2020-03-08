
# library(readxl)
# library(dplyr)
# library(fastDummies)
# library(forecast)
# library(ggplot2)
# library(lmtest)
# library(gridExtra)
# source("packages.R")

data <- read_xlsx("dane.xlsx")
data.original <- data

#funkcja do usuwania zer
data.zeros <- function(df, n.start = 6, n.stop = dim(df)[2], efekty = NULL)
{
  #usuwanie folderu (opcjonalne)
  unlink(file.path(getwd(), "files"), recursive = TRUE)
  unlink(file.path(getwd(), "plots"), recursive = TRUE)
  df2 <- df
  
  #indeksy dla v, k, s
  #DLA CO 6 ZMIENNEJ
  #zmiana
  a <- seq(n.start, n.stop, 6)
  b <- seq((n.start + 1), n.stop, 6)
  c <- seq((n.start + 2), n.stop, 6)
  
  d <- c(a, b, c)
  sort(d)
  
  #if pierwszy tydzień jest zerami
  #nie uwzględnia się pierwszych danych do pierwszego wystąpienia niezera
  for(n in d)
  {
    index <- which(df2[, n] == 0)
    
    if(length(which(1:7 %in% index)) == 7)
    {
      if (index[1] == 1) {
        i <- 2
        ind2 <- c(1, 2)
        
        while (index[i] - index[i-1] == 1 & i <= length(index)) 
        {
          i <- i + 1
          ind2 <- c(ind2, i)
        }
        ind2 <- ind2[-length(ind2)]
        index <- index[!(index%in%ind2)]
      }
    }
    else
    {
      ind2 <- NULL
    }
    
    zeros <- df$nrdnia[index]
    
    #średnie dla dnia specjalnego
    dd <- df2[-index, ] %>%
      select(dni_specjalne, names(.)[n]) %>%
      group_by(dni_specjalne) %>%
      summarise_at(.vars = colnames(.)[2] , mean)
    
    means <- pull(dd[, 2])
    ###########
    # for (i in sort(unique(df2$dni_specjalne))) 
    # {
    #   inx.below.mean <- df2  %>% 
    #     filter(dni_specjalne == i) %>%
    #     select(key, names(.)[n]) %>%
    #     filter_at(2, all_vars(. < means[i]))
    #   
    #   new.df <- df2[df2$dni_specjalne == i, c(1, n)] %>%
    #     mutate_at(.vars = colnames(.)[2], funs(./means[i]))
    #   
    #   new.df <- new.df[new.df[, 1] %in% inx.below.mean[, 1], ]
    #   
    #   new.df[new.df[, 2] > to.wpisz.ile, 1]
    #   
    # }
    ###########
    df2[index, n] <- means[zeros]
    # 
    # df.nas <- df2
    # df.nas[index, n] <- NA
    
  }
  df3 <- cbind("key" = as.numeric(rownames(df2)), df2)

  #efekty dnia 1 10
  if(!is.null(efekty))
  {
    df3 <- ds78(df3, efekty)
  }
  
  return(df3)
}

dummies <- function(data, type = "")
{
  if(type == "dns")
  {
    aa <- data %>%
      select(dni_specjalne)
    
    len <- length(sort(unique(aa)[, 1])) - 1 
    
    
    d.norm <- which(aa == 0)
    d.spec <- which(aa != 0)
    
    d <- as.factor(data$nrdnia)
    d <- dummy_cols(d)
    
    df <- as.data.frame(matrix(NA, nrow = dim(data)[1], ncol = len))
    colnames(df) <- paste0("ds", 1:len)
    
    aaa <- cbind(d, df)
    aaa <- aaa[, -c(1:2)]
    
    ds <- as.factor(data$dni_specjalne)
    ds <- dummy_cols(ds)
    ds <- ds[, -c(1:2)]
    
    aaa[which(data2$key %in% d.norm), 7:length(aaa)] <- 0
    aaa[which(data2$key %in% d.spec), 7:length(aaa)] <- ds[which(data2$key %in% d.spec), ]
    aaa[which(data2$key %in% d.spec), 1:6] <- 0
    
    colnames(aaa) <- c("wt", "sr", "czw", "pt", "sb", "nd", paste0("ds", 1:len))
  }
  else if (type == "ds")
  {
    aaa <- as.factor(data$dni_specjalne)
    aaa <- dummy_cols(aaa)
    aaa <- aaa[, -c(1:2)]
    colnames(aaa) <- paste0("ds", 1:length(aaa))
  }
  return(aaa)
}

ds78 <- function(data, atr = C(1, 1, 1))
{
  days.10 <- data %>%
    select(key, data) %>%
    filter(day(data) == c(1) | day(data) == c(10)) %>%
    pull(key)
  
  p2 <- max(unique(data$dni_specjalne))+2
  p1 <- max(unique(data$dni_specjalne))+1
  p3 <- max(unique(data$dni_specjalne))+3
  
  for (x in data$key)
  {
    day <- as.POSIXlt(data$data[x])$wday
    daytype <- data$dni_specjalne[x]
    month.day <- data$Dzien[x]
    
    if(month.day == 10 & atr[2] == 1)
    {
      p <- p2
      
      i <- 0
      while (day == 0 | day == 6 | daytype == 1) 
      {
        i <- i + 1
        day <- as.POSIXlt(data$data[x - i])$wday
        daytype <- data$dni_specjalne[x - i]
      }
      
      data$dni_specjalne[x - i] <- p
      # }
    }
    else if(month.day == 1 & atr[1] == 1)
    {
      p <- p1
      
      
      i <- 0
      while (day == 0 | day == 6 | daytype == 1) 
      {
        i <- i + 1
        day <- as.POSIXlt(data$data[x + i])$wday
        daytype <- data$dni_specjalne[x + i]
      }
      
      data$dni_specjalne[x + i] <- p
      # }
      
    }
    else if(data$data[x] == LastDayInMonth(data$data[x]) & atr[3] == 1)
    {
      p <- p3
      
      
      i <- 0
      while (day == 0 | day == 6 | daytype == 1) 
      {
        i <- i + 1
        day <- as.POSIXlt(data$data[x - i])$wday
        daytype <- data$dni_specjalne[x - i]
      }
      
      data$dni_specjalne[x - i] <- p
    }
  }
  return(data)
}


# bottom.outlier <- function(data, sds = 3)
# {
#   stand.data <- data
#   stand.data <- scale(stand.data)
#   
#   len <- dim(stand.data)[2]
#   
#   for (x in 1:len) 
#   {
#     m <- mean(inside.data[, x], na.rm = TRUE)
#     
#   }
# }
data2 <- data.zeros(data, efekty = efekt.d1.d10)
# data.nas <- data.zeros(data, efekty = efekt.d1.d10)$df.nas
dns <- dummies(data2, type = "dns")
ds <- dummies(data2, type = "ds")
# plots(data2)
# a <- 10
# day <- as.POSIXlt(data2$data[a])$wday
# 
# deys.10 <- data2 %>%
#   select(key, data) %>%
#   filter(day(data) == 10) %>%
#   pull(key)

