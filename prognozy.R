library(dplyr)
library(stringr)
library(data.table)
# rozróznienie na dni normalne (7) i dni specjalne (5) na raz
# dodać daty jako okno estymacji 

# # attach(data2)

tib <- data2 %>%
  select(nrdnia, ends_with("_v"), ends_with("_k")) %>%
  group_by(nrdnia) %>%
  summarise_all(funs(sum(.)))

tib <- tib[, -c(1)]

d <- dim(tib)[2]

tib <- tib[,((d/2 + 1):d)]/tib[,(1:(d/2))]

tib2 <- data2 %>%
  select(dni_specjalne, ends_with("_v"), ends_with("_k")) %>%
  group_by(dni_specjalne) %>%
  summarise_all(funs(sum(.)))

tib2 <- tib2[, -c(1)]

d2 <- dim(tib2)[2]

tib2 <- tib2[,((d2/2 + 1):d2)]/tib2[,(1:(d2/2))]
plot(tib2[, 1], type = "l")
# 
# cnam <- tib[, c(1:2)]
tib <- tib[, -c(1)]

d <- dim(tib)[2]

tib <- tib[,((d/2 + 1):d)]/tib[,(1:(d/2))]
# 
# rr <- bind_cols(tib2, as_tibble(tib))
# rr <- as.data.frame(rr)
# 
# max <- dim(data2)[1]
# windo <- (max - 51):max
# 
# abc <- selfarima.simple(data2, oo = 150, nr = 7, 52)
# ff <- abc[[4]]$mean
# ff
# 
# df <- data2[which(data2$key %in% (1:length(windo))), c(5, 6)]
# 
# df2 <- df == data.frame("a" = rep(rr[1, 1], dim(df)[1]), "b" = rep(rr[1, 2], dim(df)[1]))
# 
# df <- cbind(df, as.data.frame(ff))
# 
# ind <- which(df2[, 1] == TRUE & df2[, 2] == TRUE)
# 
# for (i in vector)
# {
#   df[ind, 3] <- df[ind, 3] * rr[1, 3]
# }
# 
# library(data.table)
# aaa <- c("123_v", "142_v" , "512_k")
# 
# substring(aaa, -5)
# 
# tstrsplit(aaa, "_")[[1]]

foo <- function(data, oo = NULL, nr = 7, n = 7, log = FALSE, type = "simple", xr = NULL, mnk = FALSE)
{
  nam <- colnames(data)[nr]
  nam <- tstrsplit(nam, "_")[[1]]
  
  max <- dim(data)[1]
  windo <- (max - (oo - 1)):max
  
  tib <- data2[windo, ] %>%
    select(nrdnia, dni_specjalne, starts_with(nam)) %>%
    select(nrdnia, dni_specjalne, ends_with("_v"), ends_with("_k")) %>%
    group_by(nrdnia, dni_specjalne) %>%
    summarise_all(funs(sum(.))) 
  
  cnam <- tib[, c(1:2)]
  s.days.values <- tib[, -c(1:2)]
  
  d <- dim(s.days.values)[2]
  
  s.days.values <- s.days.values[,((d/2 + 1):d)]/s.days.values[,(1:(d/2))]
  
  # df1 <- bind_cols(cnam, as_tibble(s.days.values))
  # df1 <- as.data.frame(df1)
  
  s.days.to.pred1 <- data2[which(data2$key %in% (1:n)), c(5, 6)]
  
  model.list <- selfarima(data, oo = oo, nr = nr, n = n, log = log, type = type, xr = xr, mnk = mnk, result.save = FALSE)
  
  fcast.v <- model.list[[4]]
  
  s.days.to.pred2 <- fcast.v$mean
  
  for (i in 1:dim(cnam)[1]) 
  {
    TF.df <- s.days.to.pred1 == data.frame(as.numeric(rep(cnam[i, 1], dim(s.days.to.pred1)[1])), as.numeric(rep(cnam[i, 2], dim(s.days.to.pred1)[1])))
    
    ind <- which(TF.df[, 1] == TRUE & TF.df[, 2] == TRUE)
    
    
    s.days.to.pred2[ind] <- s.days.to.pred2[ind] * s.days.values[i, 1]
    
  }
  
  df <- data.frame(1:n, s.days.to.pred1, as.numeric(s.days.to.pred2))
  return(list(df, tib, model.list))
}

xxx <- foo(data2, oo = 365, nr = 7, n = 100)

plot(xxx[[1]][, 4], type = "l")
plot(data2[, 8], type = "l")
plot(c(data2[, 8], xxx[[1]][, 4]), type = "l")
