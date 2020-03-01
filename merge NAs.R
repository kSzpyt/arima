self.add.NAs <- function(data, data2)
{
  index <- which(is.na(data))
  
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
      #ciąg zer -> zamiana na NA
      ind2 <- ind2[-length(ind2)]
      # df2[ind2, n] <- NA
      #index- index zer niepoczątkowych
      # index <- index[!(index%in%ind2)]
      # df2[index, n] <- NA
    }
  }
  else
  {
    ind2 <- NULL
  }
  data2 <- c(rep(NA, length(ind2)), data2)
  

  return(data2)
}

first.n.zeros <- function(data)
{
  index <- which(is.na(data))
  
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
      #ciąg zer -> zamiana na NA
      ind2 <- ind2[-length(ind2)]
      # df2[ind2, n] <- NA
      #index- index zer niepoczątkowych
      # index <- index[!(index%in%ind2)]
      # df2[index, n] <- NA
    }
  }
  else
  {
    ind2 <- NULL
  }
  return(ind2)
}
