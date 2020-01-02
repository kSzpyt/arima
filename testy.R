
cft <- function(model, data)
{
  ct <- coeftest(model)
  # if(dim(ct)[1] > 1)
  # {
  ct.df <- as.data.frame(ct[1:dim(ct)[1], 1:dim(ct)[2]])
  # }
  # else if(dim(ct)[1] == 1)
  # {
  #   ct.df <- t(data.frame(ct[1, 1:dim(ct)[2]]))
  #   rownames(ct.df) <- "Intercept"
  # }
  
  if("Estimate" %in% rownames(ct.df))
  {
    ct.df <- t(ct.df)
    rownames(ct.df) <- names(model$coef)
    ct.df <- as.data.frame(ct.df)
  }
  
  if("wt" %in% rownames(ct.df) & "ds1" %in% rownames(ct.df))
  {
    ind <- which(rownames(ct.df) == "wt")
    
    if (ind != 1) 
    {
      h1 <- ct.df[1:(ind - 1), ]
      h2 <- ct.df[ind:dim(ct.df)[1], ]
      ct.df <- rbind(h2, h1)
    }
    
    
  }
  else if(!("wt" %in% rownames(ct.df)) & "ds1" %in% rownames(ct.df))
  {
    ind <- which(rownames(ct.df) == "ds1")
    
    h1 <- ct.df[1:(ind - 1), ]
    h2 <- ct.df[ind:dim(ct.df)[1], ]
    ct.df <- rbind(h2, h1)
  }
  
  
  df <- data.frame("coefs" = rownames(ct.df), "p.value" = ct.df$`Pr(>|z|)`)
  return(df)
}


#tutaj naprawić, gdy model ma tylko jeden wspołczynnik 