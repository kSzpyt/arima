
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
  df <- data.frame("coefs" = rownames(ct.df), "p.value" = ct.df$`Pr(>|z|)`)
  return(df)
}


#tutaj naprawić, gdy model ma tylko jeden wspołczynnik 