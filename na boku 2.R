dd <- tibble("ds" = c(0, 2, 3, 5), "value" = c(143, 543,123,835))
dd

tib2 <- tibble("ds" = c(1, 4), "value" = rep(NA, 2))
tib2

merge(tib2, tib, all = T)

a <- c(0, 2, 3, 5)
b <- 0:5
b[!b %in% a]

if(length(pull(dd[, 2])) != length(1:max(dd$ds)))
{
  a <- pull(dd[, 1])
  b <- 0:max(dd$ds)
  b <- b[!b %in% a]
  tib2 <- tibble("dni_specjalne" = b, "val" = rep(NA, length(b)))
  colnames(tib2) <- c("ds", "value")
  
  dd <- merge(tib2, dd, all = T)
}

win <- 366:730

data[win, c(1:5, 79)]
data2[win, c(1:6, 80)]
