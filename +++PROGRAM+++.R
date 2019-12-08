#jeżeli wartość poniższej zmiennej będzie ustawiona na FALSE to efekty nie zostaną uwzględnione
#TRUE to uwzględnienie efektóW dnia pierwszego i ostatniego
efekt.d1.d10 <- FALSE #TRUE
source("files.R")

s1 <- seq(7, dim(data2)[2], by = 6)
s2 <- seq(8, dim(data2)[2], by = 6)
i <- 1
n <- 14
start <- "2010-01-01"
end <- "2010-06-30"

s1 <- s1[1]
s2 <- s2[1]



for (x in s1) 
{
  a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "simple", n = n, seas = TRUE)
  # b <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE)
  # c <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE)

  write.means(a, data2, typ = "simple", i = i)
  # write.means(b, data2, typ = "xreg_ds", i = i)
  # write.means(c, data2, typ = "xreg_dns", i = i)

  i <- i + 1
}

i <- 1
for (x in s2) 
{
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "simple", n = n, seas = TRUE, log = TRUE , typ = "simple_log")
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = TRUE , typ = "xreg_ds_log")
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = TRUE , typ = "xreg_dns_log")
  
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "simple", n = n, seas = TRUE, log = FALSE , typ = "simple")
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = FALSE , typ = "xreg_ds")
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = FALSE , typ = "xreg_dns")
  
  i <- i + 1
}

