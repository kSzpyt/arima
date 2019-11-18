data2 <- data.zeros(data)

dns <- dummies(data2, type = "dns")
ds <- dummies(data2, type = "ds")

s1 <- seq(7, dim(data2)[2], by = 6)
s2 <- seq(8, dim(data2)[2], by = 6)
i <- 1
n <- 14
start <- "2011-03-01"
end <- "2012-02-29"

x1 <- list(a = 0, b = 0, c = 0, w1 = 0, w2 = 0, w3 = 0)
for (x in s1) 
{
  a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "simple", n = n, seas = TRUE)
  x1$a <- x1$a + 1
  b <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE)
  x1$b <- x1$b + 1
  c <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE)
  x1$c <- x1$c + 1
  
  write.means(a, data2, typ = "simple", i = i)
  x1$w1 <- x1$w1 + 1
  write.means(b, data2, typ = "xreg_ds", i = i)
  x1$w2 <- x1$w2 + 1
  write.means(c, data2, typ = "xreg_dns", i = i)
  x1$w3 <- x1$w3 + 1
  
  i <- i + 1
}

# a <- selfarima.means(data2, start.date = start, end.date = end, nr = s1[2], type = "simple", n = n, seas = TRUE)
# write.means(a, data2, typ = "simple", i = 2)

# selfarima2(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, 
#            result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(dns), n = 14, seas = FALSE, log = TRUE)

i <- 1
for (x in s2) 
{
  selfarima2(data2, start.date = start, end.date = end, nr = n, result.save = TRUE, i = i, type = "simple", n = n, seas = TRUE, log = TRUE , typ = "simple_log")
  selfarima2(data2, start.date = start, end.date = end, nr = n, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = TRUE , typ = "xreg_ds_log")
  selfarima2(data2, start.date = start, end.date = end, nr = n, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = TRUE , typ = "xreg_dns_log")
  
  selfarima2(data2, start.date = start, end.date = end, nr = n, result.save = TRUE, i = i, type = "simple", n = n, seas = TRUE, log = FALSE , typ = "simple")
  selfarima2(data2, start.date = start, end.date = end, nr = n, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = FALSE , typ = "xreg_ds")
  selfarima2(data2, start.date = start, end.date = end, nr = n, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = FALSE , typ = "xreg_dns")
  
  i <- i + 1
}
