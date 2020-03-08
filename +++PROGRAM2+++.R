#TO TRZEBA USTAWIĆ
#pierwszy / dziesiąty / ostatni dzień miesiąca
#  0/1    /    0/1    /    0/1
efekt.d1.d10 <- c(0, 0, 0) #TRUE
max.percentage <- 0.1
source("files.R")

#ile szeregów dla danego bankomatu
sz <- 2
#tak jak Pan mówił w tej chwili będą 2

#tego nie trzeba ruszać
s1 <- seq(7, dim(data2)[2], by = sz)
s2 <- seq(8, dim(data2)[2], by = sz)
#############

#liczba zaprognozowanych dni
n <- 31

start <- "2018-11-04"
end <- "2019-11-03"

y <- 0
j <- 1

s2 <- seq(8, dim(data2)[2], by = sz)
# s2 <- s2[37]
for(x in s2)
{
  tryCatch({
    selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "xreg", 
               xr = as.matrix(dns), n = n, seas = FALSE, log = TRUE , typ = "xreg_dns_log", data.nas = data3)
    y <- y+1
  }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  
  
  message("Loading: ",paste0(j, "/", length(s2)), paste0(" ..... ", round(j*100/length(s2), 0), "%", "\n"))
  j <- j + 1
}
beep()
