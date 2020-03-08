#TO TRZEBA USTAWIĆ
#pierwszy / dziesiąty / ostatni dzień miesiąca
#  0/1    /    0/1    /       0/1
efekt.d1.d10 <- c(0, 0, 0) #TRUE
###
#to trzeba uruchomić
max.percentage <- 0.1
source("files.R")
#w razie błędu spróbować uruchomić poniższą linijkę kodu (odkomentarzować) i ponownie uruchomić 'source("files.R")'
# install.packages("minpack.lm_1.2-1.tar.gz", repos = NULL)


#ile szeregów dla danego bankomatu
sz <- 2
#tak jak Pan mówił w tej chwili będą 2

#tego nie trzeba ruszać
s1 <- seq(7, dim(data2)[2], by = sz)
s2 <- seq(8, dim(data2)[2], by = sz)
#############


#liczba zaprognozowanych dni
n <- 31
#te dwie linijki kodu poniżej sprawią że uruchomi się tylko jeden, pierwszy bankomat
# wykomentarzować żeby odpalić wszystkie bankomaty

# s1 <- s1[-c(12, 19, 20, 37, 51, 55, 72, 84, 86:88, 91:95)]
# s2 <- s2[-c(12, 19, 20, 37, 51, 55, 72, 84, 86:88, 91:95)]
# data początku i końca modelowania- n dni prognozy będzie po dacie "end"
start <- "2018-11-04"
end <- "2019-11-03"
# start.date <- as.POSIXct(start, tz = "UTC", format = c("%Y-%m-%d"))
# end.date <- as.POSIXct(end, tz = "UTC", format = c("%Y-%m-%d"))
###
#025, 039, 033
#wszystko poniżej uruchomić
# z <- 0
# i <- 1
# for (x in s1)
# {
  # a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "simple", n = n, seas = TRUE)
  # write.means(a, data2, typ = "simple", i = i, data.nas = data3, nr = x)
  # z <-z+1

  # b <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE)
  # write.means(b, data2, typ = "xreg_ds", i = i, data.nas = data3, nr = x)
  # z <- z+1

  # c <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE)
  # write.means(c, data2, typ = "xreg_dns", i = i, data.nas = data3, nr = x)
  # z <- z+1
  
  # a.log <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "simple", n = n, seas = TRUE, log = TRUE)
  # write.means(a.log, data2, typ = "simple_log", i = i, data.nas = data3, nr = x)
  # z <- z+1
  # tryCatch({
    # b.log <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = TRUE)
    # write.means(b.log, data2, typ = "xreg_ds_log", i = i, data.nas = data3, nr = x)
    # z <- z+1
    
    # c.log <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = TRUE)
    # write.means(c.log, data2, typ = "xreg_dns_log", i = i, data.nas = data3, nr = x)
    # z <- z+1
    
  # }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n")})

  # i <- i + 1
  
# }
# beep()

y <- 0
j <- 1
# s2 <- s2[19]
for (x in s2)
{
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "simple", n = n, seas = TRUE, log = TRUE , typ = "simple_log", data.nas = data3)
  # y <- y+1
  tryCatch({
    # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = TRUE , typ = "xreg_ds_log", data.nas = data3)
    # y <- y+1
    selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "xreg", 
               xr = as.matrix(dns), n = n, seas = FALSE, log = TRUE , typ = "xreg_dns_log", data.nas = data3)
    y <- y+1
  }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})

  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "simple", n = n, seas = TRUE, log = FALSE , typ = "simple", data.nas = data3)
  # y <- y+1
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = FALSE , typ = "xreg_ds", data.nas = data3)
  # y <- y+1
  # selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = FALSE , typ = "xreg_dns", data.nas = data3)
  # y <- y+1

  j <- j + 1
}
beep()

#547
#zerowe dni specjalne