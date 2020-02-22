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
x <- 2
#tak jak Pan mówił w tej chwili będą 2

#tego nie trzeba ruszać
s1 <- seq(7, dim(data2)[2], by = x)
s2 <- seq(8, dim(data2)[2], by = x)
#############


#liczba zaprognozowanych dni
n <- 14
#te dwie linijki kodu poniżej sprawią że uruchomi się tylko jeden, pierwszy bankomat
#wykomentarzować żeby odpalić wszystkie bankomaty
s1 <- s1[1]
s2 <- s2[1]
#data początku i końca modelowania- n dni prognozy będzie po dacie "end"
start <- "2018-01-01"
end <- "2019-11-30"
###

#wszystko poniżej uruchomić
# i <- 1
# for (x in s1)
# {
#   a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "simple", n = n, seas = TRUE)
#   write.means(a, data2, typ = "simple", i = i, data.nas = data3, nr = x)
#   
#   b <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE)
#   write.means(b, data2, typ = "xreg_ds", i = i, data.nas = data3, nr = x)
#   
#   c <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE)
#   write.means(c, data2, typ = "xreg_dns", i = i, data.nas = data3, nr = x)
#   
#   i <- i + 1
# }

i <- 1
for (x in s2) 
{
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "simple", n = n, seas = TRUE, log = TRUE , typ = "simple_log", data.nas = data3)
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = TRUE , typ = "xreg_ds_log", data.nas = data3)
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = TRUE , typ = "xreg_dns_log", data.nas = data3)
  
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "simple", n = n, seas = TRUE, log = FALSE , typ = "simple", data.nas = data3)
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE, log = FALSE , typ = "xreg_ds", data.nas = data3)
  selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = i, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE, log = FALSE , typ = "xreg_dns", data.nas = data3)
  
  i <- i + 1
}
#547

#zerowe dni specjalne