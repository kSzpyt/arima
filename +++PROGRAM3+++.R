#efekty pierwszego, dziesiątego, ostatniego dnia miesiąca 
efekt.d1.d10 <- c(0, 0, 0)  #0 - wyłączone, 1- włączone
# wartości mniejsze od % wartości średniej (tylko dolne wartości) będą nie brane pod uwagę
max.percentage <- 0.1
#ładowanie wszystkich funkcji (pierwsze uruchomienie chwilę trwa)
source("files.R")

#ile szeregów dla danego bankomatu
sz <- 2

# s1 służy do uruchomienia pętli  dla danych o ilości wypłat wypłat 
s1 <- seq(7, dim(data2)[2], by = sz)
# s2 służy do uruchomienia pętli  dla danych o wielkości wypłat 
s2 <- seq(8, dim(data2)[2], by = sz)

#liczba zaprognozowanych dni
n <- 31

#początek i koniec okresu modelowania
start <- "2018-11-04"
end <- "2019-11-03"

#czy w funckji ma być uwzględniona sezonowość?
seas <- FALSE #TRUE / FALSE

#czy dane mają być logarytmowane
log <- TRUE #TRUE / FALSE

########################################################################################################################
################################# MODELE WYKORZYSTUJĄCE WIELKOŚĆ WYPŁAT ###################################################
########################################################################################################################
#dla prostej arimy
type <- "simple"
xr <- NULL
nazwa <- ifelse(log == FALSE, "simple", "simple_log") 

#to wystarczy uruchomić, nie trzeba nic zmieniać
j <- 1
ldi <- 0
for(x in s2) 
{
  ldi <- ldi + 1
  tryCatch(
    {
      selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = type, xr = as.matrix(xr),
                 n = n, seas = seas, log = log , typ = nazwa, data.nas = data3)
    }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  j <- j + 1
  loading(ldi, length(s2))
}

########################################################################################################################
#dla modelu z uwzględnienem tylko dni specjalnych
type <- "xreg"
xr <- ds
nazwa <- ifelse(log == FALSE, "xreg_ds", "xreg_ds_log")

j <- 1
ldi <- 0
for(x in s2) 
{
  ldi <- ldi + 1
  tryCatch(
    {
      selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = type, xr = as.matrix(xr),
                 n = n, seas = seas, log = log , typ = nazwa, data.nas = data3)
    }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  j <- j + 1
  loading(ldi, length(s2))
}

########################################################################################################################
#dla modelu z uwzględnienem tylko dni specjalnych oraz dni tygodnia
type <- "xreg"
xr <- dns
nazwa <- ifelse(log == FALSE, "xreg_dns", "xreg_dns_log")

j <- 1
ldi <- 0
for(x in s2) 
{
  ldi <- ldi + 1
  tryCatch(
    {
      selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = type, xr = as.matrix(xr),
                 n = n, seas = seas, log = log , typ = nazwa, data.nas = data3)
    }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  j <- j + 1
  loading(ldi, length(s2))
}

########################################################################################################################
################################# MODELE WYKORZYSTUJĄCE ILOŚĆ WYPŁAT ###################################################
########################################################################################################################
#dla prostej arimy
type <- "simple"
xr <- dns
nazwa <- ifelse(log == FALSE, "means_simple", "means_simple_log")

j <- 1
ldi <- 0
for(x in s1) 
{
  ldi <- ldi + 1
  tryCatch(
    {
      selfarima2(data2, start.date = start, end.date = end, nr = x, result.save = TRUE, i = j, type = type, xr = as.matrix(xr),
                 n = n, seas = seas, log = log , typ = nazwa, data.nas = data3)
    }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  j <- j + 1
  loading(ldi, length(s2))
}
########################################################################################################################
#dla modelu z uwzględnienem tylko dni specjalnych 
type <- "xreg"
xr <- ds
nazwa <- ifelse(log == FALSE, "means_xreg_ds", "means_xreg_ds_log")

j <- 1
ldi <- 0
for(x in s1) 
{
  ldi <- ldi + 1
  tryCatch(
    {
      a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = type, n = n, seas = seas)
      write.means(a, data2, typ = nazwa, i = j, data.nas = data3, nr = x)
    }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  j <- j + 1
  loading(ldi, length(s2))
}

########################################################################################################################
#dla modelu z uwzględnienem dni specjalnych oraz dni tygodnia
type <- "xreg"
xr <- dns
nazwa <- ifelse(log == FALSE, "means_xreg_dns", "means_xreg_dns_log")

j <- 1
ldi <- 0
for(x in s1) 
{
  ldi <- ldi + 1
  tryCatch(
    {
      a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = type, n = n, seas = seas)
      write.means(a, data2, typ = nazwa, i = j, data.nas = data3, nr = x)
    }, error = function(e){cat("ERROR :", print(x), conditionMessage(e), "\n"); beep()})
  j <- j + 1
  loading(ldi, length(s2))
}





# a <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "simple", n = n, seas = TRUE)
# write.means(a, data2, typ = "simple", i = i, data.nas = data3, nr = x)
# z <-z+1

# b <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(ds), n = n, seas = TRUE)
# write.means(b, data2, typ = "xreg_ds", i = i, data.nas = data3, nr = x)
# z <- z+1

# c <- selfarima.means(data2, start.date = start, end.date = end, nr = x, type = "xreg", xr = as.matrix(dns), n = n, seas = FALSE)
# write.means(c, data2, typ = "xreg_dns", i = i, data.nas = data3, nr = x)
# z <- z+1