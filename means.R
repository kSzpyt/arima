tib.dn <- data2 %>%
  select(nrdnia, dni_specjalne, ends_with("_v"), ends_with("_k")) %>%
  filter(dni_specjalne == 0) %>%
  group_by(nrdnia) %>%
  summarise_at(vars(-dni_specjalne), sum)

tib.ds <- data2 %>%
  select(nrdnia, dni_specjalne, ends_with("_v"), ends_with("_k")) %>%
  filter(dni_specjalne != 0) %>%
  group_by(dni_specjalne) %>%
  summarise_at(vars(-nrdnia), sum)


tib.mean <- function(tib)
{
  tib <- tib[, -c(1)]
  
  d <- dim(tib)[2]
  
  tib <- tib[,((d/2 + 1):d)]/tib[,(1:(d/2))]
  
  tib <- cbind(rownames(tib), tib)
  
  colnames(tib)[1] <- "daytype"
  return(tib)
  # colnames(select(data2, ends_with("_v")))
}

tib.dn <- tib.mean(tib.dn)

tib.ds <- tib.mean(tib.ds)

vol.pred.list <-  selfarima2(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, 
                             result.save = FALSE, i = 1, type = "xreg", xr = as.matrix(dns), n = 14, seas = FALSE, log = TRUE)

vol.pred.list$fcast
a <- data2$dni_specjalne[vol.pred.list$pred.wind]
b1 <- which(a == 0) #indeksy dni normlanych
b2 <- which(a != 0) #indeksy dni specjalnych

c1 <- vol.pred.list$pred.wind[b1]
c2 <- vol.pred.list$pred.wind[b2]

d1 <- data2$nrdnia[c1]
d2 <- data2$dni_specjalne[c2]

e1 <- sapply(1:length(d1), function(x)
  {
    tib.dn[tib.dn$daytype == d1[x], -1]
})
e1 <- t(e1)

e2 <- sapply(1:length(d2), function(x)
  {
    tib.ds[tib.dn$daytype == d2[x], -1]
})
e2 <- t(e2)

f <- rep(NA, length(vol.pred.list$fcast))

f[b1] <- as.numeric(e1[, 1])
f[b2] <- as.numeric(e2[, 1])

g <- f*vol.pred.list$fcast

plot(data2$`88209_k`[vol.pred.list$pred.wind], type = "l")
lines(g, col = "red")

