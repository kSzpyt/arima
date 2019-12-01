tibb.dn.dns <- function(data)
{
  tib.dn <- data %>%
    select(nrdnia, dni_specjalne, ends_with("_v"), ends_with("_k")) %>%
    filter(dni_specjalne == 0) %>%
    group_by(nrdnia) %>%
    summarise_at(vars(-dni_specjalne), sum)
  
  tib.ds <- data %>%
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
  # colnames(tib.dn) <- paste0("m", seq(7, dim(data)[2], by = 6))
  tib.ds <- tib.mean(tib.ds)
  return(list(tib.dn = tib.dn, tib.ds = tib.ds))
}

selfarima.means <- function(data, start.date = "2010-01-01", end.date = "2012-12-31", nr = 7, n = 7,
         type = "simple", xr = NULL, typ = NULL, i, seas = TRUE)
{
  vol.pred.list <-  selfarima2(data, start.date = start.date, end.date = end.date, nr = nr, 
                               result.save = FALSE, i = i, type = type, xr = xr, n = n, seas = seas, log = FALSE)
  
  tib.dn <- tibb.dn.dns(data)$tib.dn
  
  tib.ds <- tibb.dn.dns(data)$tib.ds
  
  #c(vol.pred.list$wind, vol.pred.list$pred.wind)
  foo <- function(xxx, tib.dn, tib.ds)
  {
    a <- data$dni_specjalne[xxx]
    b1 <- which(a == 0) #indeksy dni normlanych
    b2 <- which(a != 0) #indeksy dni specjalnych
    
    c1 <- c(xxx)[b1]
    c2 <- c(xxx)[b2]
    
    d1 <- data$nrdnia[c1]
    d2 <- data$dni_specjalne[c2]
    
    e1 <- sapply(d1, function(x)
    {
      tib.dn[tib.dn$daytype == x, -1]
    })
    e1 <- t(e1)
    
    e2 <- sapply(d2, function(x)
    {
      tib.ds[tib.ds$daytype == x, -1]
    })
    e2 <- t(e2)
    
    f1 <- rep(NA, length(xxx))
    
    f1[b1] <- as.numeric(e1[, nr %/% 6])
    f1[b2] <- as.numeric(e2[, nr %/% 6])
    
    # pred.whole <- f1*vol.pred.list$fcast
    return(f1)
  }
  
  pred.whole <- foo(vol.pred.list$pred.wind, tib.dn, tib.ds)*vol.pred.list$fcast # podać jak się module dla K
  
  fit.whole <- foo(vol.pred.list$wind, tib.dn, tib.ds)*(vol.pred.list$model$fitted + vol.pred.list$trend)
  
  # f2 <- rep(NA, length(vol.pred.list$fcast))
  # 
  # f2[b1] <- as.numeric(e1[(length(vol.pred.list$wind) + 1):length(a), nr %/% 6])
  # f2[b2] <- as.numeric(e2[(length(vol.pred.list$wind) + 1):length(a), nr %/% 6])
  # 
  # fit.whole <- f2*(vol.pred.list$model$fitted + vol.pred.list$trend)
  
  return(list(raw.vol = data[, nr], fit.vol = vol.pred.list$model$fitted, 
              raw.k = data[, nr + 1], fit.k = fit.whole,
              pred.vol = vol.pred.list$fcast, pred.k = pred.whole, 
              plis = vol.pred.list))
}



write.means <- function(foo.list, data, typ, i)
{
  nam <- colnames(data)[foo.list$plis$nr]
  #####################################################################################
  infcrit <- t(data.frame(foo.list$plis$model$aic, foo.list$plis$model$aicc, foo.list$plis$model$bic))
  rownames(infcrit) <- c("aic", "aicc", "bic")
  colnames(infcrit) <- nam
  #####################################################################################
  errors.res.fitted <- t(as.data.frame(err(foo.list$plis$model$fitted, foo.list$plis$res)))#modelowanie
  
  errors.whole.predicted <- t(as.data.frame(err(foo.list$raw.k[foo.list$plis$pred.wind], foo.list$pred.k)))#prognoza
  
  colnames(errors.res.fitted) <- nam
  colnames(errors.whole.predicted) <- nam
  #####################################################################################
  ct <- cft(foo.list$plis$model, data)
  
  foo.list$plis$model$coef <- foo.list$plis$model$coef[complete.cases(ct)]
  ct <- ct[complete.cases(ct), ]
  
  coef.pval <- data.frame(foo.list$plis$model$coef, ct[, 2])
  colnames(coef.pval) <- c(paste0("coefs_", nam), paste0("p.val_", nam))
  #####################################################################################
  #REST
  df.rest.fitted <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.rest.fitted[foo.list$plis$wind, ] <- data.frame(as.numeric(foo.list$plis$res), as.numeric(foo.list$plis$model$fitted))
  colnames(df.rest.fitted) <- c(paste0("real_", nam), paste0("fitted_", nam))
  
  df.rest.pred <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.rest.pred[foo.list$plis$wind, ] <- data.frame(as.numeric(foo.list$plis$res), as.numeric(foo.list$plis$model$fitted))
  df.rest.pred[foo.list$plis$pred.wind, ] <- data.frame(as.numeric(data[foo.list$plis$pred.wind, foo.list$plis$nr] - foo.list$plis$pred.trend), 
                                                   as.numeric(foo.list$plis$fcast.res)) 
  colnames(df.rest.pred) <- c(paste0("real_", nam), paste0("fitted_", nam))
  #####################################################################################
  #WHOLE
  df.whole.fitted.vol <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.whole.fitted.vol[foo.list$plis$wind, ] <- data.frame(foo.list$raw.vol[foo.list$plis$wind], foo.list$plis$model$fitted+foo.list$plis$trend)
  colnames(df.whole.fitted.vol) <- c(paste0("real_volumen_", nam), paste0("fitted_volumen_", nam))
  
  df.whole.pred.vol <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.whole.pred.vol[foo.list$plis$wind, ] <- data.frame(foo.list$raw.vol[foo.list$plis$wind], as.numeric(foo.list$plis$model$fitted+foo.list$plis$trend))#BŁAD
  df.whole.pred.vol[foo.list$plis$pred.wind, ] <- data.frame(as.numeric(foo.list$raw.vol[foo.list$plis$pred.wind]), as.numeric(foo.list$pred.vol))
  colnames(df.whole.pred.vol) <- c(paste0("real_volumen_", nam), paste0("fitted_volumen_", nam))
  
  df.whole.fitted.k <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.whole.fitted.k[foo.list$plis$wind, ] <- data.frame(foo.list$raw.k[foo.list$plis$wind], foo.list$fit.k)
  colnames(df.whole.fitted.k) <- c(paste0("real_k_", nam), paste0("fitted_k_", nam))
  
  df.whole.pred.k <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.whole.pred.k[foo.list$plis$wind, ] <- data.frame(foo.list$raw.k[foo.list$plis$wind], foo.list$fit.k)
  df.whole.pred.k[foo.list$plis$pred.wind, ] <- data.frame(foo.list$raw.k[foo.list$plis$pred.wind], foo.list$pred.k)
  colnames(df.whole.pred.k) <- c(paste0("real_k_", nam), paste0("fitted_k_", nam))
  #####################################################################################
  dir.create(file.path(getwd(), "files"), showWarnings = FALSE)
  
  if(!file.exists(file.path(getwd(), "files", paste0("fitted_means_", as.character(typ), ".xlsx"))))
  {
    wb <- createWorkbook()
    sheet.rf.real <- addWorksheet(wb, "real-fitted whole")
    sheet.rf.rest <- addWorksheet(wb, "real-fitted rest")
    sheet.error <- addWorksheet(wb, "errors")
    sheet.coef <- addWorksheet(wb, "coefs")
    sheet.infcrit <- addWorksheet(wb, "infcrit")
    
    writeData(wb = wb, sheet = sheet.rf.real, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.rf.rest, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.error, data.frame("names" = rownames(errors.res.fitted)), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.infcrit, data.frame("names" = rownames(infcrit)), rowNames = FALSE, startCol = 1)
    # writeData(wb = wb, sheet = sheet.coef, data.frame("names" = rownames(ct)), rowNames = FALSE, startCol = 1)
  }
  else
  {
    wb <- loadWorkbook(file.path(getwd(), "files", paste0("fitted_means_", as.character(typ), ".xlsx")))
    sheet.rf.real <- "real-fitted whole"
    sheet.rf.rest <- "real-fitted rest"
    sheet.error <- "errors"
    sheet.coef <- "coefs"
    sheet.infcrit <-"infcrit"
  }
  
  writeData(wb = wb, sheet = sheet.rf.real, cbind(df.whole.fitted.vol, df.whole.fitted.k), rowNames = FALSE, startCol = ((i * 4) - 2))
  writeData(wb = wb, sheet = sheet.rf.rest, df.rest.fitted, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.error, errors.res.fitted, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.infcrit, infcrit, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.coef, coef.pval, rowNames = TRUE, startCol = i * 3 - 2)
  
  saveWorkbook(wb, file = file.path(getwd(), "files", paste0("fitted_means_", as.character(typ), ".xlsx")), overwrite = TRUE)
  
  #####################################################################################
  
  if(!file.exists(file.path(getwd(), "files", paste0("pred_means_", as.character(typ), ".xlsx"))))
  {
    wb <- createWorkbook()
    sheet.rf.real <- addWorksheet(wb, "real-fitted whole")
    sheet.rf.rest <- addWorksheet(wb, "real-fitted rest")
    sheet.error <- addWorksheet(wb, "errors")
    sheet.coef <- addWorksheet(wb, "coefs")
    sheet.infcrit <- addWorksheet(wb, "infcrit")
    
    writeData(wb = wb, sheet = sheet.rf.real, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.rf.rest, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.error, data.frame("names" = rownames(errors.res.fitted)), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.infcrit, data.frame("names" = rownames(infcrit)), rowNames = FALSE, startCol = 1)
    # writeData(wb = wb, sheet = sheet.coef, data.frame("names" = rownames(ct)), rowNames = FALSE, startCol = 1)
  }
  else
  {
    wb <- loadWorkbook(file.path(getwd(), "files", paste0("pred_means_", as.character(typ), ".xlsx")))
    sheet.rf.real <- "real-fitted whole"
    sheet.rf.rest <- "real-fitted rest"
    sheet.error <- "errors"
    sheet.coef <- "coefs"
    sheet.infcrit <-"infcrit"
  }
  
  writeData(wb = wb, sheet = sheet.rf.real, cbind(df.whole.pred.vol, df.whole.pred.k), rowNames = FALSE, startCol = ((i * 4) - 2))
  writeData(wb = wb, sheet = sheet.rf.rest, df.rest.pred, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.error, errors.whole.predicted, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.infcrit, infcrit, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.coef, coef.pval, rowNames = TRUE, startCol = i*2)
  
  saveWorkbook(wb, file = file.path(getwd(), "files", paste0("pred_means_", as.character(typ), ".xlsx")), overwrite = TRUE)
}



# aaa <- selfarima.means(data2, start.date = "2010-03-01", end.date = "2012-02-29", nr = 7, type = "xreg", xr = as.matrix(ds), n = 14, seas = TRUE)

# plot(aaa$raw.k[aaa$plis$wind], type = "l")
# lines(aaa$pred.k, col = "red")

# write.means(aaa, data2, typ = "TEST", i = 1)
