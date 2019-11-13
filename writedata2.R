df.xl.write2 <- function(foo.list, data, typ, i)
{
  nam <- colnames(data)[foo.list$nr]
  #####################################################################################
  infcrit <- t(data.frame(foo.list$model$aic, foo.list$model$aicc, foo.list$model$bic))
  rownames(infcrit) <- c("aic", "aicc", "bic")
  colnames(infcrit) <- nam
  #####################################################################################
  errors.res.fitted <- t(as.data.frame(err(foo.list$model$fitted, foo.list$res)))#modelowanie

  errors.whole.predicted <- t(as.data.frame(err(data[foo.list$pred.wind, foo.list$nr], foo.list$fcast)))#prognoza
  
  colnames(errors.res.fitted) <- nam
  colnames(errors.whole.predicted) <- nam
  #####################################################################################
  ct <- cft(foo.list$model, data)
  
  foo.list$model$coef <- foo.list$model$coef[complete.cases(ct)]
  ct <- ct[complete.cases(ct), ]
  
  coef.pval <- data.frame(foo.list$model$coef, ct[, 2])
  colnames(coef.pval) <- c(paste0("coefs_", nam), paste0("p.val_", nam))
  #####################################################################################
  df.rest <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.rest[foo.list$wind, ] <- data.frame(as.numeric(foo.list$res), as.numeric(foo.list$model$fitted))
  colnames(df.rest) <- c(paste0("real_", nam), paste0("fitted_", nam))
  
  df.whole <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.whole[foo.list$wind, ] <- data.frame(as.numeric(foo.list$dat), as.numeric(foo.list$model$fitted+foo.list$trend))
  colnames(df.whole) <- c(paste0("real_", nam), paste0("fitted_", nam))#tu dopisać nazwę zmiennej
  
  #DOPYTAĆ JAK POKAZAĆ RESZTY
  # df.rest.pred <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  # df.rest.pred[foo.list$wind, ] <- data.frame(as.numeric(foo.list$res), as.numeric(foo.list$model$fitted))
  # df.rest.pred[foo.list$pred.wind, ] <- data.frame(as.numeric(foo.list$res), as.numeric(foo.list$model$fitted))
  # colnames(df.real) <- c(paste0("real_", nam), paste0("fitted_", nam))
  
  df.whole.pred <- data.frame(rep(0, dim(data)[1]), rep(0, dim(data)[1]))
  df.whole.pred[foo.list$wind, ] <- data.frame(as.numeric(foo.list$dat), as.numeric(foo.list$model$fitted+foo.list$trend))
  df.whole.pred[foo.list$pred.wind, ] <- data.frame(as.numeric(data[foo.list$pred.wind, foo.list$nr]), as.numeric(foo.list$fcast))
  colnames(df.whole.pred) <- c(paste0("real_", nam), paste0("fitted_", nam))#tu dopisać nazwę zmiennej
  #####################################################################################
  dir.create(file.path(getwd(), "files"), showWarnings = FALSE)
  
  if(!file.exists(file.path(getwd(), "files", paste0("fitted_", as.character(typ), ".xlsx"))))
  {
    wb <- createWorkbook()
    sheet.rf.real <- addWorksheet(wb, "real-fitted rest+trend")
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
    wb <- loadWorkbook(file.path(getwd(), "files", paste0("fitted_", as.character(typ), ".xlsx")))
    sheet.rf.real <- "real-fitted rest+trend"
    sheet.rf.rest <- "real-fitted rest"
    sheet.error <- "errors"
    sheet.coef <- "coefs"
    sheet.infcrit <-"infcrit"
  }
  #####################################################################################
  writeData(wb = wb, sheet = sheet.rf.real, df.whole, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.rf.rest, df.rest, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.error, errors.res.fitted, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.infcrit, infcrit, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.coef, coef.pval, rowNames = TRUE, startCol = i*2)
  
  saveWorkbook(wb, file = file.path(getwd(), "files", paste0("fitted_", as.character(typ), ".xlsx")), overwrite = TRUE)
  
  
  if(!file.exists(file.path(getwd(), "files", paste0("predicted_", as.character(typ), ".xlsx"))))
  {
    wb <- createWorkbook()
    sheet.rf.real <- addWorksheet(wb, "real-fitted rest+trend")
    # sheet.rf.rest <- addWorksheet(wb, "real-fitted rest")
    sheet.error <- addWorksheet(wb, "errors")
    sheet.coef <- addWorksheet(wb, "coefs")
    sheet.infcrit <- addWorksheet(wb, "infcrit")
    
    writeData(wb = wb, sheet = sheet.rf.real, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    # writeData(wb = wb, sheet = sheet.rf.rest, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.error, data.frame("names" = rownames(errors.res.fitted)), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.infcrit, data.frame("names" = rownames(infcrit)), rowNames = FALSE, startCol = 1)
    # writeData(wb = wb, sheet = sheet.coef, data.frame("names" = rownames(ct)), rowNames = FALSE, startCol = 1)
    
  }
  else
  {
    wb <- loadWorkbook(file.path(getwd(), "files", paste0("predicted_", as.character(typ), ".xlsx"))) 
    sheet.rf.real <- "real-fitted rest+trend"
    # sheet.rf.rest <- "real-fitted rest"
    sheet.error <- "errors"
    sheet.coef <- "coefs"
    sheet.infcrit <-"infcrit"
  }
  
  #####################################################################################
  writeData(wb = wb, sheet = sheet.rf.real, df.whole.pred, rowNames = FALSE, startCol = i*2)
  # writeData(wb = wb, sheet = sheet.rf.rest, df.rest, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.error, errors.whole.predicted, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.infcrit, infcrit, rowNames = FALSE, startCol = i + 1)
  writeData(wb = wb, sheet = sheet.coef, coef.pval, rowNames = TRUE, startCol = i*3-2)
  
  saveWorkbook(wb, file = file.path(getwd(), "files", paste0("predicted_", as.character(typ), ".xlsx")), overwrite = TRUE)
}