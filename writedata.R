df.xl.write <- function(foo.list, data, typ, mnk = FALSE, i)
{
  nam <- colnames(data)[foo.list[[3]]]
  #####################################################################################
  nr <- foo.list[[3]]
  aaa <- t(data.frame("aic" = foo.list[[2]]$aic, "aicc" = foo.list[[2]]$aicc, "bic" = foo.list[[2]]$bic))
  colnames(aaa) <- "value"
  
  bbb <- t(as.data.frame(err(foo.list)[-5]))
  colnames(bbb) <- "error"
  #####################################################################################
  ct <- cft(foo.list, data)
  
  foo.list[[2]]$coef <- foo.list[[2]]$coef[complete.cases(ct)]
  ct <- ct[complete.cases(ct), ]
  
  xxx <- list(data.frame("coefs" = foo.list[[2]]$coef, "p.val" = ct[, 2]), aaa, bbb)
  #####################################################################################
  df.real <- data.frame(as.numeric(foo.list[[1]]), as.numeric(foo.list[[4]]))
  colnames(df.real) <- c(paste0("real_", nam), paste0("fitted_", nam))#tu dopisać nazwę zmiennej
  
  df.rest <- data.frame(as.numeric(foo.list[[7]]), as.numeric(foo.list[[8]]))
  colnames(df.rest) <- c(paste0("real_", nam), paste0("fitted_", nam))#tu dopisać nazwę zmiennej
  
  # if(mnk == TRUE)
  # {
  #   fitt <- foo.list[[4]]
  # }
  # else
  # {
  #   fitt <- foo.list[[4]]$mean
  # }
  # df.real <- rbind(df.real, data.frame("real" = NA, "fitted" = as.numeric(fitt)))
  # colnames(df.real) <- c("real", "fitted")
  #####################################################################################
  dir.create(file.path(getwd(), "files"), showWarnings = FALSE)
  
  if(!file.exists(file.path(getwd(), "files", paste0(as.character(typ), ".xlsx"))))
  {
    wb <- createWorkbook()
    sheet.rf.real <- addWorksheet(wb, "real-fitted rest+trend")
    sheet.rf.rest <- addWorksheet(wb, "real-fitted rest")
    sheet.error <- addWorksheet(wb, "errors")
    sheet.coef <- addWorksheet(wb, "coefs")
    
    writeData(wb = wb, sheet = sheet.rf.real, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
    writeData(wb = wb, sheet = sheet.rf.rest, data.frame("data" = data$data), rowNames = FALSE, startCol = 1)
  }
  else
  {
    wb <- loadWorkbook(file.path(getwd(), "files", paste0(as.character(typ), ".xlsx"))) 
  }
  #####################################################################################
  writeData(wb = wb, sheet = sheet.rf.real, df.real, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.rf.rest, df.rest, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.error, df.real, rowNames = FALSE, startCol = i*2)
  writeData(wb = wb, sheet = sheet.coef, df.real, rowNames = FALSE, startCol = i*2)
  currRow <- 1
  for(i in 1:length(xxx))
  {
    writeData(wb = wb,
              sheet = sheet,
              x = xxx[[i]],
              rowNames = TRUE, 
              startRow = currRow,
              startCol = 5)
    
    currRow <- currRow + nrow(xxx[[i]]) + 2 
  }
  
  saveWorkbook(wb, file = file.path(getwd(), "files", paste0(as.character(typ), ".xlsx")), overwrite = TRUE)
}
