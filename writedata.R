df.xl.write <- function(foo.list, data, typ, mnk = FALSE)
{
  nr <- foo.list[[3]]
  aaa <- t(data.frame("aic" = foo.list[[2]]$aic, "aicc" = foo.list[[2]]$aicc, "bic" = foo.list[[2]]$bic))
  colnames(aaa) <- "value"
  
  bbb <- t(as.data.frame(err(foo.list)[-5]))
  colnames(bbb) <- "error"
  
  ct <- cft(foo.list, data)
  
  foo.list[[2]]$coef <- foo.list[[2]]$coef[complete.cases(ct)]
  ct <- ct[complete.cases(ct), ]
  
  xxx <- list(data.frame("coefs" = foo.list[[2]]$coef, "p.val" = ct[, 2]), aaa, bbb)
  
  dir.create(file.path(getwd(), "files"), showWarnings = FALSE)
  
  if(!file.exists(file.path(getwd(), "files", paste0("result_",as.character(typ), ".xlsx"))))
  {
    wb <- createWorkbook()
  }
  else
  {
    wb <- loadWorkbook(file.path(getwd(), "files", paste0("result_",as.character(typ), ".xlsx"))) 
  }
  
  sheet <- addWorksheet(wb, as.character(names(data)[nr]))
  
  ddff <- data.frame(as.numeric(foo.list[[1]]), as.numeric(foo.list[[2]]$fitted))
  colnames(ddff) <- c("real", "fitted")
  if(mnk == TRUE)
  {
    fitt <- foo.list[[4]]
  }
  else
  {
    fitt <- foo.list[[4]]$mean
  }
  ddff <- rbind(ddff, data.frame("real" = NA, "fitted" = as.numeric(fitt)))
  # colnames(ddff) <- c("real", "fitted")
  
  writeData(wb = wb, sheet = sheet, ddff, rowNames = FALSE)
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
  
  saveWorkbook(wb, file = file.path(getwd(), "files", paste0("result_",as.character(typ), ".xlsx")), overwrite = TRUE)
}
