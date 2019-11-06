library(dygraphs)
library(tseries)
library(dplyr)
library(TSA)

plots <- function(data, n.start = 7, n.stop = dim(df)[2], log = FALSE)
{
  dir.create(file.path(getwd(), "plots"))
  df <- data
  
  for (x in n.start:n.stop)
  {
    if(log == TRUE)
    {
      df[, x] <- log(df[, x])
    }
    startW <- as.numeric(strftime(head(df$data[n.start:n.stop], 1), format = "%W"))
    startD <- as.numeric(strftime(head(df$data[n.start:n.stop], 1) + 1, format =" %w"))
    
    ts1 <- ts(df[, x], start = c(startW, startD), frequency = 7)

    a <- adf.test((df[, x]), alternative = "stationary")
    b <- kpss.test((df[, x]))
    
    dir.create(file.path(getwd(), "plots", as.character(names(data2)[x])))
    
    png(paste0(getwd(), "/plots/", as.character(names(data2)[x]), "/", as.character(names(data2)[x]), ".png"))
    
    layout(mat = matrix(c(1,2, 3, 2), ncol = 2))
    acf((df[, x]), lag.max = 100, main = c("ACF ", as.character(names(data2)[x])))
    mtext(paste("adf.test$p.value=", as.character(a$p.value)), side = 3, adj = 0)
    
    periodogram((df[, x]), main = c("Periodogram ", as.character(names(data2)[x])))
    
    pacf((df[, x]), lag.max = 100, main = c("PACF ", as.character(names(data2)[x])))
    mtext(paste("kpss.test$p.value=", as.character(b$p.value)), side = 3, adj = 0)
    
    dev.off()
    
    png(paste0(getwd(), "/plots/", as.character(names(data2)[x]), "/", "ACF ", as.character(names(data2)[x]), ".png"))
    acf((df[, x]), lag.max = 100, main = c("ACF ", as.character(names(data2)[x])))
    dev.off()

    png(paste0(getwd(), "/plots/", as.character(names(data2)[x]), "/", "PACF ", as.character(names(data2)[x]), ".png"))
    pacf((df[, x]), lag.max = 100, main = c("PACF ", as.character(names(data2)[x])))
    dev.off()

    png(paste0(getwd(), "/plots/", as.character(names(data2)[x]), "/", "Periodogram ", as.character(names(data2)[x]), ".png"))
    periodogram((df[, x]), main = c("Periodogram ", as.character(names(data2)[x])))
    dev.off()
  }
}
plots(data2)

