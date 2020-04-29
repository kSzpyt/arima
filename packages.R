load.lib<-c("bsts", "readxl", "plyr", "fastDummies","forecast","ggplot2","lmtest",
            "gridExtra","forecast","openxlsx","dplyr","lattice", 'stringr', 
            "data.table", "TSA", "tseries", "dygraphs", "car", "lubridate", "beepr")


install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)


# install.packages("minpack.lm_1.2-1.tar.gz", repos = NULL)
library(minpack.lm)

loading <- function(i.actual, i.max)
{
  if(i.actual > i.max) stop("Actual value is greater than max value")
  
  p <- 100 * i.actual/i.max
  p <- round(p, 2)
  
  perc <- 1:50 * 2
  perc <- length(which((p > perc)))
  
  pasek <- "|"
  
  if(perc != 50)
  {
    for (x in 1:ifelse(perc < 2, 1, perc)) {
      pasek <- paste0(pasek, "=")
    }
    
    for (x in 1:(50 - perc)) {
      pasek <- paste0(pasek, " ")
    }
    
  }
  else 
  {
    for (x in 1:perc) {
      pasek <- paste0(pasek, "=")
    }
  }
  
  pasek <- paste0(pasek, "|")
  
  message("Loading: ", pasek, paste0(p, "%"), "\n")
}
