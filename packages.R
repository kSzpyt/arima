load.lib<-c("bsts", "readxl", "plyr", "fastDummies","forecast","ggplot2","lmtest",
            "gridExtra","forecast","openxlsx","dplyr","lattice", 'stringr', 
            "data.table", "TSA", "tseries", "dygraphs", "car", "lubridate", "beepr")


install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)


# install.packages("minpack.lm_1.2-1.tar.gz", repos = NULL)
library(minpack.lm)
