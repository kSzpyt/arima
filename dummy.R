library(fastDummies)

d <- data2$dni_specjalne
d <- as.factor(d)
d
d <- dummy_cols(d)
dim(d)
d <- d[, -c(1:2)]
names(d) <- paste0("ds", 1:5)
d <- as.matrix(d)
d
