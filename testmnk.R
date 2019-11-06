data.frame(coefs = na.omit(b[[2]]$coef), p.val = cft(b, data2)[, 2])

b[[2]]

coeftest(b[[2]])
coeftest(b[[2]]$coef)



fit <- lm(data2[, 7] ~ data2$key)

# plot(fit)
print(fit)
plot(data2[, 7] ~ as.numeric(data2$key), type = "l")
abline(fit, col = "red")

fit$coefficients

max <- dim(data2)[1]
oo <- 50
wind <- (max - (oo - 1)):max

int <- fit$coefficients[1]
beta <- fit$coefficients[2]
h <- (length(data2$key) + 1):((length(data2$key) + 52))

model.res <- selfarima.mnk(data2, oo = oo, nr = 7, n = 52)
res.pred <- model.res[[4]]
res.pred <- as.numeric(res.pred$mean)

plot(model.res[[4]])

res <- model.res[[2]]$residuals
fit$residuals[wind]



fitted <- int + (beta * wind) + fit$residuals[wind]
err(list(data2[wind, 7], fitted), custom = TRUE)


plot(data2[wind, 7], type = "l")
lines(fitted, col = "red")




pr <- (beta*(h)+int)+res
plot(pr, type = "l")
plot(c(data2[, 7]), type = "l")

# err(list(data2[wind, 7], pr), custom = TRUE) #nie tak, easy
library(lattice)

ddff <- data.frame(x = c(data2[wind, 7], pr), y = c(rep("a", oo), rep("b", length(h))))

xda <- xyplot(x ~ 1:length(x), data = ddff, group = y, type = "b", col = c("black", "red"))
xda



sad <- selfarima.mnk(data2, nr = 7, n = 52)
sad[[5]]
sad[[6]]
plot(sad[[4]])
