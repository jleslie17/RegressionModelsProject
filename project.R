data("mtcars")
library(ggplot2)
require(GGally)
cars <- mtcars
summary(cars)
str(cars)
cars$trans <- ifelse(cars$am == 0, "automatic", "manual")

##Figure 1
##0 => automatic, 1 => manual
g1 <- ggplot(cars, aes(x = as.factor(trans), y = mpg))
p1 <- g1 + geom_boxplot() +
        labs(title = "Figure 1: Fuel efficiences") +
        labs(x = "Transmission Type")
p1

##Look for strongest correlations
cor(cars$mpg, cars[,-12])
cor(cars$am, cars[,-12])


##Playing with models, none of variables are factors
fit1 <- lm(mpg ~ 1, data = cars)
add1(fit1, scope =cars[,-12], test = "F")

fit2 <- lm(mpg ~ wt, data = cars)
add1(fit2, scope = cars[,-12], test = "F")

##shouldn't use fit3...cyl not a factor variable
fit3 <- lm(mpg ~ wt + cyl, data = cars)
add1(fit3, scope = cars[,-12], test = "F")
summary(fit3)

fit3f <- lm(mpg ~ wt + as.factor(cyl), data = cars)
summary(fit3f)
anova(fit1, fit2, fit3f)
anova(fit1, fit2, fit3f, fit4)

fit3I <- lm(mpg ~ wt * as.factor(cyl), data = cars)
anova(fit1, fit2, fit3I)

##Now look for the effect of the transmission type
fit4 <- lm(mpg ~ wt + as.factor(cyl) + am, data = cars)
summary(fit4)##This shows that a manual trans increases MPG by 0.15
anova(fit3f, fit3I)

##Now try a log fit for wt
fit5 <- lm(mpg ~ log(wt) + as.factor(cyl) + am, data = cars)
summary(fit5)
anova(fit4, fit5)
##Model fit to log wt I think fits better, but I'm not going to bother

##Diagnostics on model fit4
plot(mpg ~ wt, data = cars, pch = 21, col = "black", bg =as.factor(cyl))
##Plot means of three groups
abline(h = mean(cars$mpg[cars$cyl == 4]), lwd = 3)
abline(h = mean(cars$mpg[cars$cyl == 6]), lwd = 3, col = "red")
abline(h = mean(cars$mpg[cars$cyl == 8]), lwd = 3, col = "dark green")
##Plot linear fit of mpg vs wt + cyl
abline(lm(mpg~wt+as.factor(cyl), data = cars), lwd = 2, col = "blue")
abline(lm(mpg~wt+as.factor(cyl)+ am, data = cars), lwd = 2, col = "purple")
abline(fit4$coef[1], fit4$coef[2])
abline(fit4$coef[1] + fit4$coef[3], fit4$coef[2], col = "red")
abline(fit4$coef[1] + fit4$coef[4], fit4$coef[2], col = "red")
abline(fit4, lwd = 5)
abline(fit3I)

##Residual diagnostics
par(mfrow = c(2,2)); plot(fit4)
round(hatvalues(fit4), 4)
round(dfbetas(fit4), 2)
par(mfrow = c(1,1))
plot(predict(fit4), resid(fit4), pch = 21)
abline(h = 0, lwd = 3)
plot(predict(fit3I), resid(fit3I), pch = 21)
range(resid(fit4))
range(resid(fit3I))

plot(mpg~wt, data = cars)
abline(lm(mpg~wt+am, data = cars))
abline(fit2)
summary(fit2)

plot(fit4, main = "Fig 2")

##Trying ggplot
cars$cyl <- factor(cars$cyl)
g2 <- ggplot(cars, aes(x = wt, y = mpg, colour = cyl))
p2 <- g2 + geom_point(size = 3) + 
        geom_smooth(method = lm, se = T, fullrange = F) +
        geom_abline(intercept = fit2$coef[1], slope = fit2$coef[2]) +
        geom_abline(intercept = fit2$coef[1] + fit4$coef[5], 
                    slope = fit2$coef[2], colour = "blue")
p2

g3 <- ggplot(cars, aes(x = wt, y = mpg, colour = trans, shape = cyl))
p3 <- g3 + geom_point(size = 3) + 
        geom_abline(intercept = fit4$coef[1], slope = fit4$coef[2]) +
        geom_abline(intercept = fit4$coef[1] + fit4$coef[5], 
                    slope = fit4$coef[2], colour = "blue")
p3

cars$cyl <- factor(cars$cyl)
g2 <- ggplot(cars, aes(x = wt, y = mpg, colour = cyl))
P2 <- g2 + geom_point(size = 3) + stat_smooth(method = lm, fullrange = T)
P2
library(grid)
multiplot(p2, p3, cols = 2)


##T-test to give 95% confidence interval
summary(fit4)
str(cars)
control <- as.numeric(fit4$coef[1])
TStat <- dt(control, df = 28)
TStat

0.15 + c(-1,1)* qt(0.975, df = 27) *1.3
summary((fit4))
