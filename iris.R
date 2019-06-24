setwd('/home/girija/Datasets')
iris<-read.csv('Iris.csv',header=FALSE,sep=',')
colnames(iris)<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
results <- lm(iris$Petal.Width ~ iris$Petal.Length, data=iris)
summary(results)

par(mai=c(1,1,0,0))
plot(iris$Petal.Length, iris$Petal.Width, pch=".")
abline(results, lwd=2)
# Plot distances between points and the regression line (i.e. residuals)
predictedY <- predict(results)
segments(iris$Petal.Length, iris$Petal.Width, iris$Petal.Length,
         predictedY, col="red")

new <- data.frame(Petal.Length=seq(-3, 3, 0.5))
predict(results, new)

formula <- "Sepal.Width ~ Petal.Length + Petal.Width"
y <- iris$Sepal.Width
fit <- lm(as.formula(formula), data=iris[,1:4])
summary(fit)

pred <- predict(fit)
# Add regression between predicted and observed
fit2 <- lm(pred ~ y)
# Plot predicted versus observed
title <- paste0("Formula: ", formula, "; R-squared: ", round(summary(fit2)$r.squared,
                                                             3))
plot(y, pred, xlim=range(c(y, pred)), ylim=range(c(y, pred)), xlab="observed",
     ylab="predicted", main=title)
# Add regression line
abline(fit2, lwd=2)
