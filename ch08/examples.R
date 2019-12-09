# 8.2
data(women)
summary(women)

fit <- lm(weight ~ height, data=women)
summary(fit)

coefficients(fit)
confint(fit)
fitted(fit)
residuals(fit)
anova(fit)
vcov(fit)
AIC(fit)
# plot(fit) # need to hit enter for each plot
predict(fit, data.frame(height=120))

fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

fit3 <- lm(weight ~ height + I(height^2) + I(height^3), data=women)
summary(fit3)

plot(women$height,women$weight, xlab="Height (in inches)", ylab="Weight (in lbs)")
lines(women$height, fitted(fit), lty=2)
lines(women$height, fitted(fit2))
lines(women$height, fitted(fit3), lty=5)

library(car)
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")

states <- as.data.frame(
  state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
cor(states)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

library(car)
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)

library(effects)
plot(effect("hp:wt", fit,, list(wt=c(2.2,3.2,4.2))), multiline=TRUE)

# 8.3
states <- as.data.frame(
  state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fit <- lm(Murder~Population+Illiteracy+Income+Frost, data=states)
summary(fit)
confint(fit)

fit <- lm(weight~height, data=women)
par(mfrow=c(2,2))
plot(fit)

fit2 <- lm(weight~height+I(height^2), data=women)
plot(fit2)

fit3 <- lm(weight~height+I(height^2)+I(height^3), data=women)
plot(fit3)

newfit2 <- lm(weight~height+I(height^2), data=women[-c(13,14,15),])
plot(newfit2)

newfit3 <- lm(weight~height+I(height^2)+I(height^3), data=women[-c(13,14,15),])
plot(newfit3)

library(car)
states <- as.data.frame(
  state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fit <- lm(Murder~Population+Illiteracy+Income+Frost, data=states)

par(mfrow=c(1,1))
qqPlot(fit, labels=row.names(states),
       id.method="identify", simulate=TRUE, main="Q-Q Plot")

states["Nevada",]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(fit)

durbinWatsonTest(fit)
durbinWatsonTest(fit,simulate=FALSE)

crPlots(fit)

ncvTest(fit)

spreadLevelPlot(fit)

library(gvlma)
model <- gvlma(fit)
summary(model)

vif(fit)
sqrt(vif(fit)) > 2