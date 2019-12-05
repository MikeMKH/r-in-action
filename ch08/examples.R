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