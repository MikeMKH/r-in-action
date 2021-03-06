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

# 8.4
library(car)
states <- as.data.frame(
  state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fit <- lm(Murder~Population+Illiteracy+Income+Frost, data=states)

outlierTest(fit)

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  #identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

avPlots(fit, ask=FALSE, id.method="identify")

influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# 8.5
library(car)
summary(powerTransform(states$Murder))

boxTidwell(Murder~Population+Illiteracy,data=states)

fit <- lm(Murder~Population+Illiteracy+Income+Frost, data=states)
durbinWatsonTest(fit)
crPlots(fit)
ncvTest(fit)
outlierTest(fit)
spreadLevelPlot(fit)

fit <- lm(I(sqrt(Murder))~Population+Illiteracy+Income+Frost, data=states)
durbinWatsonTest(fit)
crPlots(fit)
ncvTest(fit)
outlierTest(fit)
spreadLevelPlot(fit)

spreadLevelPlot(Population+Illiteracy+Income+Frost~Murder,data=states)

# 8.6
states <- as.data.frame(
  state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])

fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)

anova(fit2, fit1)
AIC(fit1, fit2)

library(MASS)
fit <- lm(Murder ~ ., data=states)
stepAIC(fit, direction = "backward")
stepAIC(fit, direction = "both")

library(leaps)
model <- regsubsets(Murder ~ ., data=states, nbest=4)
plot(model, scale="adjr2")

library(car)
subsets(model, statistic="cp",
        main="Cp Plot for All Subsets Regression", legend=FALSE)
abline(1,1,lty=2,col="red")

# 8.7
shrinkage <- function(fit, k=10){
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

states <- as.data.frame(
  state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy, data=states)
fit2 <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit1)
shrinkage(fit2)

zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)

relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="blue")