# 13.1
summary(mtcars)

glm(family=binomial(link="logit"),
    am~cyl+disp+hp+wt, data=mtcars)
glm(family=gaussian(link="identity"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=inverse.gaussian(link="1/mu^2"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=poisson(link="log"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=quasi(link="identity", variance="constant"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=quasibinomial(link="logit"),
    am~cyl, data=mtcars)
glm(family=quasipoisson(link="log"),
    qsec~cyl+disp+hp+wt, data=mtcars)

fit <- glm(family=inverse.gaussian(link="1/mu^2"),
           qsec~cyl+disp+hp+wt, data=mtcars)
summary(fit)
coefficients(fit)
confint(fit)
residuals(fit)
anova(fit)
# plot(fit)
predict(fit)
deviance(fit)
df.residual(fit)

# 13.2
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)

Affairs$ynaffair[Affairs$affairs  > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(
  Affairs$ynaffair, levels=c(0,1), labels=c("No","Yes"))
table(Affairs$ynaffair)

fit.full <- glm(
  ynaffair ~ gender + age + yearsmarried + children +
    religiousness + education + occupation + rating,
  data=Affairs, family=binomial())
summary(fit.full)

fit.reduced <- glm(
  ynaffair ~ age + yearsmarried + religiousness + rating,
  data=Affairs, family=binomial())
summary(fit.reduced)

anova(fit.reduced, fit.full, test="Chisq")

coefficients(fit.reduced)
exp(coefficients(fit.reduced))

confint(fit.reduced)
exp(confint(fit.reduced))

(testdata <- data.frame(
  rating=c(1, 2, 3, 4, 5),
  age=mean(Affairs$age),
  yearsmarried=mean(Affairs$yearsmarried),
  religiousness=mean(Affairs$religiousness)))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

(testdata <- data.frame(
  rating=mean(Affairs$age),
  age=seq(17,107,5),
  yearsmarried=mean(Affairs$yearsmarried),
  religiousness=mean(Affairs$religiousness)))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

(testdata <- data.frame(
  rating=mean(Affairs$age),
  age=mean(Affairs$age),
  yearsmarried=seq(0,40,3),
  religiousness=mean(Affairs$religiousness)))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

(testdata <- data.frame(
  rating=mean(Affairs$age),
  age=mean(Affairs$age),
  yearsmarried=mean(Affairs$yearsmarried),
  religiousness=seq(1,5,1)))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

deviance(fit.reduced)/df.residual(fit.reduced)

fit <- glm(ynaffair ~ age + yearsmarried + religiousness + rating,
           family=binomial(), data=Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness + rating,
              family=quasibinomial(), data=Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual,
       fit$df.residual, lower = F)

# 13.3
data(breslow.dat, package="robust")
names(breslow.dat)
data <- breslow.dat[c(6,7,8,10)]
summary(data)

par(mfrow=c(1,2))
attach(data)
hist(sumY, breaks=20, xlab="Seizure Count",
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")
par(opar)
detach(data)

fit  <- glm(sumY ~ Base+Age+Trt, data=data, family=poisson())
summary(fit)

coef(fit)
exp(coef(fit))
deviance(fit)/df.residual(fit)

library(qcc)
qcc.overdispersion.test(data$sumY, type="poisson")

fit2 <- glm(sumY ~ Base+Age+Trt, data=data, family=quasipoisson())
summary(fit2)

library(pscl)
fit3 <- zeroinfl(sumY ~ Base+Age+Trt, data=data, dist="poisson")
summary(fit3)

library(robust)
fit4 <- glmRob(sumY ~ Base+Age+Trt, data=data,
               family=poisson(), method="cubif")
summary(fit4)