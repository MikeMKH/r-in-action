# 9.3
library(multcomp)
attach(cholesterol)
table(trt)
aggregate(response, by=list(trt), FUN=mean)
aggregate(response, by=list(trt), FUN=sd)
fit <- aov(response ~ trt)
summary(fit)

library(gplots)
plotmeans(response ~ trt,
          xlab="Treatment", ylab="Response", main="Mean Plot (95% CI)")
detach(cholesterol)

TukeyHSD(fit)

par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))

library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(trt="Tukey"))
summary(tuk)
plot(cld(tuk, level=0.5), col="lightgrey")

library(car)
qqPlot(lm(response ~ trt, data=cholesterol), simulate=TRUE,
       main="Q-Q Plot", labels=FALSE)

bartlett.test(response ~ trt, data=cholesterol)
fligner.test(response ~ trt, data=cholesterol)
outlierTest(fit)

# 9.4
data(litter, package="multcomp")
attach(litter)

table(dose)
aggregate(weight, by=list(dose), FUN=mean)

fit <- aov(weight ~ gesttime + dose)
summary(fit)

library(effects)
effect("dose", fit)

library(multcomp)
comp <- rbind("none to dose" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=comp)))

fit2 <- aov(weight ~ gesttime * dose)
summary(fit2)

library(HH)
ancova(weight ~ gesttime + dose, data=litter) # need to give data
ancova(weight ~ gesttime * dose, data=litter) # need to give data

detach(litter)

# 9.5
attach(ToothGrowth)
table(supp, dose)

aggregate(len, by=list(supp, dose), FUN=mean)
aggregate(len, by=list(supp, dose), FUN=sd)

dose <- factor(dose)
fit <- aov(len ~ supp * dose)
summary(fit)

interaction.plot(dose, supp, len,
                 type="b", col=c("red", "blue"), pch=c(16, 18),
                 main="Interaction between Dose and Supplement Type")

library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "),
          connect=list(c(1,3,5), c(2,4,6)),
          col=c("red", "darkgreen"),
          main = "Interaction Plot with 95% CIs",
          xlab="Treatment and Dose Combination")

library(HH)
interaction2wt(len ~ supp * dose)

detach(ToothGrowth)

# 9.6
summary(CO2)
pairs(CO2)

CO2$conc <- factor(CO2$conc)
chilled <- subset(CO2, Treatment=="chilled")
summary(chilled)

fit <- aov(uptake ~ conc*Type + Error(Plant/(conc)), chilled)
summary(fit)

par(las=2)
par(mar=c(10,4,4,2))
with(chilled,
     interaction.plot(
       conc, Type, uptake,
       type="b", col=c("red","blue"), pch=c(16,18),
       main="Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type*conc, data=chilled, col=(c("gold", "green")),
         main="Chilled Quebec and Mississippi Plants",
         ylab="Carbon dioxide uptake rate (umol/m^2 sec)")

library(nlme)
fit2 <- gls(uptake ~ conc*Type, data=chilled)
summary(fit2)

# 9.7
library(MASS)

attach(UScereal)
summary(UScereal)
pairs(UScereal)

shelf <- factor(shelf)
y <- cbind(calories, sugars, fat, vitamins)
aggregate(y, by=list(shelf), FUN=mean)
cov(y)

fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit)

center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y, center, cov)
coord <- qqplot(qchisq(ppoints(n), df=p), d, ylab="Mahalanobis D2",
                main="Q-Q Plot Assessing Multivariate Normality")
abline(a=0, b=1)
# identify(coord$x, coord$y, labels=row.names(UScereal))

y <- cbind(calories, sugars, fat)

library(mvoutlier)
(aq.plot(y))

library(rrcov)
Wilks.test(y, shelf, method="mcd") # takes awhile

# 9.8
library(multcomp)
summary(cholesterol)
pairs(cholesterol)
levels(cholesterol$trt)

fit.aov <- aov(response ~ trt, data=cholesterol)
summary(fit.aov)

fit.lm <- lm(response ~ trt, data=cholesterol)
summary(fit.lm)
contrasts(cholesterol$trt)

(contrasts(cholesterol$trt) <- contr.helmert(5))
fit.lm.1 <- lm(response ~ trt, data=cholesterol, contrasts="contr.helmert")
summary(fit.lm.1)

(contrasts(cholesterol$trt) <- contr.poly(5))
fit.lm.2 <- lm(response ~ trt, data=cholesterol, contrasts="contr.poly")
summary(fit.lm.2)

(contrasts(cholesterol$trt) <- contr.sum(5))
fit.lm.3 <- lm(response ~ trt, data=cholesterol, contrasts="contr.sum")
summary(fit.lm.3)

(contrasts(cholesterol$trt) <- contr.treatment(5))
fit.lm.4 <- lm(response ~ trt, data=cholesterol, contrasts="contr.treatment")
summary(fit.lm.4)

(contrasts(cholesterol$trt) <- contr.SAS(5))
fit.lm.5 <- lm(response ~ trt, data=cholesterol, contrasts="contr.SAS")
summary(fit.lm.5)