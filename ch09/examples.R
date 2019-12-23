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