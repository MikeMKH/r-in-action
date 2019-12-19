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