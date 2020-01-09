# 12.1
library(coin)
library(lmPerm)

# 12.2
library(coin)
score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
treatment <- factor(c(rep("A", 5), rep("B", 5)))
mydata <- data.frame(treatment, score)
summary(mydata)
t.test(score~treatment, data=mydata, var.equal=TRUE)
oneway_test(score~treatment, data=mydata, distribution="exact")

library(MASS)
UScrime <- transform(UScrime, So=factor(So))
summary(UScrime)
wilcox.test(Prob~So, data=UScrime)
wilcox_test(Prob~So, data=UScrime, distribution="exact")

library(multcomp)
summary(cholesterol)
summary(aov(response~trt, data=cholesterol))
set.seed(1234)
oneway_test(response~trt, data=cholesterol,
            distribution=approximate(nresample=9999))

library(coin)
library(vcd)
Arthritis <- transform(Arthritis,
                       Improved=as.factor(as.numeric(Improved)))
summary(Arthritis)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)
set.seed(1234)
chisq_test(Treatment~Improved, data=Arthritis,
           distribution=approximate(nresample=9999))

library(coin)
library(MASS)
summary(UScrime)
sapply(UScrime[c("U1","U2")],
       function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))
wilcoxsign_test(U1~U2, data=UScrime, distribution="exact")

# vignette("coin")
# 3 from vignette("coin")
library(coin)
YOY <- data.frame(
  length = c(46, 28, 46, 37, 32, 41, 42, 45, 38, 44, 42, 60, 32, 42, 45, 58, 27, 51, 42, 52, 38, 33, 26, 25, 28, 28, 26, 27, 27, 27, 31, 30, 27, 29, 30, 25, 25, 24, 27, 30),
  site = gl(4, 10, labels = as.roman(1:4)))
it <- independence_test(length ~ site, data = YOY,
                        ytrafo = function(data)
                          trafo(data, numeric_trafo = rank_trafo),
                        teststat = "quadratic")
it
statistic(it, type = "linear")
expectation(it)
covariance(it)
statistic(it, type = "standardized")
statistic(it)
pvalue(it)

kt <- kruskal_test(length ~ site, data = YOY,
                   distribution = approximate(nresample = 10000))
kt
statistic(kt, type = "linear")
expectation(kt)
covariance(kt)
statistic(kt, type = "standardized")
statistic(kt)
pvalue(kt)