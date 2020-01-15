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

# 12.3
library(lmPerm)
set.seed(1234)
summary(women)
summary(lm(weight~height, data=women))

fit <- lmp(weight~height, data=women, perm="Prob")
summary(fit)

summary(lm(weight~height + I(height^2), data=women))
fit2 <- lmp(weight~height + I(height^2), data=women, perm="Prob")
summary(fit2)

states <- as.data.frame(state.x77)
summary(states)
summary(lm(Murder~Population+Illiteracy+Income+Frost, data=states))
fit <- lmp(Murder~Population+Illiteracy+Income+Frost,
           data=states, perm="Prob")
summary(fit)

library(lmPerm)
library(multcomp)
set.seed(1234)
summary(cholesterol)
summary(aov(response~trt, data=cholesterol))
fit <- aovp(response~trt, data=cholesterol, perm="Prob")
anova(fit)

library(lmPerm)
set.seed(1234)
summary(litter)
summary(aov(weight ~ gesttime + dose, data=litter))
fit <- aovp(weight ~ gesttime + dose, data=litter, perm="Prob")
anova(fit)

library(lmPerm)
set.seed(1234)
summary(ToothGrowth)
summary(aov(len~supp*dose, data=ToothGrowth))
fit <- aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
anova(fit)

# 12.5
n <- 10
X.bar <- 40
s <- 5
t <- 2.262

(ci <- c(X.bar - t*(s/sqrt(n)),
         X.bar + t*(s/sqrt(n))))

# 12.6
library(boot)
rsq <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

set.seed(1234)
summary(mtcars)
(results <- boot(formula=mpg~wt+disp,
                 data=mtcars, statistic=rsq, R=1000))
plot(results)
boot.ci(results, type=c("perc", "bca"))

library(boot)
bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}

set.seed(1234)
summary(mtcars)
results <- boot(formula=mpg~wt+disp,
                data=mtcars, statistic=bs, R=1000)
# 1 = intecept, 2 = wt, 3 = disp
print(results)
plot(results, index=2)
boot.ci(results, type="bca", index=2)

library(boot)
rsq <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

(results.10 <- boot(formula=mpg~wt+disp,
                    data=mtcars, statistic=rsq, R=10))
(results.100 <- boot(formula=mpg~wt+disp,
                     data=mtcars, statistic=rsq, R=100))
(results.1000 <- boot(formula=mpg~wt+disp,
                      data=mtcars, statistic=rsq, R=1000))
(results.10000 <- boot(formula=mpg~wt+disp,
                       data=mtcars, statistic=rsq, R=10000))

results.10$t0 == results.100$t0
results.100$t0 == results.1000$t0
results.1000$t0 == results.10000$t0

results.10$t

boot.ci(results.10000, conf=0.90)
boot.ci(results.10000, conf=0.95)
boot.ci(results.10000, conf=0.99)

boot.ci(results.10000, conf=0.99, type="norm")
boot.ci(results.10000, conf=0.99, type="basic")
boot.ci(results.10000, conf=0.99, type="perc")
boot.ci(results.10000, conf=0.99, type="bca")

boot.ci(results.10000, conf=c(0.90, 0.95, 0.99),
        type=c("norm","basic", "perc", "bca"))