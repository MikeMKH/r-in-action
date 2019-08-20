# 7.1
mt <- mtcars[c("mpg", "hp", "wt", "am")]
head(mt)
pairs(mt)
summary(mt)

# 7.2
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}

sapply(mt, mystats)

# 7.3
library(Hmisc)
describe(mt)

# 7.4
library(pastecs)
stat.desc(mt)

# 7.5
library(psych)
describe(mt)

# 7.6
aggregate(mt, by=list(am=mt$am), mean)
aggregate(mt, by=list(am=mt$am), sd)

# 7.7
dstats <- function(x)sapply(x, mystats)
by(mt, mt$am, dstats)

# 7.8
library(doBy)
summaryBy(mpg+hp+wt~am, data=mt, FUN=mystats)
tail(summaryBy(hp+wt+am~mpg, data=mt, FUN=mean))

# 7.9
library(psych)
describeBy(mt, list(am=mt$am))

library(reshape)
dstats <- function(x)(c(n=length(x), mean=mean(x), sd=sd(x)))
dfm <- melt(mtcars, measure.vars=c("mpg", "hp", "wt"), 
            id.vars=c("am", "cyl"))
cast(dfm, am + cyl + variable ~ ., dstats)

# 7.10
library(vcd)
head(Arthritis)
pairs(Arthritis)

mytable <- with(Arthritis, table(Improved))
mytable
prop.table(mytable)
prop.table(mytable)*100

mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable
margin.table(mytable, 1)
29 + 7 + 7 # Placebo
13 + 7 + 21 # Treated
margin.table(mytable, 2)
prop.table(mytable)
prop.table(mytable, 1) # by Treatment
prop.table(mytable, 2) # by Improved
addmargins(mytable)

addmargins(prop.table(mytable))
addmargins(prop.table(mytable, 1), 2)
addmargins(prop.table(mytable, 2), 1)

library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# 7.11
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)
margin.table(mytable, 1)
margin.table(mytable, 2)
margin.table(mytable, 3)
margin.table(mytable, c(1,3))
ftable(prop.table(mytable, c(1,2)))
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))

# 7.12
library(vcd)

# Chi-square test of independence
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable) # significant

mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable) # no evidence

# Fisher's exact test
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable) # positive relationship
fisher.test(mytable, hybrid=TRUE)
fisher.test(mytable, simulate.p.value=TRUE)
fisher.test(mytable, simulate.p.value=TRUE, hybrid=TRUE)
fisher.test(mytable, alternative="greater")
fisher.test(mytable, alternative="less")

# Chochran-Mantel-Haenszel test
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable) # significant

# 7.13
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)
summary(assocstats(mytable))

# 7.14
states<- state.x77[,1:6]
pairs(states)

cov(states)
cor(states)
cor(states, method="kendall")
cor(states, method="spearman")

x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)


library(ggm)
head(states)
pcor(c(1,5,2,3,6), cov(states))

# 7.15
cor.test(states[,3], states[,5])
cor.test(states[,2], states[,3])
cor.test(states[,3], states[,4])
cor.test(states[,2], states[,6])

# 7.16
library(psych)
corr.test(states, use="complete")

library(psych)
library(quantmod)

# CPI, Swiss, Norway, Sweden, UK, 5000
tickers = c("CPIAUCSL", "AEXSZUS", "AEXNOUS", "AEXSDUS", "AEXUSUK", "WILL5000INDFC")
getSymbols(tickers, src="FRED")

ret <- to.monthly(WILL5000INDFC, indexAt='firstof', OHLC=FALSE)
ret <- na.omit(cbind(CPIAUCSL, ret))
ret <- na.omit(cbind(AEXSZUS, ret))
ret <- na.omit(cbind(AEXNOUS, ret))
ret <- na.omit(cbind(AEXUSUK, ret))

ret <- ret["1980::2018"]
summary(ret)

corr.test(ret, use="complete")
print(corr.test(ret, use="complete"), short=FALSE)
# US/CH and US/GB exchange rate are nearly independent

with(ret, t.test(coredata(AEXUSUK), coredata(AEXSZUS)))
with(ret, t.test(coredata(AEXUSUK), coredata(AEXSZUS), paired=TRUE))

library(MASS)
summary(UScrime)
pairs(UScrime)

# t test
t.test(Prob ~ So, data=UScrime)

# dependent t test
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))

# Wilcoxon two group comparison
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)

sapply(UScrime[c("U1", "U2")], median)
with(UScrime, wilcox.test(U1, U2, paired=TRUE))

# Kruskal Wallis test
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)

# 7.17
source ("http://www.statmethods.net/RiA/wmc.txt")
states <- data.frame(state.region, state.x77)
summary(states)
head(states)

# Nonparametric multiple comparisons
wmc(Illiteracy ~ state.region, data=states, method="holm")