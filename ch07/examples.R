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