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