# 18.2
data(sleep, package="VIM")
summary(sleep)

sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]
sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))
mean(!complete.cases(sleep))

# 18.3
library(mice)
data(sleep, package="VIM")
summary(sleep)

md.pairs(sleep)
md.pattern(sleep)

library(VIM)
aggr(sleep, prop=FALSE, numbers=TRUE)
matrixplot(sleep)
marginplot(sleep[c("Gest", "Dream")],
           pch=c(20), col=c("blue", "red", "black"))

x <- as.data.frame(abs(is.na(sleep)))
head(sleep, n=5)
head(x, n=5)

y <- x[which(apply(x, 2, sum)>0)]
head(y, n=5)
cor(y)
cor(sleep, y, use="pairwise.complete.obs")