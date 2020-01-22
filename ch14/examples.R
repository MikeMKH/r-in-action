# 14.2
library(psych)
data <- USJudgeRatings[,-1]
summary(data)

fa.parallel(data, fa="pc",
            n.iter=100, show.legend=FALSE,
            main="Scree plot with parallel analysis")

(pc <- principal(data, nfactors=1))

library(psych)
summary(Harman23.cor)

fa.parallel(Harman23.cor$cov, n.obs=302, fa="pc",
            n.iter=100, show.legend=FALSE,
            main="Scree plot with parallel analysis")

(pc <- principal(Harman23.cor$cov, nfactors=2, rotate="none"))

(rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax"))

library(psych)
pc <- principal(USJudgeRatings[,-1], nfactors=1, score=TRUE)
head(pc$scores)

cor(USJudgeRatings$CONT, pc$scores)

library(psych)
rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
round(unclass(rc$weights), 2)