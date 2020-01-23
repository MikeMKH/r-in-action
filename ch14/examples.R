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

# 14.3
library(psych)
(cv <- ability.cov$cov)
(cr <- cov2cor(cv))
fa.parallel(cr, n.obs=12, fa="both",
            n.iter=100, main="Scree plots with parallel analysis")
(fa <- fa(cr, nfactors=2, rotate="none", fm="pa"))

(fa.varimax <- fa(cr, nfactors=2, rotate="varimax", fm="pa"))
(fa.promax <- fa(cr, nfactors=2, rotate="promax", fm="pa"))

fsm <- function(oblique) {
  if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    P <- unclass(oblique$loading)
    F <- P %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2")
    return(F)
  }
}

fsm(fa.promax)

factor.plot(fa.varimax, labels=rownames(fa.promax$loadings))
factor.plot(fa.promax, labels=rownames(fa.promax$loadings))

fa.diagram(fa.varimax, simple=FALSE)
fa.diagram(fa.promax, simple=FALSE)

fa.varimax$weights
fa.promax$weights

library(psych)
(fa.24tests <- fa(Harman74.cor$cov, nfactors=4, rotate="promax"))
factor.plot(fa.24tests, labels=rownames(fa.24tests$loadings))
fa.diagram(fa.24tests, simple=TRUE)
fa.diagram(fa.24tests, simple=FALSE)
fa.24tests$weights