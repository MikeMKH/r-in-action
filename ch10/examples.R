# 10.2
library(pwr)

pwr.t.test(d=(1/1.25), sig.level=(1-0.95), power=0.9,
           type="two.sample", alternative="two.sided")
pwr.t.test(n=20, d=0.5, sig.level=(1/100),
           type="two.sample", alternative="two.sided")

pwr.anova.test(k=5, f=0.25, sig.level=0.05, power=0.8)

pwr.r.test(r=0.25, sig.level=0.05, power=0.90, alternative="greater")

pwr.f2.test(u=(6-3), f2=((0.35-0.30)/(1-0.35)), sig.level=0.05, power=0.90)

p <- matrix(byrow=TRUE, nrow=3,
  c(.42, .28, .03,
    .07, .10, .10))
pwr.chisq.test(w=ES.w2(p), df=2, sig.level=.05, power=.9)

es <- seq(.1, .5, .01)
nes <- length(es)

samsize <- NULL
for (i in 1:nes){
  result <- pwr.anova.test(k=5, f=es[i], sig.level=.05, power=.9)
  samsize[i] <- ceiling(result$n)
}

plot(samsize,es, type="l", lwd=2, col="red",
     ylab="Effect Size",
     xlab="Sample Size (per cell)",
     main="One Way ANOVA with Power=.90 and Alpha=.05")

# 10.3
library(pwr)

r <- seq(0.1, 0.5, 0.01)
nr <- length(r)

p <- seq(0.4, 0.9, 0.1)
np <-length(p)

samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np) {
  for (j in 1:nr) {
    result <- pwr.r.test(
      n=NULL, r=r[j], sig.level=0.05, power=p[i],
      alternative="two.sided")
    samsize[j, i] <- ceiling(result$n)
  }
}
summary(samsize)

xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(np)

plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)")

for (i in 1:np) {
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],0.02), lty=2, col="grey89")

title("Sample Size Estimation for Correlation Studies\nSig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p), fill=colors)