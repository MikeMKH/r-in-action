# 10.2
library(pwr)

pwr.t.test(d=0.8, sig.level=0.05, power=0.9,
           type="two.sample", alternative="two.sided")
pwr.t.test(n=20, d=0.5, sig.level=0.01,
           type="two.sample", alternative="two.sided")

pwr.anova.test(k=5, f=0.25, sig.level=0.05, power=0.8)

pwr.r.test(r=0.25, sig.level=0.05, power=0.90, alternative="greater")

pwr.f2.test(u=3, f2=0.0769, sig.level=0.05, power=0.90)