# 13.1
summary(mtcars)

glm(family=binomial(link="logit"),
    am~cyl+disp+hp+wt, data=mtcars)
glm(family=gaussian(link="identity"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=inverse.gaussian(link="1/mu^2"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=poisson(link="log"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=quasi(link="identity", variance="constant"),
    qsec~cyl+disp+hp+wt, data=mtcars)
glm(family=quasibinomial(link="logit"),
    am~cyl, data=mtcars)
glm(family=quasipoisson(link="log"),
    qsec~cyl+disp+hp+wt, data=mtcars)

fit <- glm(family=inverse.gaussian(link="1/mu^2"),
           qsec~cyl+disp+hp+wt, data=mtcars)
summary(fit)
coefficients(fit)
confint(fit)
residuals(fit)
anova(fit)
# plot(fit)
predict(fit)
deviance(fit)
df.residual(fit)