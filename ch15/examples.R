# 15.1
sales <- c(18, 33, 41,  7, 34, 35, 24, 25, 24, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
(tsales <- ts(sales, start=c(2018, 1), frequency=12))
plot(tsales)
start(tsales)
frequency(tsales)

(tsales.subset <- window(tsales, start=c(2018, 5), end=c(2019, 6)))

# 15.2
library(forecast)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw time series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

summary(AirPassengers)
plot(AirPassengers)

data <- log(AirPassengers)
summary(data)
fit <- stl(data, s.window="period")
summary(fit)
plot(fit)

fit$time.series
exp(fit$time.series)

opar <- par(no.readonly=TRUE)
library(forecast)
par(mfrow=c(2,1))
monthplot(AirPassengers, xlab="",  ylab="")
seasonplot(AirPassengers, year.labels="TRUE", main="")
par(opar)

# 15.3
library(forecast)
summary(nhtemp)
plot(nhtemp)
fit.ann <- ets(nhtemp, model="ANN")
summary(fit.ann)

plot(forecast(fit.ann, 1),
     xlab="Year", ylab=expression(paste("Temperature {", degree*F, "}")),
     main="New Haven Annual Mean Temperature")
accuracy(fit.ann)

fit.aan <- ets(nhtemp, model="AAN")
summary(fit.aan)

accuracy(fit.ann)
accuracy(fit.aan)

library(forecast)
(fit.aaa <- ets(AirPassengers, model="AAA"))
(pred.aaa <- forecast(fit.aaa, 5))
plot(pred.aaa, main="Forecast for Air Travel",
     xlab="Time", ylab="Log(AirPassengers)")

pred.aaa$mean <- exp(pred.aaa$mean)
pred.aaa$lower <- exp(pred.aaa$lower)
pred.aaa$upper <- exp(pred.aaa$upper)
p <- cbind(pred.aaa$mean, pred.aaa$lower, pred.aaa$upper)
dimnames(p)[[2]]
p

(fit.mmm <- ets(AirPassengers, model="MMM"))
(pred.mmm <- forecast(fit.mmm, 5))
plot(pred.mmm, main="Forecast for Air Travel",
     xlab="Time", ylab="Log(AirPassengers)")

pred.mmm$mean <- exp(pred.mmm$mean)
pred.mmm$lower <- exp(pred.mmm$lower)
pred.mmm$upper <- exp(pred.mmm$upper)
p <- cbind(pred.mmm$mean, pred.mmm$lower, pred.mmm$upper)
dimnames(p)[[2]]
p

accuracy(pred.aaa)
accuracy(pred.mmm)

library(forecast)
(fit <- ets(JohnsonJohnson))
plot(forecast(fit), main="JNJ Forecasts",
     xlab="Time", ylab="Earnings")
forecast(fit)

forecast(fit, 40*4)

# 15.4
library(forecast)
library(tseries)
plot(Nile)
ndiffs(Nile)

(Nile.d <- diff(Nile, differences=1))
plot(Nile.d)
adf.test(Nile.d)

Acf(Nile.d)
Pacf(Nile.d)

(fit <- arima(Nile, order=c(0,1,1)))
accuracy(fit)

(fit.10 <- arima(Nile, order=c(0,1,10)))
accuracy(fit.10)

qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")

forecast(fit, 10)
plot(forecast(fit, 10), xlab="Year", ylab="Annual Flow")

library(forecast)
(fit.auto <- auto.arima(Nile))
forecast(fit.auto, 10)

fit$aic
accuracy(fit)
fit.auto$aic
accuracy(fit.auto)

library(forecast)
(fit <- auto.arima(sunspots))
forecast(fit, 3)

library(forecast)
(fit <- auto.arima(JohnsonJohnson))
forecast(fit, 3)