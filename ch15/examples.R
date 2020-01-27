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