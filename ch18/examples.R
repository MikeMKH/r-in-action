# 18.2
data(sleep, package="VIM")
summary(sleep)

sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]
sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))
mean(!complete.cases(sleep))
