# 16.2
data(nutrient, package="flexclust")
summary(nutrient)
pairs(nutrient)

d <- dist(nutrient)
as.matrix(d)[1:5,1:5]