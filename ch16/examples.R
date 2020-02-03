# 16.2
data(nutrient, package="flexclust")
summary(nutrient)
pairs(nutrient)

d <- dist(nutrient)
as.matrix(d)[1:5,1:5]

# 16.3
data(nutrient, package="flexclust")
row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)
summary(nutrient)
summary(nutrient.scaled)

d <- dist(nutrient)
d.scaled <- dist(nutrient.scaled)
as.matrix(d)[1:5,1:5]
as.matrix(d.scaled)[1:5,1:5]

fit.average <- hclust(d.scaled, method="average")
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")

fit.centroid <- hclust(d.scaled, method="centroid")
plot(fit.centroid, hang=-1, cex=.8, main="Centroid Linkage Clustering")

fit.ward <- hclust(d.scaled, method="ward")
plot(fit.ward, hang=-1, cex=.8, main="Ward Linkage Clustering")

fit.single <- hclust(d.scaled, method="single")
plot(fit.single, hang=-1, cex=.8, main="Single Linkage Clustering")

fit.complete <- hclust(d.scaled, method="complete")
plot(fit.complete, hang=-1, cex=.8, main="Complete Linkage Clustering")

library(NbClust)
devAskNewPage(ask=FALSE)
nc <- NbClust(nutrient.scaled, min.nc=2, max.nc=15,
              distance="euclidean", method="average")
par(mfrow=c(1,1))
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), main="Number of Clusters",
        xlab="Numer of Clusters", ylab="Number of Criteria")

clusters <- cutree(fit.average, k=5)
table(clusters)
aggregate(nutrient, by=list(clusters=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(clusters=clusters), median)
plot(fit.average, hang=-1, cex=.8, main="Average Linkage\n5 Cluster")

clusters <- cutree(fit.centroid, k=5)
table(clusters)
aggregate(nutrient, by=list(clusters=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(clusters=clusters), median)
plot(fit.centroid, hang=-1, cex=.8, main="Centroid Linkage\n5 Cluster")

clusters <- cutree(fit.ward, k=5)
table(clusters)
aggregate(nutrient, by=list(clusters=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(clusters=clusters), median)
plot(fit.ward, hang=-1, cex=.8, main="Ward Linkage\n5 Cluster")

clusters <- cutree(fit.single, k=5)
table(clusters)
aggregate(nutrient, by=list(clusters=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(clusters=clusters), median)
plot(fit.single, hang=-1, cex=.8, main="Single Linkage\n5 Cluster")

clusters <- cutree(fit.complete, k=5)
table(clusters)
aggregate(nutrient, by=list(clusters=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(clusters=clusters), median)
plot(fit.complete, hang=-1, cex=.8, main="Complete Linkage\n5 Cluster")

# 16.4
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

data(wine, package="rattle")
head(wine)
pairs(wine)

df <- scale(wine[-1])
wssplot(df)

library(NbClust)
set.seed(1234)
devAskNewPage(ask=FALSE)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
n <- nc$Best.n[1,]
table(n)

par(mfrow=c(1,1))
barplot(table(n), xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")

set.seed(1234)
fit.kmeans <- kmeans(df, 3, nstart=25)
fit.kmeans$size
fit.kmeans$centers
aggregate(wine[-1], by=list(clusters=fit.kmeans$cluster), mean)

(ct.kmeans <- table(wine$Type, fit.kmeans$cluster))
library(flexclust)
randIndex(ct.kmeans)

library(cluster)
set.seed(1234)
fit.pam <- pam(wine[-1], k=3, stand=TRUE)
fit.pam$medoids
clusplot(fit.pam, main="Bivariate Cluster Plot")
(ct.pam <- table(wine$Type, fit.pam$clustering))
randIndex(ct.pam)