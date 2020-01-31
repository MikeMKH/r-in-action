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