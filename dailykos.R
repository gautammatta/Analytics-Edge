setwd("C:/Users/G-Machine/Downloads/Analytics Edge/Unit 6")


dailyKos <- read.csv("dailykos.csv")
str(dailyKos)
head(dailyKos)

dailyKosVector <- as.vector(as.matrix(dailyKos))

distance <- dist(dailyKos, method = "euclidean")

dailyKosClust <- hclust(distance, method = "ward")

plot(dailyKosClust)

dailyKosClusters <- cutree(dailyKosClust, k =7)

str(dailyKosClusters)

table(dailyKosClusters)

HierCluster1 = subset(dailyKos, dailyKosClusters == 1)
HierCluster2 = subset(dailyKos, dailyKosClusters == 2)
HierCluster3 = subset(dailyKos, dailyKosClusters == 3)
HierCluster4 = subset(dailyKos, dailyKosClusters == 4)
HierCluster5 = subset(dailyKos, dailyKosClusters == 5)
HierCluster6 = subset(dailyKos, dailyKosClusters == 6)
HierCluster7 = subset(dailyKos, dailyKosClusters == 7)


tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))




### K-means clustering algorithm 


set.seed(1000)
k= 7

KMC <- kmeans(dailyKos, centers = k)

KMC$centers[3]

KMC$size

KmeansCluster1 = subset(dailyKos, KMC$cluster == 1)
KmeansCluster2 = subset(dailyKos, KMC$cluster == 2)
KmeansCluster3 = subset(dailyKos, KMC$cluster == 3)
KmeansCluster4 = subset(dailyKos, KMC$cluster == 4)
KmeansCluster5 = subset(dailyKos, KMC$cluster == 5)
KmeansCluster6 = subset(dailyKos, KMC$cluster == 6)
KmeansCluster7 = subset(dailyKos, KMC$cluster == 7)


str(KmeansCluster4)

KmeansCluster = split(dailyKos, KMC$cluster)

tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))


table(KMC$cluster,dailyKosClusters)


