library(caTools)

orders <- read.csv('orders.csv')

summary(orders$order_hour_of_day)

sort(table(orders$order_hour_of_day))

summary(orders$days_since_prior_order)


cor(orders$fresh.fruits, orders$fresh.vegetables)

prop.table(table(orders$frozen.pizza))

orders.aisle = orders[, 5:ncol(orders)]

library(caret)

preproc = preProcess(orders.aisle)

ordersNorm = predict(preproc, orders.aisle)

summary(ordersNorm$frozen.dessert )

summary(ordersNorm$soft.drinks)


distances <- dist(ordersNorm, method = "euclidean")

ClusterProducts <- hclust(distances, method = "ward.D")

plot(ClusterProducts, labels = FALSE)


library(caret)
set.seed(200)
ProductsCluster <- kmeans(ordersNorm, center = 4)

ProductsCluster$size

c1 <- subset(ordersNorm, ProductsCluster$cluster == 1)
c2 <- subset(ordersNorm, ProductsCluster$cluster == 2)
c3 <- subset(ordersNorm, ProductsCluster$cluster == 3)
c4 <- subset(ordersNorm, ProductsCluster$cluster == 4)

sort(colSums(c1))
sort(colSums(c2))
sort(colSums(c3))
sort(colSums(c4))

str(c4)
