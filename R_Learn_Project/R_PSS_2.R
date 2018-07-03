mtcars <- mtcars
View(mtcars)

mtcars1 <- scale(mtcars[, -c(8, 9)])

mtcars_dist <- dist(mtcars1, method = "euclidean") # distances

clust <- hclust(mtcars_dist) # clustering

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 3) # take 3 clusters

mtcars$cluster_membership <- cutree(clust, k = 3) # column to show the cluster of an observation
View(mtcars)

cluster_mean <- aggregate(mtcars[, -c(8, 9)], by = list(mtcars$cluster_membership), FUN = "mean", na.rm = TRUE)
View(cluster_mean)


##k-means
set.seed(45) # seed is 45
km <- kmeans(mtcars[, -c(8:9, 12)], 3)
km$iter
names(km)
mtcars$k1 <- km$cluster

## experimental seed
set.seed(32) # seed is 32
km <- kmeans(mtcars[, -c(8:9, 12)], 3)
km$iter
names(km)
mtcars$k2 <- km$cluster
mtcars$k2 <- NULL

table(mtcars$k1, mtcars$k2) # compare seeds

table(mtcars$k1, mtcars$cluster_membership) # compare k-means and hierarchical

cluster_mean_kmeans <- aggregate(mtcars[, -c(8, 9)], by = list(mtcars$k1), FUN = "mean", na.rm = TRUE)
View(cluster_mean_kmeans)




