data <- read.csv("new.csv", row.names = 1) ## putting row.name no need for following lines
##data1 <- scale(data[, 2:3])
##data <- data.frame(CustomerID = data$Customer.ID, as.data.frame(data1))

data1 <- scale(data)

dd <- dist(data, method = "euclidean")
dd ## distance between points

clust <- hclust(dd)
clust
plot(clust, hang = -1)
rect.hclust(clust, 3)

data$cluster_membership <- cutree(clust, k = 3)
View(data)

clust$height

##### learn later

market <- read.csv( 'Market segmentation.csv' )
View( market )

market <- na.omit( market )                      # omit NAs
hc <- hclust( dist( market[c(1,6)] ), "ave" )    # hierarcial clustering take only 1:6 columns from market
plot( hc )                                       # plot the clusters
plot( hc, hang = -1 )


M <- cutree( hc, 3 )    # divide into 3 clusters
market$cluster <- M   # add cluster column to data frame and indicate the cluster for each row
View( market )

attach( market )
aggdata <-aggregate( market, by=list( cluster ), FUN=mean, na.rm=TRUE ) # aggregate by clusters and take mean of other variables
print( aggdata )                                                        # print the result
detach( market )


set.seed( 5555 )
clusters <- kmeans( market[,1:6], 3 )  # calculate k=3 means with 1:6 columns from market
clusters$size                          # 3 clusters containing number of elements -> 12 12 16
names( clusters )


clusters <- kmeans( market, 3 )  # take all variables
clusters$size                    # 8 16 16
names( clusters )


