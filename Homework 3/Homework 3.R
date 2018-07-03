#####Homework 3#####
# ATTENTION!!!: whenever you are applying k means clustering, set seed equal to 5.
# You need to set seed every time before performing k means clustering.
# The Global Competitiveness Report 2015-2016 assesses the competitiveness
# landscape of 144 economies, providing insight into the drivers of their 
# productivity and prosperity. The report remains the most comprehensive assessment of 
# national competitiveness worldwide, providing a platform for 
# dialogue between government, business and civil society about the actions 
# required to improve economic prosperity. Competitiveness is defined as the 
# set of institutions, policies and factors that determine the level of 
# productivity of a country. The level of productivity, in turn, sets the 
# level of prosperity that can be earned by an economy.
# Visit http://reports.weforum.org/global-competitiveness-report-2015-2016 for more details

# The different aspects of competitiveness are captured in 12 pillars, 
# which compose the Global Competitiveness Index. The value for pillar for each country is a 
# number between 1-7 and is measured based on the set of sub-indicators (also measured by the scale 1-7)
# Your task is to create clusters of countries based on the pillars of competitivness.

# 1. Load the dataset GCI.csv into R.

GCI_csv <- read.csv("GCI.csv")
View(GCI_csv)

# 2. Make the variable Country.code as rownames for the dataframe 
# (hint: use rownames() command) (1)

?rownames
rownames(GCI_csv) <- GCI_csv[, 1]

# 3. Remove the variable Country.Code from the dataframe as you will 
# not need it anymore. (1)

GCI_csv[, 1] <- NULL

# 4. Run hierarchical clustering on the dataset, using 12 pillars as clustering variables (2)

GCI1 <- as.data.frame(scale(GCI_csv[, -1]))
GCI_dist <- dist(GCI1, method = "euclidean") # distances

clust <- hclust(GCI_dist)

plot(clust, labels = FALSE, hang = -1)

# 5.Plot the dendogram. What do you think is the optimal number of clusters?
# Try 4 different options. The code should be written in such a 
# way that R gives you a new clear plot each time you try different number 
# of clusters and the rectangles are drown on top of them for each of your options.
# Your goal is to get as nit plots as possible. (2)

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 3) # take 3 clusters

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 4) # take 4 clusters

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 8) # take 8 clusters

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 16) # take 16 clusters

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 32) # take 32 clusters

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 64) # take 64 clusters


# 5.1 Choose one of the numbers of the clusters that you created in problem 5.
# Describe what are the differnces between the clusters in terms of differences in  means. (1)

## we can see that the countries, that are clustered togeter have some sonnections
## although 32 is not sufficent and 64 is too much, we can take the average of those numbers
## which is 48

plot(clust, labels = FALSE, hang = -1) # ploting clusters
rect.hclust(clust, 48) # take 48 clusters

GCI_csv$cluster_hierarchical <- cutree(clust, k = 48)
cluster_mean <- aggregate(GCI_csv[, -1], by = list(GCI_csv$cluster_hierarchical), FUN = "mean", na.rm = TRUE)
View(cluster_mean)

# 5.2 How will you describe your clusters? Try to give names to each of the clusters.(1)

## I'll try to classify some of the clusters accoring to their geo-political similarities

## 23'rd cluster includes RUS, CHN.
## These countries share a political(sociliastic) and economical connections

## 28'th cluster includes NOR, DNK(Denmark),LUX, SWE,QAT, ARE 
## This are mainly nordic countries except for two of them, but all of these countries are alike
## in their foreign investment strategies and revenue generation.
## These countries can be called "Nordlike Investors"

## 29'th cluster includes GUY, ZMB, GHA, DOM, HND
## These are African and American countries(and Carribean pool) that
## share similar history and development patterns(economical,political).
## These countries can be called "Afro - American"

## 40'th cluster includes USA, GBR, JPN. 
## These are post modernized and post industrialized countries
## that are one the top of world technological, inovative and finacial force.
## These countries can be called "Rich Inovators"

## 42'nd cluster includes only KOR (south Korea)
## This is an expected outcome, as South Korea is an isolated country

## The other clusters are also combining quite similar countries, 
## mostly by their geographical, historical, financial aspects, though it is hard to classify them


# 5.3 Looking at the averages of the pillars for each cluster, pick a pillar
# that you think constitutes the largest difference between the clusters.
# Create a boxplot of that pillar against the clusters. Give your comments. (2)

summary(cluster_mean) # pillar 10 has the largest differences
boxplot(GCI_csv$Pillar.10 ~ GCI_csv$cluster_hierarchical)

# 6.Run K-means algorithm, with the same number of clusters that you used in the
# prevous problem. (1) 

set.seed(5)
km <- kmeans(GCI_csv[, -1], 48)
km$iter ## itterates 2 times
GCI_csv$km_cluster <- km$cluster

# 7. Are the results the same? Comment.
# (Remember that you might get different numbers (labels) for the 
# clusters if you are using different methods. (2)

table(GCI_csv$cluster_hierarchical, GCI_csv$km_cluster)
## the numbers(labels) of clusters are changed, but the contents of the clusters remain
## fairly alike. The clusters are not perfectly the same, though numerous
## similarities can be observed.

# Now choose one of the methods and continue with that.

## I choose k-means

# The dataset WDI indicators has some social and economic data on the countries included in GCI study.
# Note the WDI dataset has the same order as GCI,
# so you can easly add cluster variable to the WDI dataset

# 8.1 Read the dataset into R. Look at the population - what are the min and max values 
# for each cluster? what does this info tell you about the clusters? (2)

WDI_csv <- read.csv("WDI.csv")
View(WDI_csv)
WDI_csv$km_cluster <- GCI_csv$km_cluster # using k-means clusters on this dataframe

wdi_pop_max <- aggregate(WDI_csv$Population, by = list(WDI_csv$km_cluster), FUN = "max", na.rm = TRUE)
wdi_pop_min <- aggregate(WDI_csv$Population, by = list(WDI_csv$km_cluster), FUN = "min", na.rm = TRUE)
summary(wdi_pop_min)
summary(wdi_pop_max)
View(wdi_pop_min)
View(wdi_pop_max)

which.min(wdi_pop_min$x)
which.max(wdi_pop_max$x)

## 11th cluster has the min population, as those countries are developing ones and outflow may be big
## the 23rd cluster has max population, because it includes the most over-populated country, China

## The rates in  cluster 16 that is in BFA, MOZ are low and the average male doesnt survive until 53
## 43rd cluster, includes Nigerians, which is the lowest life excectancy country
## The Post-Soviet countries have almost similar age boundries at 75 +-4
## In scandinavian countries + Luxemburg + Qatar the life expectancy is from 77-81
## Yemen is the country representing the median between Europe and Africa 

ClustUneploymentMean <- aggregate(WDI_csv$Unemployment, by = list(WDI_csv$km_cluster), FUN="mean", na.rm = TRUE)
ClustGDPPerCapitaMean <-aggregate(WDI_csv$GDP_per_.capita, by = list(WDI_csv$km_cluster), FUN="mean", na.rm = TRUE)
summary(ClustGDPPerCapitaMean)
summary(ClustUneploymentMean)
View(ClustGDPPerCapitaMean)
View(ClustUneploymentMean)

## In cluster 28, having the minumum unemployment of all the clusters and their
## GDP per capta is the higest among all of the clusters. Their economical infrastructure is great.

## In cluster 40, though being developed in their infastructure
## the unemployment rates are quite average. Still those rates are impressive in 
## comparison to the whole world 6,7 ~= 6,3 < 9,126. The  overal GDP is in best triple of the dateset.

## In cluster 23, though having unemployment rates that
## are in TOP 5 of the overal dataset, their GDP Per capita is lower than the average of the datestet.

## In cluster 29, though the uneployment rates in these countries 
## are quite average the GDP Per Capita is one of the lowest in the dataset. 
## This indicates the fact of having low and cheap productions.


# Global Peace Index is an attempt to measure the relative position of nations' and regions' peacefulness.
# The dataset GPI provides the Global Peace Index scores and rankings 
# for the countries included in the list.
# Note the GPI dataset has the same order as GCI,
# so you can easly add cluster variable to the WDI dataset.

# 9.1 Load the dataset into R.

GPI_csv <- read.csv("GPI.csv")
View(GPI_csv)

# 9.2 Calcualte average score for each cluster. Comment on your findings. (2)

GPI_csv$km_cluster <- GCI_csv$km_cluster # using k-means clustering
aggregate(GPI_csv$GPI.2016.Score, by = list(GPI_csv$km_cluster), FUN = "mean", na.rm = TRUE)


# 9.3 Estimate rankings for cluster based on the average scores you received in previous step.
# (The rankings for each country are available in the GPI dataset.)(2)

aggregate(GPI_csv$GPI.2016.Rank, by = list(GPI_csv$km_cluster), FUN = "mean", na.rm = TRUE)

library(ggplot2)
qplot(GPI.2016.Rank, GPI.2016.Score, data = GPI_csv, xlab = "Ranking", ylab = "Score")
cor(GPI_csv$GPI.2016.Score, GPI_csv$GPI.2016.Rank, use="complete.obs")

## there is an extremely high corelation of 0.967 between the rankings and scores
## so they are enormously dependent on each other

# 10. Do your own research on the datasets, find something interesting.(5)
# Do not do trivial manipulations like creating plots without making any valuable
# inferences about those plots.

which.max(GPI_csv$GPI.2016.Rank)
GPI_csv[132,]                      # Ukraine
which.min(GPI_csv$GPI.2016.Rank)
GPI_csv[54,]                       # Iceland

which.max(GPI_csv$GPI.2016.Score)
GPI_csv[132,]                      # Ukraine
which.min(GPI_csv$GPI.2016.Score)
GPI_csv[54,]                       # Iceland

## both in ranking and score the highest one is Ukraine and the lowest one is Iceland
## Ukraine was in war, so this is quite logical
## and Iceland is one of the most peaceful countries

#Football data analysis
# The file Soccer.csv summarized various data for football players from 11 European 
# Leagues. The data is collecting based on the FIFA ratings. 
# http://sofifa.com/player/192883 By opening this link, you can find the data for
# Henrikh Mkhitaryan, as well as find the decriptions of the variables. Do some
# reasearch on this web site before starting your homework.
  
# 11. Load file Soccer.csv into R. Perform k means clustering. 
# Try with at least two different numbers of clusters. (2)
  
soccer <- read.csv("Soccer.csv")
View(soccer)

soccer1 <- as.data.frame(scale(soccer[,5:42]))
set.seed(5)
k1 <- kmeans(soccer1, 3)
k2 <- kmeans(soccer1, 4)
k3 <- kmeans(soccer1, 5)
k4 <- kmeans(soccer1, 8)

# 12. Plot the clusters for all the trials (numbers of clusters).
# Do you see any similarity in both graphs?
# Hint: you may find useful package fpc and function called plotcluster. (3)

install.packages("fpc")
library(fpc)

plotcluster(soccer1, k1$cluster)
plotcluster(soccer1, k2$cluster)
plotcluster(soccer1, k3$cluster)
plotcluster(soccer1, k4$cluster)

## In all graphs only one cluster is highly separated from the other clusters 
## In this cluster the skillset of the player is quite different from the members of other clusters

# 13. Aggregate the means for all the variables for one of 
# the results that you got in question 2. Give comments about the clusters. (2)

soccer$kmeans_cluster <- k1$cluster
soccer_kmeans_cluster <- k1$cluster # let's take 3 clusters
aggregate(soccer[,5:42], by=list(soccer_kmeans_cluster), FUN="mean", na.rm=TRUE)

## In cluster 1 there are low stats for all the skills and gk positionsing,
## gk kicking handling, diving and average height are the lowest ones

## In cluster 3 there are many goalkeepers => they are tall, low agility, better goal keeps (logical :D), 
## less aggression, and best impressive reflexes

## In cluster 2 players are more skillful and experienced
## Although, in cluster 2 players seem to have less long and short passing potential, worse vision and worse positioning than in cluster 1

#14. Now perform hierarchical clustering. Again try with several number of clusters. (2)

distance <- dist(soccer1, method="euclidean")
soccer_hier_cluster <- hclust(distance)

# 15. For all the trials, again create plots and try to find patterns.
# Describe the patterns that you noticed. (2)

plot(soccer_hier_cluster, hang=-1, labels = FALSE)
rect.hclust(soccer_hier_cluster, k=2)

plot(soccer_hier_cluster, hang=-1, labels = FALSE)
rect.hclust(soccer_hier_cluster, k=3)

plot(soccer_hier_cluster, hang=-1, labels = FALSE)
rect.hclust(soccer_hier_cluster, k=4)

plot(soccer_hier_cluster, hang=-1, labels = FALSE)
rect.hclust(soccer_hier_cluster, k=5)

## Very similar to k-means clustering results
## Again the leftmost cluster is separted
## this is probably a cluster of goalkeepers with rather unique skillset and fewer number of people
## other clusters are mostly different in players' skillfulness

# 16. Based on the patterns and similarities between the graphs that you
# noticed while performing clustering with kmeans and hierarchical 
# clustering, come up with an optimal number of clusters. Choose
# either of the clustering methods, and assign clusters to each of 
# the cases in the soccer dataset. (2)

## let's take k-means again
soccer_membership <- cutree(soccer_kmeans_cluster, k=3)

# 17. Aggregate the average data for all the variables for the number
# of the clusters that you chose in question 7. What are the differences 
# between those clusters? Try to give a general description for 5-6 of them. (2)

aggregate(soccer[,5:42], by = list(soccer_kmeans_cluster), FUN="mean", na.rm=TRUE)
## cluster 2 includes players with higher qualities in vision,accuracy, ball control, less weight, kicking, acceleration
## probably cluster of forwards
## cluster 3 includes those with goalkeepers's specific skills
## probably a cluster of goalkeepers
## cluster 1 is some kind of mixture of skills from 1st and 2nd clusters
## probably a cluster of defending players

# 18. Pick a player from each of those clusters. Describe those players in terms of
# their affiliation to the clusters. What are the similarities and differences between them? (1)

set.seed(13)
sample(nrow(soccer), 3)
soccer[2076,"kmeans_cluster"]
soccer[2076,"player_name"] # Stefan de Vrij
soccer[1792,"kmeans_cluster"] 
soccer[1792,"player_name"] # Andrea Poli
soccer[3001,"kmeans_cluster"] # This one is my random choice, as there was no cluster 3 in set.seed() provided obs.
soccer[3001,"player_name"] # Sergiusz Prusak
## Stefan de Vrij is a defensive player from cluster 2, randomly chosen
## Andrea Poli is a midfielder from cluster 1, randomly chosen
## Sergiusz Prusak is a goalkeeper from cluster 3, randomly chosen by me
soccer[2076,]
soccer[1792,]
soccer[3001,]

## Stefan de Vrij has low finishing, dribbling, balance and vision, buthas high long passing, standing and sliding tackle and interceptions
## Andrea Poli has low finishing, heading accuracy, but has average vision, agression and high ball control
## Sergiusz Prusak has low stats in every aspect, though high or average stats in all of the goalkeeping skills (gk_diving, gk_handling, gk_kicking and gk_reflexes)

# 19. Now aggregate the data in a way that you end up with dataframe
# where each row represents a club. Now run clustering on that dataframe
# (use whichever method you prefer.) Aggregate the average values for
# each of the clusters. Again, choose 5-6 variables, compare the clubs
# based on them, and try to give names to the clusters.

clubs <- aggregate(soccer, by = list(soccer$team_long_name), FUN="mean", na.rm=TRUE)
summary(clubs)

club_dist <- dist(clubs[,5:42], method="euclidean")
club_cluster <- hclust(club_dist)
plot(club_cluster, hang = -1, labels = FALSE)
rect.hclust(club_cluster, k=3)
club_clustering <- cutree(club_cluster, k=3)
aggregate(clubs[,14:42], by = list(club_clustering), FUN="mean", na.rm=TRUE)

## Cluster 1 has teams with weakest players
## Let's name this cluster "Econom Class Teams"
## Cluster 2 has teams with average players, not as good as in cluster 3 and not as weak as in cluster 1
## Let's name this cluster "Developing Teams"
## Cluster 3 has teams with skillful players in almost every aspect
## Let's name this cluster "Money and Skill"


#20. Do your own research on the datasets, find something interesting.
# Do not do trivial manipulations like creating plots without making any valuable
# inferences about those plots. (5)

pairs(~ Age + aggression + heading_accuracy + stamina, data = soccer)
cor(soccer[,5:42])

## It can be deducted that aggression of the player is highly on the age of the player
## Furthermore, more aggresive means better stamina heading accuracy
## Therefore, in clusters of middle skillfulness are younger players with less agression => less accuracy and ball control
## the cluster of skillful players with high ball control and accuracy mainly contains experienced players

## Being an Armenian and working with soccer dataset can cause only 1 thing :D
## Do somthing with Henrik Mkhitaryans stats
soccer[1517,] # Henrik Mkhitaryan

## Henrik Mkhitaryan's skillset point can be classified to both middle level and the high level clusters
## I believe that here also works the pattern of age, being young Henrik has higher than average stats
## vision, free kick accuracy, ball control accleretaion are the most impressive skills
