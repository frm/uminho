##### Clustering

### Data Init
set.seed(123456789)

# Loading necessary libs
library("hydroGOF")
library("fpc")
library("cluster")

# Reading the CSV
dataset <- read.csv("data\\sample.csv", header=TRUE, sep=",", dec=".")

# Analysing the CSV contents
# str(dataset)
# summary(dataset)

# Setting Performance.Task to a numeric value
# since neuralnet only works with those
dataset$Performance.Task = as.numeric(dataset$Performance.Task)


################################################
#### Calculating the ideal scale
# Use of clusters

to_cluster <- function(dataset) {
  # We are clustering everything in order to FatigueLevel
  # So we need to select a subset without FatigueLevel
  
  clusterset <- subset(dataset, select=c(Performance.KDTMean, Performance.MAMean,
                                         Performance.MVMean, Performance.TBCMean, Performance.DDCMean,
                                         Performance.DMSMean, Performance.AEDMean, Performance.ADMSLMean,
                                         Performance.Task))
}

# Determine number of clusters
# Using partitioning method
clusterize <- function(clusterset) {
  wss <- ( nrow(clusterset) - 1) * sum( apply(clusterset, 2, var) )

  # Iterating over a range of possible clusters
  for (i in 2:10) {
    set.seed(1234)
    wss[i]<- sum(kmeans(clusterset,
                      centers=i)$withinss)
  }
  
  return(wss)
}

cluster_prob <- function(wss) {
  plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
}

#### Plotting the clusters
cluster_plot <- function(clusterset, n_clusters=3) {
  # K-Means Clustering with default 3 clusters
  fit <- kmeans(clusterset, n_clustes)

  # Cluster Plot against 1st 2 principal components
  # vary parameters for most readable graph
  # clusplot from: library(cluster)
  clusplot(clusterset, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

  # Centroid Plot against 1st 2 discriminant functions
  # plotcluster from: library(fpc)
  plotcluster(clusterset, fit$cluster) 
}

clusterset <- to_cluster(dataset)
cluster_prob ( clusterize(clusterset) )
cluster_plot(clusterset)
