#Clustering
rm(list=ls())
library(ggplot2)
library("multtest")
library("fpc")
library("cluster")
library("lsa")
library("proxy")
library("kmed")
library("NbClust")
library("factoextra")
library("ggfortify")
library("Rtsne")

setwd("~/Documents/GitHub/Spotty_Tagger")

tracks = read.csv('dataWithLabel1000.csv')

tracks = tracks[complete.cases(tracks), ]
summary(tracks)

colsList = c('acousticness','danceability', 'energy', 'instrumentalness','liveness', 'loudness', 'speechiness', 'tempo', 'valence','positivity_level','energy_level')

tracksColFil = tracks[,colsList]

# Biplot
biplot(prcomp(tracksColFil,scale = TRUE))

# Data transformation
data <- tracksColFil
data_Bk <- tracks
track.measures <- c("acousticness","danceability","duration_ms","energy",
                    "instrumentalness","liveness","loudness","speechiness","tempo",
                    "valence","energy_level","positivity_level")

nrow(data_Bk)
nrow(data)
data <- data[,track.measures]
data.scaled <- data
data.scaled <- scale(data)

# Using tsen package
tsne <- Rtsne(data.scaled, dims = 5, perplexity=30, verbose=TRUE, max_iter = 2000,check_duplicates = FALSE)
load("~/Documents/GitHub/Spotty_Tagger/Data/Final Clustering Data.RData")
# Getting distance matrix
d = dist(tsne$Y)
dim(as.matrix(tsne$Y))

dfts = data.frame(pc1=tsne$Y[,1],pc2=tsne$Y[,2])
pc1=tsne$Y[,1]
pc2=tsne$Y[,2]


############################################
# Kmeans
############################################

# Finding optimal number of cluster
fviz_nbclust(tsne$Y, kmeans, method = "wss") + ggtitle("K-Means") +
  theme(plot.title = element_text(hjust = 0.5))

#Best clust Kmeans
kMeansTrk = kmeans(d,centers = 5,iter.max = 10)
kMeansTrk$tot.withinss


kMeanClust = as.factor(kMeansTrk$cluster)
# GGPlot for Kmeans
# By Track name
ggplot(dfts, aes(pc2,pc1,color=kMeanClust)) + geom_point() +geom_text(aes(label=data_Bk$track_name)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

ggplot(dfts, aes(pc2,pc1,color=kMeanClust)) + geom_point() +geom_text(aes(label=data_Bk$genre)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

ggplot(dfts, aes(pc1,pc2,color=kMeanClust)) + geom_point() +geom_text(aes(label=data_Bk$artist)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# Cluster plots

fviz_cluster(kMeansTrk, data = tsneData, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By Track Name
fviz_cluster(kMeansTrk, data =tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$track_name))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By mood
fviz_cluster(kMeansTrk, data =tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$mood))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")



############################################
# K-Medoids
############################################
# Finding optimal number of cluster
fviz_nbclust(tsne$Y, pam, method = "wss")  + ggtitle("K-Medoids") +
  theme(plot.title = element_text(hjust = 0.5))

# By fastkmed
kmedFast = fastkmed(d, 5, iterate = 10)
kMedoids_Cluster = as.factor(kmedFast$cluster)

# GGPlots for KMed

# By Track name
ggplot(dfts, aes(pc1,pc2,color=kMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$track_name)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By genre
ggplot(dfts, aes(pc1,pc2,color=kMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$genre)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By mood
ggplot(dfts, aes(pc2,pc1,color=KMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$mood))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By pam
pamKmed = pam(tsne$Y, 5, metric = "euclidean", stand = FALSE)

KMedoids_Cluster = as.factor(pamKmed$clustering)

# ggplots for Pam Kmed

# By Track name
ggplot(dfts, aes(pc2,pc1,color=KMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$track_name))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By genre
ggplot(dfts, aes(pc2,pc1,color=KMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By mood
ggplot(dfts, aes(pc2,pc1,color=KMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$mood))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

tracks_CLust$clust = kMeansTrk$cluster
tsneData = tsne$Y

# Cluster plots

fviz_cluster(pamKmed, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

fviz_cluster(pamKmed, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm",labelsize = 10) +geom_text(aes(label=data_Bk$track_name))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

fviz_cluster(pamKmed, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm",labelsize = 10) +geom_text(aes(label=data_Bk$mood))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")



#Best cluster

#Kmed

#hclust
fviz_nbclust(tsne$Y, hcut, method = "wss")  + ggtitle("Hierarchical Clustering") +
  theme(plot.title = element_text(hjust = 0.5))


############################################
# Hierarchical Ckustering
############################################

# Finding optimal number of cluster
fviz_nbclust(tsne$Y, hcut, method = "wss")  + ggtitle("Hierarchical Clustering") +
  theme(plot.title = element_text(hjust = 0.5))

# Hierarchical clustering results
hc <- hclust(d, method = "complete")

# Visualization of hclust
plot(hc, labels = FALSE, hang = -1)

# Add rectangle around 3 groups
rect.hclust(hc, k = 5, border = 2:4) 

hc.cut <- cutree(hc, k = 5)

h_Cluster = as.factor(hc.cut)
# ggplots

# By track name
ggplot(dfts, aes(pc2,pc1,color=h_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$track_name)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2") 

# By genre
ggplot(dfts, aes(pc2,pc1,color=h_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$genre)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

# By Mood
ggplot(dfts, aes(pc2,pc1,color=h_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$mood)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2") 

