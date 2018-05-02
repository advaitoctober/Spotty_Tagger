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
setwd("~/Documents/GitHub/Spotty_Tagger")
#setwd("C:/DragonBallZ/git_Repo/Spotty_Tagger")

tracks = read.csv('dataWithLabel1000.csv')
tracks = tracks[complete.cases(tracks), ]
summary(tracks)

colsList = c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 'key','liveness', 'loudness', 'mode', 'speechiness', 'tempo','time_signature', 'valence','positivity_level','energy_level')

colsList = c('acousticness','danceability', 'energy', 'instrumentalness','liveness', 'loudness', 'speechiness', 'tempo', 'valence','positivity_level','energy_level')

tracksColFil = tracks[,colsList]

trackPCA = prcomp(tracksColFil)

pc1 = trackPCA$x[,1]
pc2 = trackPCA$x[,2]
pc3 = trackPCA$x[,3]
pc4 = trackPCA$x[,4]
pc5 = trackPCA$x[,5]

track_name = as.character(tracks[,'track_name'])
artist = as.character(tracks[,'artist'])
mood = as.character(tracks[,'mood'])

df = data.frame(artist,pc1,pc2)

####
#d<- dist(tracksColFil)
d = dist(tracksColFil,method = "Cosine")
#d = dist(tracksColFil,method = "canberra")
#d = dist(tracksColFil,method = "binary")
d
dim(as.matrix(d))
#####
# Kmeans
#####

kMeansTrk = kmeans(d,centers = 4)

table(kMeansTrk$cluster,tracks$genre)

tracks_CLust = tracks

tracks_CLust$clust = kMeansTrk$cluster
metaCol = c('album', 'artist', 'genre', 'track_name','clust')
tracks_CLust = tracks_CLust[,metaCol]


ggplot(df, aes(pc1,pc2,color=as.factor(kMeansTrk$cluster))) + geom_point() + geom_text(aes(label=tracks$genre))
ggplot(df, aes(pc1,pc2,color=as.factor(tracks$energy_level))) + geom_point() #+ geom_text(aes(label=tracks$positivity_level))


###################
##################
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- d
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10,iter.max = 10 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

####
summary(tracks)

#####################################
# biplot 
tracksNumeric <- tracks[,c(6:18)]  
pc_sdata <- prcomp(tracksColFil, center = TRUE, scale = TRUE)
biplot(pc_sdata)
#####################################


d<- dist(tracksColFil)
d = dist(tracksColFil,method = "Cosine")

dim(as.matrix(d))
# avg linkage

hc_avg <- hclust(d, method = "average")
x11()
plot(hc_avg, main="Average Linkage",labels = tracks$genre)

data2clusters <- cutree(hc_avg,9)

hclustResultDF <- data.frame(tracks$artist,tracks$track_name,tracks$album,tracks$genre,data2clusters)

# 3395 for 1 
sum(hclustResultDF$data2clusters == 1)

trackPCA = prcomp(scaledDF)
pc1 = trackPCA$x[,1]
pc2 = trackPCA$x[,2]
artist = as.character(tracks[,'artist'])

df = data.frame(artist,pc1,pc2)
plot(df$pc1,df$pc2) 
with(df, text(pc1~pc2, labels = artist), pos = 4)


text(pc1 ~ pc2, labels=artist, cex= 0.7)


ggplot(df, aes(pc1,pc2)) + geom_point() + geom_text(aes(label=artist))

summary(tracksColFil)




####################################################
# TSNE working part
################################
library("Rtsne")
data <- tracksColFil
data_Bk <- tracks
track.measures <- c("acousticness","danceability","duration_ms","energy",
                    "instrumentalness","liveness","loudness","speechiness","tempo",
                    "valence","energy_level","positivity_level")

#data = data[complete.cases(data), ]
#data = unique(data)

#data_Bk = data_Bk[complete.cases(data_Bk), ]
#data_Bk = unique(data_Bk)
nrow(data_Bk)

nrow(data)
data <- data[,track.measures]
data.scaled <- data
data.scaled <- scale(data)

Labels <-data$genre
colors  = rainbow(length(unique(Labels)))
names(colors) = unique(Labels)

#data.scaled = data.scaled[complete.cases(data.scaled), ]
tsne <- Rtsne(data.scaled, dims = 5, perplexity=30, verbose=TRUE, max_iter = 2000,check_duplicates = FALSE)


dfts = data.frame(pc1=tsne$Y[,1],pc2=tsne$Y[,2])
ggplot(dfts, aes(pc1,pc2,color=as.factor(data_Bk$genre))) + geom_point() +geom_text(aes(label=data_Bk$genre))



text(tsne$Y, labels=Labels, col=colors[Labels])

d = dist(tsne$Y)
dim(as.matrix(tsne$Y))
dim(as.matrix(d))
##################
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- d
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10,iter.max = 10 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

####

kMeansTrk = kmeans(d,centers = 5)

table(kMeansTrk$cluster,data_Bk$energy_level)
table(kMeansTrk$cluster,data_Bk$positivity_level)

tracks_CLust = tracks

tracks_CLust$clust = kMeansTrk$cluster
metaCol = c('album', 'artist', 'genre', 'track_name','clust')
tracks_CLust = tracks_CLust[,metaCol]
quartz()
ggplot(dfts, aes(pc1,pc2,color=as.factor(kMeansTrk$cluster))) + geom_point() +geom_text(aes(label=data_Bk$track_name))

quartz()
ggplot(dfts, aes(pc1,pc2,color=as.factor(kMeansTrk$cluster))) + geom_point() +geom_text(aes(label=data_Bk$genre))

tracks_CLust$clust = kMeansTrk$cluster
metaCol = c('album', 'artist', 'genre', 'track_name','clust')
tracks_CLust = tracks_CLust[,metaCol]




ggplot(df, aes(pc1,pc2,color=as.factor(kMeansTrk$cluster))) + geom_point() #+ geom_text(aes(label=tracks$positivity_level))




#################IMP CODE ###################
tracks = read.csv('dataWithLabel1000.csv')
colsList = c('acousticness','danceability', 'energy', 'instrumentalness','liveness', 'loudness', 'speechiness', 'tempo', 'valence','positivity_level','energy_level')

tracksColFil = tracks[,colsList]

#Transform data using tsne

library("Rtsne")
data <- tracksColFil
data_Bk <- tracks
track.measures <- c("acousticness","danceability","duration_ms","energy",
                    "instrumentalness","liveness","loudness","speechiness","tempo",
                    "valence","energy_level","positivity_level")

#data = data[complete.cases(data), ]
#data = unique(data)

#data_Bk = data_Bk[complete.cases(data_Bk), ]
#data_Bk = unique(data_Bk)
nrow(data_Bk)

nrow(data)
data <- data[,track.measures]
data.scaled <- data
data.scaled <- scale(data)

Labels <-data$genre
colors  = rainbow(length(unique(Labels)))
names(colors) = unique(Labels)

#data.scaled = data.scaled[complete.cases(data.scaled), ]
tsne <- Rtsne(data.scaled, dims = 5, perplexity=30, verbose=TRUE, max_iter = 2000,check_duplicates = FALSE)


d = dist(tsne$Y)



dim(as.matrix(tsne$Y))
dim(as.matrix(d))


dfts = data.frame(pc1=tsne$Y[,1],pc2=tsne$Y[,2])
pc1=tsne$Y[,1]
pc2=tsne$Y[,2]
##

#Finding our best number of cluster for Kmeans
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- d
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10,iter.max = 10 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
####

############# KMedoids
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- d
wss <- sapply(1:k.max, 
              function(k){fastkmed(d, k, iterate = 10)$minimum_distance})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
####

nb <- NbClust(tsne$Y, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "median")


#####



#Kmed
kmedFast = fastkmed(d, 5, iterate = 10)
pamKmed = pam(tsne$Y, 5, metric = "euclidean", stand = FALSE)
round(kmedFast$minimum_distance)

pamKmed

#############

#Best clust Kmeans
kMeansTrk = kmeans(d,centers = 5,iter.max = 10)
kMeansTrk$tot.withinss

table(kMeansTrk$cluster,data_Bk$energy_level)
table(kMeansTrk$cluster,data_Bk$positivity_level)

tracks_CLust = tracks

tracks_CLust$clust = kMeansTrk$cluster
metaCol = c('album', 'artist', 'genre', 'track_name','clust')
tracks_CLust = tracks_CLust[,metaCol]

dfts = data.frame(pc1=tsne$Y[,1],pc2=tsne$Y[,2])

#Plot for Kmeans
kMeanClust = as.factor(kMeansTrk$cluster)
ggplot(dfts, aes(pc1,pc2,color=kMeanClust)) + geom_point() +geom_text(aes(label=data_Bk$track_name)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

ggplot(dfts, aes(pc1,pc2,color=kMeanClust)) + geom_point() +geom_text(aes(label=data_Bk$genre)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

ggplot(dfts, aes(pc1,pc2,color=kMeanClust)) + geom_point() +geom_text(aes(label=data_Bk$artist)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

#Plot for KMed
kMedoids_Cluster = as.factor(kmedFast$cluster)
ggplot(dfts, aes(pc1,pc2,color=kMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$track_name)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")
ggplot(dfts, aes(pc1,pc2,color=kMedoids_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$genre)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")


#Plot for Pam Kmed
ggplot(dfts, aes(pc1,pc2,color=as.factor(pamKmed$clustering))) + geom_point() +geom_text(aes(label=data_Bk$track_name))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")
ggplot(dfts, aes(pc1,pc2,color=as.factor(pamKmed$clustering))) + geom_point() +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")


tracks_CLust$clust = kMeansTrk$cluster


#Better plots
#Kmeans
fviz_cluster(kMeansTrk, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

fviz_cluster(kMeansTrk, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$track_name))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")


#KMed
fviz_cluster(pamKmed, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

fviz_cluster(pamKmed, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$track_name))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")


#Hclust
fviz_cluster(hc, data = tsne$Y, geom = "point",
             stand = FALSE, frame.type = "norm") +geom_text(aes(label=data_Bk$genre))+ xlab("t-SNE Component 1") + ylab("t-SNE Component 2")


#Best cluster
#Kmeans
fviz_nbclust(tsne$Y, kmeans, method = "wss")
#Kmed
fviz_nbclust(tsne$Y, pam, method = "wss") 
#hclust
fviz_nbclust(tsne$Y, hcut, method = "wss") 



#Hclust
# Hierarchical clustering results
hc <- hclust(d, method = "complete")
# Visualization of hclust
plot(hc, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(hc, k = 5, border = 2:4) 
hc.cut <- cutree(hc, k = 5)


h_Cluster = as.factor(hc.cut)
ggplot(dfts, aes(pc1,pc2,color=h_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$track_name)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")
ggplot(dfts, aes(pc1,pc2,color=h_Cluster)) + geom_point() +geom_text(aes(label=data_Bk$genre)) + xlab("t-SNE Component 1") + ylab("t-SNE Component 2")

head(hc.cut, 20)
