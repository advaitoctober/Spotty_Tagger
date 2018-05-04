rm(list=ls())
library(ggplot2)
library("multtest")
library("fpc")
library("cluster")
library("lsa")
library("proxy")
setwd("~/Documents/GitHub/Spotty_Tagger")
#setwd("C:/DragonBallZ/git_Repo/Spotty_Tagger")

tracks = read.csv('dataWithLabel.csv')
tracks = tracks[which(tracks$genre == "Rock" | tracks$genre == "Pop"),]
tracks = tracks[complete.cases(tracks), ]
summary(tracks)

colsList = c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 'key','liveness', 'loudness', 'mode', 'speechiness', 'tempo','time_signature', 'valence','positivity_level','energy_level')

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


#####
# Kmeans
#####
d<- dist(tracksColFil)
d = dist(tracksColFil,method = "Cosine")


kMeansTrk = kmeans(d,centers = 5)

table(kMeansTrk$cluster,tracks$energy_level)
table(kMeansTrk$cluster,tracks$positivity_level)

tracks_CLust = tracks

tracks_CLust$clust = kMeansTrk$cluster
metaCol = c('album', 'artist', 'genre', 'track_name','clust')
tracks_CLust = tracks_CLust[,metaCol]


ggplot(df, aes(pc1,pc2,color=as.factor(kMeansTrk$cluster))) + geom_point() #+ geom_text(aes(label=tracks$positivity_level))
ggplot(df, aes(pc1,pc2,color=as.factor(tracks$positivity_level))) + geom_point() #+ geom_text(aes(label=tracks$positivity_level))



################################
library("Rtsne")
data <- tracksColFil
data_Bk <- tracks
track.measures <- c("acousticness","danceability","duration_ms","energy",
                    "instrumentalness","liveness","loudness","speechiness","tempo",
                    "valence","energy_level","positivity_level")

data = data[complete.cases(data), ]
data = unique(data)

data_Bk = data_Bk[complete.cases(data_Bk), ]
data_Bk = unique(data_Bk)
nrow(data_Bk)

nrow(data)
data <- data[,track.measures]
data.scaled <- data
data.scaled <- scale(data)

Labels <-data$genre
colors  = rainbow(length(unique(Labels)))
names(colors) = unique(Labels)

data.scaled = data.scaled[complete.cases(data.scaled), ]
tsne <- Rtsne(data.scaled, dims = 5, perplexity=30, verbose=TRUE, max_iter = 2000,check_duplicates = FALSE)


dfts = data.frame(pc1=tsne$Y[,1],pc2=tsne$Y[,2])
ggplot(dfts, aes(pc1,pc2,color=as.factor(data_Bk$genre))) + geom_point() #+geom_text(aes(label=data_Bk$genre))



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

hc_avg <- hclust(d, method = "complete")
x11()
plot(hc_avg, main="Average Linkage",labels = tracks$mood)

data2clusters <- cutree(hc_avg,5)

hclustResultDF <- data.frame(tracks$artist,tracks$track_name,tracks$album,tracks$genre,data2clusters)

table(data2clusters,tracks$energy_level)
table(data2clusters,tracks$positivity_level)

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

####################################
# KMediods
####################################
kmed <- pamk(tracksColFil)

# let the program decide "optimal k"
kmed$nc # getting this value as 3

# tabulate the results
table(kmed$pamobject$clustering, tracks$genre)

kmed9DF = data.frame(kmed$pamobject$clustering,tracks$artist,tracks$genre,tracks$album,tracks$track_name)

x11()
layout(matrix(c(1,2),1,2))
plot(kmed$pamobject)




########################################################
# Using PCA
trackPCADF = trackPCA$x

d<- dist(trackPCADF )
d = cosine(trackPCADF)


hc_avg <- hclust(d, method = "average")
x11()
plot(hc_avg, main="Average Linkage",labels = tracks$genre)

data2clusters <- cutree(hc_avg,9)
table(tracks$genre,data2clusters)
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

####################################
# KMediods
####################################
kmed <- pamk(tracksColFil)

# let the program decide "optimal k"
kmed$nc # getting this value as 3

# tabulate the results
table(kmed$pamobject$clustering, tracks$genre)

kmed9DF = data.frame(kmed$pamobject$clustering,tracks$artist,tracks$genre,tracks$album,tracks$track_name)

x11()
layout(matrix(c(1,2),1,2))
plot(kmed$pamobject)



