rm(list=ls())
library(ggplot2)
library("multtest")
library("fpc")
library("cluster")
#setwd("~/Documents/GitHub/Spotty_Tagger")
setwd("C:/DragonBallZ/git_Repo/Spotty_Tagger")

tracks = read.csv('tracksMusicInfo.csv')

summary(tracks)

#colsList = c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 'key','liveness', 'loudness', 'mode', 'speechiness', 'tempo','time_signature', 'valence')
colsList = c('acousticness','danceability', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence')

tracksColFil = tracks[,colsList]

trackPCA = prcomp(tracksColFil)
pc1 = trackPCA$x[,1]
pc2 = trackPCA$x[,2]
pc3 = trackPCA$x[,3]
pc4 = trackPCA$x[,4]
pc5 = trackPCA$x[,5]

artist = as.character(tracks[,'track_name'])
artist = as.character(tracks[,'artist'])

df = data.frame(artist,pc1,pc2)



ggplot(df, aes(pc4,pc5,color=tracks$genre)) + geom_point() #+ geom_text(aes(label=artist))


#####################################
# biplot 
tracksNumeric <- tracks[,c(6:18)]  
pc_sdata <- prcomp(tracksColFil, center = TRUE, scale = TRUE)
biplot(pc_sdata)
#####################################


d<- dist(tracksColFil)

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

####################################
# KMediods
####################################
kmed <- pamk(tracksColFil)

# let the program decide "optimal k"
kmed$nc # getting this value as 3

# tabulate the results
table(kmed$pamobject$clustering, tracks$genre)

x11()
layout(matrix(c(1,2),1,2))
plot(kmed$pamobject)
#####################################
# K means
#####################################
d = dist(tracksColFil)
kMeansTrk = kmeans(d,centers = 9)
table(kMeansTrk$cluster,tracks$genre)
