setwd("~/Documents/GitHub/Spotty_Tagger")

tracks = read.csv('billboardTop100.csv')

colsList = c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 'key','liveness', 'loudness', 'mode', 'speechiness', 'tempo','time_signature', 'valence')

tracksColFil = tracks[,colsList]

trackPCA = prcomp(tracksColFil)
pc1 = trackPCA$x[,1]
pc2 = trackPCA$x[,2]
artist = as.character(tracks[,'track_name'])
artist = as.character(tracks[,'artist'])

df = data.frame(artist,pc1,pc2)
plot(df$pc1,df$pc2) 
with(df, text(pc1~pc2, labels = artist), pos = 4)


text(pc1 ~ pc2, labels=artist, cex= 0.7)


ggplot(df, aes(pc1,pc2)) + geom_point() + geom_text(aes(label=artist))


scaledDF = scale(tracksColFil)


trackPCA = prcomp(scaledDF)
pc1 = trackPCA$x[,1]
pc2 = trackPCA$x[,2]
artist = as.character(tracks[,'artist'])

df = data.frame(artist,pc1,pc2)
plot(df$pc1,df$pc2) 
with(df, text(pc1~pc2, labels = artist), pos = 4)


text(pc1 ~ pc2, labels=artist, cex= 0.7)
library(ggplot2)

ggplot(df, aes(pc1,pc2)) + geom_point() + geom_text(aes(label=artist))

summary(tracksColFil)
