rm(list=ls())
setwd('C:/DragonBallZ/Spring2018/STA546/ProjectProposal')

library(ggplot2)

tracks = read.csv('RockMusicInfo.csv')


summary(tracks)

#colsList = c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 'key','liveness', 'loudness', 'mode', 'speechiness', 'tempo','time_signature', 'valence')
colsList = c('acousticness','danceability', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence')


tracksColFil = tracks[,colsList]

trackPCA = prcomp(tracksColFil)
pc1 = trackPCA$x[,1]
pc2 = trackPCA$x[,2]
pc3 = trackPCA$x[,3]
rockType = as.character(tracks[,'genre'])
df = data.frame(rockType,pc1,pc2)


ggplot(df, aes(pc1,pc2 , color=rockType)) + geom_point() + geom_text(aes(label=rockType))


pc_ex2 <- prcomp(tracksColFil, scale = TRUE)
biplot(pc_ex2)
biplot(princomp(tracksColFil),choices=c(1,3))
