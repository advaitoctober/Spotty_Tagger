rm(list=ls())
setwd('C:/DragonBallZ/git_Repo/Spotty_Tagger')


library(corrplot)
require(class)
require(gdata)

data <- read.csv('dataWithLabel1000.csv')

colsList = c('acousticness','danceability', 'energy', 'instrumentalness','liveness', 'loudness', 'speechiness', 'tempo', 'valence')

tracksColFil = data[,colsList]

pairs(tracksColFil)

corrData <- cor(tracksColFil)
corrplot(corrData, method = "circle")
corrplot(corrData, method = "number") 
