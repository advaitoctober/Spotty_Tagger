rm(list=ls())
setwd('C:/DragonBallZ/git_Repo/Spotty_Tagger')


library(corrplot)
require(class)
require(gdata)

data <- read.csv('dataWithLabel1000.csv')


colsList = c('acousticness','danceability', 'energy', 'instrumentalness','liveness', 'loudness', 'speechiness', 'tempo', 'valence','positivity_level' , 'energy_level')

tracksColFil = data[,colsList]


# filling NA values with 0 

tracksColFil$energy_level <- ifelse(is.na(tracksColFil$energy_level), 0, tracksColFil$energy_level)
tracksColFil$positivity_level <- ifelse(is.na(tracksColFil$positivity_level), 0, tracksColFil$positivity_level)

pairs(tracksColFil)

corrData <- cor(tracksColFil)
corrplot(corrData, method = "circle")
corrplot(corrData, method = "number") 



