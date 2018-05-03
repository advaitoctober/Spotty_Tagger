library("ggplot2")
data <- read.csv("/Users/advait/Documents/GitHub/Spotty_Tagger/dataWithLabel1000.csv")


#Each genre
#make a bar graph

ggplot(data, aes(x=factor(genre)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + xlab("Genre") + ylab("Count") + ggtitle("Genre Count")

#average features for each genre
data[is.na(data)] <- 0

#speechiness
ggplot(data, aes(x=genre,y=speechiness))+
  geom_bar(stat="summary", width=0.7, fill="steelblue")+
  theme_minimal() + ggtitle("Speechiness")

#acousticness
ggplot(data, aes(x=genre,y=acousticness))+
  geom_bar(stat="summary", width=0.7, fill="steelblue")+
  theme_minimal() + ggtitle("acousticness")

#danceability
ggplot(data, aes(x=genre,y=danceability))+
  geom_bar(stat="summary", width=0.7, fill="steelblue")+
  theme_minimal() + ggtitle("danceability")

#energy
ggplot(data, aes(x=genre,y=energy))+
  geom_bar(stat="summary", width=0.7, fill="steelblue")+
  theme_minimal() + ggtitle("energy")

#valence
ggplot(data, aes(x=genre,y=valence))+
  geom_bar(stat="summary", width=0.7, fill="steelblue")+
  theme_minimal() + ggtitle("valence")





