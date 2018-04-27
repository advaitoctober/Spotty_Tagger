rm(list = ls())
set.seed(7)

data <- read.csv("/Users/advait/Documents/GitHub/Spotty_Tagger/tracksMusicInfo.csv")
library("kohonen")

track.measures <- c("acousticness","danceability","duration_ms","energy",
                    "instrumentalness","liveness","loudness","speechiness","tempo",
                    "valence")
data <- data[,track.measures]
data.scaled <- scale(data)

som.model <- som(as.matrix(data.scaled),grid=somgrid(xdim=6,ydim = 6))

mydata <- as.data.frame(som.model$codes)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

som_cluster <- cutree(hclust(dist(mydata)), 6)
plot(som.model, type="mapping", bgcol = coolBlueHotRed(8), main = "Clusters") 
add.cluster.boundaries(som.model, som_cluster)





