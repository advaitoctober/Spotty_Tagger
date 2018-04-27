rm(list = ls())
set.seed(7)
library(ggplot2)
data <- read.csv("/Users/advait/Documents/GitHub/Spotty_Tagger/dataWithLabel.csv")
library("kohonen")

track.measures <- c("acousticness","danceability","duration_ms","energy",
                    "instrumentalness","liveness","loudness","speechiness","tempo",
                    "valence","positivity_level","energy_level")
data <- data[,track.measures]

data.scaled <- scale(data)

som.model <- som(as.matrix(data.scaled),grid=somgrid(xdim=10,ydim = 10))

mydata <- as.data.frame(som.model$codes)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

som_cluster <- cutree(hclust(dist(mydata)), 6)
plot(som.model, type="mapping", bgcol = coolBlueHotRed(8), main = "Clusters") 
add.cluster.boundaries(som.model, som_cluster)


##tsne
library("Rtsne")
data <- read.csv("/Users/advait/Documents/GitHub/Spotty_Tagger/dataWithLabel.csv")
data_Bk <- read.csv("/Users/advait/Documents/GitHub/Spotty_Tagger/dataWithLabel.csv")
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
tsne <- Rtsne(data.scaled, dims = , perplexity=30, verbose=TRUE, max_iter = 500,check_duplicates = FALSE)


dfts = data.frame(pc1=tsne$Y[,1],pc2=tsne$Y[,2])
ggplot(dfts, aes(pc1,pc2,color=as.factor(data_Bk$mood))) + geom_point() #+geom_text(aes(label=data_Bk$genre))
text(tsne$Y, labels=Labels, col=colors[Labels])

