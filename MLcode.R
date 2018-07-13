# rm(list=ls())
# cat("\014")   
library(datasets)
library(dplyr)
library(ggplot2)

data(iris)

x <- select(iris, Sepal.Length, Petal.Length)
newx <- data.frame(replicate(2,sample(0:7,150,rep=TRUE)))

SSvals <- vector()
for(i in 1:5){
  cl <- stats::kmeans(x, centers = i, nstart = 10)
  SSvals[i] <- cl$tot.withinss
}

plot(1:5, SSvals)
#plotKmeans <- ggplot(x, aes(x = Sepal.Length,
#                    y = Petal.Length,colour = factor(cl$cluster))) + geom_point()
#plotKmeans

# Hierarchal clustering
d <- dist(iris[, 1:4])
hcl <- hclust(d)
hcl
plot(hcl)
ctk <- cutree(hcl, k = 3)
cth <- cutree(hcl, h = 3.8)
clkm <- stats::kmeans(x, centers = 3, nstart = 10)
clk <- clkm$cluster
# which is better??
par(mfrow = c(1,2))
plot(iris$Petal.Length, iris$Sepal.Length, col = clk, main = "k-means");
plot(iris$Petal.Length, iris$Sepal.Length, col = cth, main = "Hierarchal Model");
# use table to find that out
table(iris$Species, clk)
table(iris$Species, cth)
# Pre-processing the data
data(mtcars)
# verify they are of different scales
# scale data
mtcarsScaled <- scale(mtcars)
mtclh <- hclust(dist(mtcarsScaled))
mtunclh <- hclust(dist(mtcars))
par(mfrow=c(1,2))
plot(mtclh)
plot(mtunclh)

table(iris$Species, clk)
table(iris$Species, cth)

# using PCA to reduce dimensions in iris data
par(mfrow=c(1,1))
irispca <- prcomp(iris[, -5])
summary(irispca)

plot(irispca)
biplot(irispca)

PoV <- irispca$sdev^2/sum(irispca$sdev^2)
barplot(PoV, names.arg = c("PC1", "PC2", "PC3", "PC4"))

plot(irispca$x[,1], irispca$x[,2], col = iris$Species)
plot(irispca$x[,3], irispca$x[,4], col = iris$Species)

pcaData <- c(irispca$x[,1], irispca$x[,2])
rcl <- stats::kmeans(pcaData, centers = 3, nstart = 10)