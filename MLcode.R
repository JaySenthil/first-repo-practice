# rm(list=ls())
# cat("\014") 
#####################################################
#This is just a experimental file with a bunch of ML#
#####################################################

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
pairs(iris[,-5], col = iris[,5], pch = 19)
irispca <- prcomp(iris[,-5])
summary(irispca)
var <- irispca$sdev^2
(pve <- var/sum(var))
cumsum(pve)
# Preparing a barplot of percentage of variance
barplot(pve, names.arg = c("PC1", "PC2", "PC3", "PC4"))

# Repeat on mtcars with PCA compare the need for scaling
par(mfrow = c(1, 2))
biplot(prcomp(mtcars, scale = FALSE), main = "No scaling")  ## 1
biplot(prcomp(mtcars, scale = TRUE), main = "With scaling") ## 2

# Working with t-SNE
library("Rtsne")
uiris <- unique(iris[,1:5])
iristsne <- Rtsne(uiris[,1:4])
plot(iristsne$Y, col = uiris$Species)

# K-NN
set.seed(12L)
tr <- sample(150, 50)
nw <- sample(150, 50)
library(class)
knnres <- knn(iris[tr, -5], iris[nw, -5], iris$Species[tr])
head(knnres)
table(knnres, iris$Species[nw])
# How does k influence my dataset
set.seed(12L)
tr <- sample(150, 50)
nw <- sample(150, 50)
knnress_2 <- knn(iris[tr, -5], iris[nw, -5], iris$Species[tr], k= 5, prob = TRUE)
table(knnress_2, iris$Species[nw])