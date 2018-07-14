# load libraries
library(dplyr)
library(downloader)
library(rafalib)
# load files from github
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
# split into two birth weight datasets
bwt.nonsmoke <- filter(babies, smoke==0) %>% 
  select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% 
  select(bwt) %>% unlist
# find the true population difference in means
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
# E1
set.seed(1)
ns.sample <- sample(bwt.nonsmoke, 50)
s.sample <- sample(bwt.smoke, 50)
t.test(ns.sample, s.sample)$p.value
#E2
pTrials <- function(){
  ns.sample <- sample(bwt.nonsmoke, 5)
  s.sample <- sample(bwt.smoke, 5)
  return(t.test(ns.sample, s.sample)$p.value < 0.05)
}
set.seed(1)
pvals <- replicate(10000, pTrials())
length(pvals[pvals==TRUE]) / length(pvals)
# E3
pTrials <- function(size){
  ns.sample <- sample(bwt.nonsmoke, size)
  s.sample <- sample(bwt.smoke, size)
  return(t.test(ns.sample, s.sample)$p.value < 0.05)
}
set.seed(1)
pvals <- replicate(10000, pTrials(30))
length(pvals[pvals==TRUE]) / length(pvals)
pvals <- replicate(10000, pTrials(60))
length(pvals[pvals==TRUE]) / length(pvals)
pvals <- replicate(10000, pTrials(90))
length(pvals[pvals==TRUE]) / length(pvals)
pvals <- replicate(10000, pTrials(120))
length(pvals[pvals==TRUE]) / length(pvals)
# E4
pTrials <- function(size, alpha){
  ns.sample <- sample(bwt.nonsmoke, size)
  s.sample <- sample(bwt.smoke, size)
  return(t.test(ns.sample, s.sample)$p.value < alpha)
}
set.seed(1)
pvals <- replicate(10000, pTrials(30,0.01))
length(pvals[pvals==TRUE]) / length(pvals)
pvals <- replicate(10000, pTrials(60,0.01))
length(pvals[pvals==TRUE]) / length(pvals)
pvals <- replicate(10000, pTrials(90,0.01))
length(pvals[pvals==TRUE]) / length(pvals)
pvals <- replicate(10000, pTrials(120,0.01))
length(pvals[pvals==TRUE]) / length(pvals)