# load data
library(downloader)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
download(url,destfile=filename)
dat <- read.csv(filename)

# exe 1
set.seed(1)

performSim <- function(p, n){
  x <- sample(1:(1/p), n, replace = TRUE)
  z <- (mean(x==(1/p)) - p) / sqrt(p*(1-p)/n)
  return(z)
}

vals<-replicate(10000, performSim(100))

# Power of a test

# exe 2
vals<-replicate(10000, performSim(0.5, 30))
qqnorm(vals)
abline(0,1)

# exe 3
X <- filter(dat, Diet=="chow") %>% 
  select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% 
  select(Bodyweight) %>% unlist
# t-test exercises
# load data
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
# split data into two groups
bwt.nonsmoke <- filter(babies, smoke==0) %>% 
  select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>%
  select(bwt) %>% unlist
# population difference in means
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
# Exe 1
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, 25)
dat.s <- sample(bwt.smoke, 25)
tval <- abs(mean(dat.ns) - mean(dat.s)) / sqrt(var(dat.ns)/25 + var(dat.s)/25)
print(tval)
#confidence intervals exercises #1
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, 25)
dat.s <- sample(bwt.smoke, 25)
