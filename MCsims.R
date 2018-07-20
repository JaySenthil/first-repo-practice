set.seed(1)
size = 5;
t = vector('numeric')
for (i in 1:1000) {
  s <- rnorm(size)
  t[i] = sqrt(size) * mean(s) / sd(s)
}
mean(t>2)
# EXE 3
B = 1000;
ps = seq(1/(B+1), 1-1/(B+1), len=B)
theoreticals <- qt(ps, df = size-1)
qqplot(t, theoreticals)
abline(0,1)
# Ans
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 
# EXE 4
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  
# EXE 6
N <- 1000
B<- 1000
ts <- replicate(B, {
  X <- sample(c(-1,1), N, replace = TRUE)
  sqrt(N) * mean(X) / sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), ts, xlim=range(ts))
abline(0,1)
# EXE 7
N <- seq(5,30,5)
B <- 1000
exacts <- replicate(B, {
  m <- median(rnorm(N))
  sqrt(N) * m / sd()
})

theoretical <- replicate(B, {
  rnorm(0, 1/sqrt(N))
})