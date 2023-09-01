library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
#Null Distributions Exercises #1
set.seed(1)
n <- 1000
ave5 <- vector('numeric', n)
for(i in 1:n){
  X <- sample(x,5)
  ave5[i] <- mean(X)
}
hist(ave5) ##take a look
mean( abs( ave5 - mean(x) ) > 1)
#Null Distributions Exercises #2
set.seed(1)
n <- 10000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look
mean( abs( averages5 - mean(x) ) > 1)
