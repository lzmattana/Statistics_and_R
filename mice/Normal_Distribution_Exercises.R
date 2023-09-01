library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
?set.seed
#Normal Distribution Exercises #1
library(rafalib) 
###mypa(1,2)r is optional. it is used to put both plots on one page
mypar(1,2)
hist(averages5, xlim=c(18,30))
hist(averages50, xlim=c(18,30))
#Normal Distribution Exercises #2
mean( averages50 < 25 & averages50 > 23)
#Normal Distribution Exercises #3
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 








