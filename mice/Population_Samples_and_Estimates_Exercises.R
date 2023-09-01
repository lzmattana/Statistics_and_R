library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )
#Population, Samples, and Estimates Exercises #1
library(dplyr)
x <- filter(dat, Sex == 'M' & Diet == 'chow') %>% select(Bodyweight) %>% unlist()
mean(x)
#Population, Samples, and Estimates Exercises #2
library(rafalib)
popsd(x)
#Population, Samples, and Estimates Exercises #3
set.seed(1)
X <- sample(x,25)
mean(X)
#Population, Samples, and Estimates Exercises #4
y <- filter(dat, Sex=='M' & Diet=='hf') %>% select(Bodyweight) %>% unlist()
mean(y)
#Population, Samples, and Estimates Exercises #5
popsd(y)
#Population, Samples, and Estimates Exercises #6
set.seed(1)
Y <- sample(y,25)
mean(Y)
#Population, Samples, and Estimates Exercises #7
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )
#Population, Samples, and Estimates Exercises #8
x <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(2)
X <- sample(x,25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
set.seed(2)
Y <- sample(y,25)
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )





