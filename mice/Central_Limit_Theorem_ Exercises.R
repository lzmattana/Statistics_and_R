library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
#Central Limit Theorem Exercises #1
?pnorm
pnorm(1)-pnorm(-1)
#Central Limit Theorem Exercises #2
pnorm(2)-pnorm(-2)
#Central Limit Theorem Exercises #3
pnorm(3)-pnorm(-3)
#Central Limit Theorem Exercises #4
library(dplyr)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=1 )
#Central Limit Theorem Exercises #5
#What proportion of these numbers are within two standard deviations away from the list’s average?
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=2 )
#Central Limit Theorem Exercises #6
mean( abs(z) <=3 )
#Central Limit Theorem Exercises #7
library(rafalib)
mypar(1,1)
qqnorm(z)
abline(0,1)
#Central Limit Theorem Exercises #8
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
#Central Limit Theorem Exercises #9
popsd(avgs)
