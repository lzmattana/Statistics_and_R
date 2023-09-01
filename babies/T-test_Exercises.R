library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
#split this into two birth weight datasets
library(dplyr)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
# look for the true population difference in means between smoking and non-smoking
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#T-test Exercises #1
set.seed(1)
data.ns <- sample(bwt.nonsmoke,25)
data.s <- sample(bwt.smoke,25)
tval<-t.test(data.ns,data.s)$statistic
tval
#T-test Exercises #2
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))

