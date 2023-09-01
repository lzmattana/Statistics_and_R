library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)  
#Split population
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
#Look for the true population difference in means between smoking and non-smoking birth weights.
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#Confidence Intervals Exercises #1
N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,2*N-2)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
##note that if you define dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both answers
#Confidence Intervals Exercises #3
N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value

