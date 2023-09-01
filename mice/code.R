install.packages("downloader")
library(downloader) ##use install.packages to install
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
if (!file.exists(filename)) download(url, destfile=filename)
micewt <-read.csv(filename)
## Read in the file femaleMiceWeights.csv and report the exact name of the column containing the weights.
colnames(micewt)
## What is the entry in the 12th row and second column?
micewt[12,2]
weights <- micewt$Bodyweight
weights[11]
##The length() function returns the number of elements in a vector.
##How many mice are included in our dataset? 24
length(micewt$Bodyweight)
## To create a vector with the numbers 3 to 7, we can use seq(3,7) or, because they are consecutive, 3:7. View the data and determine what rows are associated with the high fat or hf diet. 
##Then use the mean function to compute the average weight of these mice.
hfdiet<-micewt[micewt$Diet=="hf",]
mean(hfdiet$Bodyweight)
#?sample
set.seed(1)
i <- sample( 13:24, 1)
micewt$Bodyweight[i]
library(dplyr)
controls <- filter(micewt, Diet=='chow') %>% select(Bodyweight) %>% unlist
mean(controls)
treatment <- filter(micewt, Diet=='hf') %>% select(Bodyweight) %>% unlist
print( mean(treatment) )
print( mean(controls) )
obsdiff <- mean(treatment) - mean(controls)
print(obsdiff)
url2 <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
population <- read.csv(url2)
population <- unlist(population)
# RANDOM VARIABLE
summary(population)
mean(sample(population,12))
# RADOM N GENERATOR
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
#Random Variables Exercises #1
mean(population)
#Random Variables Exercises #2
set.seed(1)
sample<-sample(population,5)
abs(mean(sample)-mean(population))
#Random Variables Exercises #3
set.seed(5)
sample<-sample(population,5)
abs(mean(sample)-mean(population))
#Class forloop
n <- 1000
nulls <- vector('numeric', n)
for(i in 1:n){
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulls[i] <- mean(control)-mean(population)
}
hist(nulls)
# P values
mean(nulls>obsdiff)
mean(abs(nulls)>obsdiff)
library(rafalib)
mypar()
qqnorm(nulls)
qqline(nulls)





