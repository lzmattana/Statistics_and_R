library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)
mammalian<-read.csv("msleep_ggplot2.csv")
class(mammalian)
library(dplyr) 
head(mammalian)
primates <- filter(mammalian, order=="Primates") #keep only the ones with Primates order
nrow(primates)
class(primates)
primateSleep<-filter(mammalian, order=="Primates") %>% select(sleep_total)
class(primateSleep)
mean(primateSleep)
#?unlist
y <- filter(mammalian, order=="Primates") %>% select(sleep_total) %>% unlist
mean(y)
filter(mammalian, order=="Primates") %>% summarise( mean( sleep_total) )
