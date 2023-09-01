dat=read.csv("femaleMiceWeights.csv")
library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
obsdiff <- mean(treatment)-mean(control)

#generate a null distribution by shuffling the data 1,000 times
N <- 12
avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)

#the proportion of permutations with larger difference
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)

#Now let’s repeat this experiment for a smaller dataset. We create a smaller dataset by sampling
N <- 5
control <- sample(control,N)
treatment <- sample(treatment,N)
obsdiff <- mean(treatment)- mean(control)

avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)

# Now the observed difference is not significant using this approach. Keep in mind that there is no 
# theoretical guarantee that the null distribution estimated from permutations approximates the actual 
# null distribution. For example, if there is a real difference between the populations, some of the permutations 
# will be unbalanced and will contain some samples that explain this difference. This implies that the null 
# distribution created with permutations will have larger tails than the actual null distribution. 
# This is why permutations result in conservative p-values. For this reason, when we have few samples, 
# we can’t do permutations.
# Note also that permutations tests still have assumptions: samples are assumed to be independent and “exchangeable”.
# If there is hidden structure in your data, then permutation tests can result in estimated null distributions 
# that underestimate the size of tails because the permutations may destroy the existing structure in the original data.