setwd('C:\\Users\\cnevs\\OneDrive\\Desktop\\GitHub')
read.csv('survey_2018.csv')
read.csv('re_sample_2021.csv')

library(tidyverse)
data_2018 <- read.csv("survey_2018.csv")
data_2021 <- read.csv("re_sample_2021.csv")


# Question 1

# sample 25 mean values from the 2018 data

set.seed(25)
n = 100000

sample_18 <- n %>%
  replicate(sample(data_2018$coverage, 25)) %>%
  apply(2,mean)

# find mean coverage for both datasets

mean(sample_18)

# plot result

ggplot() + 
  geom_histogram(aes(x=sample_18, y=..density..), bins = 25, alpha=0.1, color="black", size=0.05) +
  geom_density(aes(x=sample_18), color="black", size=1)+
#plot 2021 data mean value
geom_vline(aes(xintercept=23.68), color="red") 

#p-value
sum(sample_18 >=23.68)/100000

#p-value = 0.00245 < 0.05, the test hypothesis should be rejected 

