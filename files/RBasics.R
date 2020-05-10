#libraries
library(dslabs)
library(tidyverse)

#code
data(murders)
data(movielens)

#add murder rate
murders <- mutate(murders, rate = murders$total/murders$population * 100000)

lowrate <- which(murders$rate < 0.2)

ifelse(any(lowrate), print(murders$state[lowrate]), print("No state is so peaceful"))

funcavg <- function(vector)
{ 
  s <- sum(vector)
  n <- length(vector)
  s/n
}

funcavg(c(4, 8, 10, 7.5, 3.5, 5.75, 6))
