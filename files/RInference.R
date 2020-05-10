library(tidyverse)
library(dslabs)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, {
  sp <- sample(c(1,0), N, replace = TRUE, prob = c(p, (1-p)))
  p - mean(sp)
})
# Calculate the mean of the errors. Print this value to the console.
# mean(errors)
# hist(errors)

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)

#plot(se, N)


#<------------------------------------------------------------------->
#us elections 2016
# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(state == "U.S." & enddate >= as.Date("2016-10-31"))

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- head(polls, 1) %>% pull(samplesize)
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- head(polls, 1) %>% pull(rawpoll_clinton)/100
X_hat 
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat * (1 - X_hat) / N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
qnorm(0.975)
ci <- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
ci

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>% 
  mutate(X_hat = rawpoll_clinton/100, 
         se_hat = sqrt(X_hat * (1 - X_hat) / samplesize), 
         lower = X_hat - qnorm(0.975)*se_hat, 
         upper = X_hat + qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper)


# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- head(polls, 1) %>% pull(samplesize)
N

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- head(polls,1) %>% mutate(d_hat = (rawpoll_clinton/100) - (rawpoll_trump/100)) %>% pull(d_hat)
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat + 1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2 * sqrt(X_hat * (1 - X_hat) / N)


# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>% 
  mutate(X_hat = (d_hat+1)/2, 
         se_hat = 2*(sqrt(X_hat*(1-X_hat)/samplesize)), 
         lower = d_hat - qnorm(0.975)*se_hat, 
         upper = d_hat + qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, d_hat, lower, upper)