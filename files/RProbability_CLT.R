library(tidyverse)

#roulette
green <- 2
black <- 18
red <- 18

#probability of ball lands on green (0 | 00)
p_green <- green/(green + black + red)
#probability of ball not falling on green
p_not_green <- 1 - p_green
#outcomes
a <- 17 #win
b <- -1 #lose

#random variable to predict probability of ball on green
X <- sample(c(17, -1), 1, replace = TRUE, prob = c(p_green, p_not_green))
X

#calculate expected value (Formula => ap + b(1-p))
exp_val <- 17 * p_green + (-1) * p_not_green

#calculate standard error (Formula => |b-a| ???(p(1-p))
abs(a - b) * sqrt(p_green * p_not_green)

#compute de sum of 1000 samples as S
n <- 1000
X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green, p_not_green))
S <- sum(X)

#Expected value of S (sum of n draws of random variable) 
#(Formula => n * (ap + b(1 - p))
n * (a*p_green + b*p_not_green)

#Standard error of S (sum of n draws of random variable) 
#(Formula => ???n * |b - a| * ???p(1-p)
sqrt(n) * abs(a - b) * sqrt(p_green * p_not_green)
