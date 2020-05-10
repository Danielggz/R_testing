# An old version of the SAT college entrance exam had a 
# -0.25 point penalty for every incorrect answer and 
# awarded 1 point for a correct answer. 
# The quantitative test consisted of 44 multiple-choice questions each 
# with 5 answer choices. 
# Suppose a student chooses answers by guessing for all questions on the test.

#<--------------------------------------------------------------------------->
incorrect <- -0.25
correct <- 1
nquestions <- 44
options <- 5

#What is the probability of guessing correctly for one question?
p_correct <- 1/5
p_incorrect <- 1 - p_correct

#What is the expected value of points for guessing on one question?
correct * p_correct + incorrect * p_incorrect
#What is the expected score of guessing on all 44 questions?
nquestions * (correct * p_correct + incorrect * p_incorrect)

#What is the standard error of guessing on all 44 questions?
sqrt(nquestions) * abs(incorrect - correct) * sqrt(p_correct * p_incorrect)

#Use the Central Limit Theorem to determine the probability that 
# a guessing student scores 8 points or higher on the test.
avg <- nquestions * (correct * p_correct + incorrect * p_incorrect)
sd <- sqrt(nquestions) * abs(incorrect - correct) * sqrt(p_correct * p_incorrect)
1 - pnorm(8, avg, sd)

#set seed to 21
#set.seed(21, sample.kind = "Rounding")
#Monte Carlo simulation to 10000 students guessing on the test
B <- 10000 
S <- replicate(B, {
  X <- sample(c(correct,incorrect), nquestions, replace = TRUE, prob = c(p_correct, p_incorrect))
  sum(X)
})
mean(S>=8)




#<-------------------------------------------------------------------------->


#The SAT was recently changed to reduce the number of multiple choice options 
#from 5 to 4 and also to eliminate the penalty for guessing.
incorrect <- 0
correct <- 1
nquestions <- 44
options <- 4
p_correct <- 1/4
p_incorrect <- 1 - p_correct

#What is the expected value of the score when guessing on this new test?
nquestions * (correct * p_correct + incorrect * p_incorrect)

#range of correct answer probabilities
p_correct <- seq(0.25, 0.95, 0.05)

#What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p_incorrect <- 1 - p_correct
avg <- nquestions * (correct * p_correct + incorrect * p_incorrect)
sd <- sqrt(nquestions) * abs(incorrect - correct) * sqrt(p_correct * p_incorrect)
1 - pnorm(35, avg, sd) #the answer is 8.445370e-01(0.844537) exeeds 80%


#<-------------------------------------------------------------------------->


#A casino offers a House Special bet on roulette, 
# which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. 
# The bet pays out 6 to 1. 
# In other words, a losing bet yields -$1 and a successful bet yields $6. 
# A gambler wants to know the chance of losing money 
# if he places 500 bets on the roulette House Special.
p_correct <- 5/38
p_incorrect <- 1 - p_correct
correct <- 6
incorrect <- -1
n <- 500

#What is the expected value of the payout for one bet?
correct * p_correct + incorrect * p_incorrect

#What is the standard error of the payout for one bet?
abs(incorrect - correct) * sqrt(p_correct * p_incorrect)

#What is the expected value of the average payout over 500 bets?
n * (correct * p_correct + incorrect * p_incorrect)/n
#NOTE: Expected value of average payout will be sum of expected value divided by n, 
#      which is mu itself. So its the same answer as the expected value

#What is the standard error of the average payout over 500 bets?
(abs(incorrect - correct) * sqrt(p_correct * p_incorrect))/sqrt(n)

#What is the expected value of the sum of 500 bets?
n * (correct * p_correct + incorrect * p_incorrect)

#What is the standard error of the sum of 500 bets?
sqrt(n) * (abs(incorrect - correct) * sqrt(p_correct * p_incorrect))

#Use pnorm() with the expected value of the sum and standard error of the sum to 
#calculate the probability of losing money over 500 bets
avg <- n * (correct * p_correct + incorrect * p_incorrect)
sd <- sqrt(n) * (abs(incorrect - correct) * sqrt(p_correct * p_incorrect))
pnorm(0, avg, sd)


#<-------------------------------------------------------------------------->

