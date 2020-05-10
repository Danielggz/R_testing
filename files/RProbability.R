beads <- rep(c("red", "blue"), times = c(2,3))
beads
#sample
sample(beads, 3) #choose one of the array | repeat three times 
sample(beads, 3, replace = TRUE) #use replacement

# events <- replicate(10000, sample(beads, 1)) #repeat sample x times
# eventsTab <- table(events) #make a table of the events
# prop.table(eventsTab) #get proportions of table

#ASESSMENT 1 
balls <- rep(c("cyan", "magenta", "yellow"), times = c(3, 5, 7))
balls

#probabilities of colors
mean(balls == "cyan")
mean(balls == "magenta")
mean(balls == "yellow")

#What is the probability that the first draw is cyan 
#and that the second draw is not cyan?

#number of each color
cyanb <- sum(balls == "cyan")
magentab <- sum(balls == "magenta")
yellowb <- sum(balls == "yellow")

#no replacement
cyanb/(cyanb + magentab + yellowb) * (magentab + yellowb)/(cyanb + magentab + yellowb - 1)
#replacement
cyanb/(cyanb + magentab + yellowb) * (magentab + yellowb)/(cyanb + magentab + yellowb)

#generating deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

#select options which includes 'King'
kings <- paste("King", suits) 
#probability of taking one of the options
mean(deck %in% kings)

#permutations and combinations
library(gtools)
permutations(3,2)    # order matters
combinations(3,2)    # order does not matter


#birthdays
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


#function to compute probabilities
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

#plot of monte carlo simulations
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

#prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
#plot(log10(B), prob, type = "l")    # plot a line graph of estimates 


#<----------------------------------------------------------------->
#Continuous probability
F <- function(x, a) mean(x <= a)

#heights example
library(tidyverse)
library(dslabs)
data(heights)
maleh <- heights %>% mutate(height = height * 2.54) %>% filter(sex=="Male") %>% pull(height)

#probability of male taller than 180
1 - F(maleh, 177.8)

1 - pnorm(179.07, mean(maleh), sd(maleh))

#draw density plot with dnorm
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()