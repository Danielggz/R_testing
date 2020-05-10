library(gtools)
library(tidyverse)

#Olympic runners
jamaica <- 3
other <- 5

#permutations of three medals in 8 runners
marks <- permutations(8, 3)
nrow(marks)

#probability of 3 medals for jamaicans
3/8 * 2/7 * 1/6

#Monte carlo simulation for three medals for jamaican probability
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
results <- replicate(10000, {
  result <- sample(runners, 3)
  sum((result == "Jamaica"))
})
mean(results == 3)


#<------------------------------------------------------->
#Restaurant
entree <- combinations(6, 1)
sides <- combinations(6, 3)
drink <- combinations(3, 1)

total<- nrow(entree) * nrow(sides) * nrow(drink)
total

dafunc <- function(side_choices){
  #entree options needed for different total of combinations
  entree <- combinations(6, 1)
  sides <- combinations(side_choices, 2)
  drink <- combinations(3, 1)
  
  total<- nrow(entree) * nrow(sides) * nrow(drink)
  total
}

sapply(2:12, dafunc)

#assesment 3 - tobacco and shit

#probability of being cancer case in most drunk group
cancer_cases <- esoph %>%  filter(alcgp=="120+") %>% pull(ncases) %>% sum()
control_num <- esoph %>%  filter(alcgp=="120+") %>% pull(ncontrols) %>% sum()

cancer_cases/(cancer_cases + control_num)

#probability of being cancer case in lowest drunk group
cancer_cases <- esoph %>%  filter(alcgp=="0-39g/day") %>% pull(ncases) %>% sum()
control_num <- esoph %>%  filter(alcgp=="0-39g/day") %>% pull(ncontrols) %>% sum()

cancer_cases/(cancer_cases + control_num)

#if the person has cancer, probability of smoking => 10g per day
all_cases <- sum(esoph$ncases)
total_controls <- sum(esoph$ncontrols)
nsmokers <- esoph %>% filter(tobgp !="0-9g/day") %>% pull(ncontrols) %>% sum()

#probability that person who has cancer is a smoker
esoph %>% filter (tobgp != "0-9g/day") %>% summarize (sum(ncases) /all_cases)
esoph %>% filter (tobgp != "0-9g/day") %>% summarize (sum(ncontrols) /total_controls)

#probability of being in drunk group for cancer cases
esoph %>% filter (alcgp =="120+") %>% summarize (sum(ncases) /all_cases)
#probability of being in tobacco heavy smoker group for cancer cases
esoph %>% filter (tobgp == "30+") %>% summarize (sum(ncases) /all_cases)
#probability of being in both groups
esoph %>% filter (tobgp == "30+" | alcgp =="120+") %>% summarize (sum(ncases) /all_cases)

#probability of being in drunk group for control cases
esoph %>% filter (alcgp =="120+") %>% summarize (sum(ncontrols) / total_controls)
#probability of being in heavy smoker group for control cases
esoph %>% filter (tobgp == "30+") %>% summarize (sum(ncontrols) / total_controls)
#probability of being in both groups for control cases
esoph %>% filter (alcgp =="120+" | tobgp == "30+") %>% summarize (sum(ncontrols) / total_controls)
