options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(gridExtra)
library(grid)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#distribution by sex
# titanic %>% ggplot(aes(x=Age, group=Sex, fill=Sex)) +
# geom_density(alpha=0.4)
  #geom_boxplot()

#density
maleD <- titanic %>% filter(Sex == "male") %>% ggplot(aes(x=Age)) +
  geom_density(alpha=0.4, fill="blue")

femaleD <- titanic %>% filter(Sex == "female") %>% ggplot(aes(x=Age)) +
  geom_density(alpha=0.4, fill="blue")

# grid.arrange(maleD, femaleD, ncol = 2,
#              top = textGrob("Densities",gp=gpar(fontsize=20,font=3)))

#histograms
maleH <- titanic %>% filter(Sex == "male") %>% ggplot(aes(x=Age)) +
geom_histogram(binwidth = 2, fill = "blue", col = "black")

femaleH <- titanic %>% filter(Sex == "female") %>% ggplot(aes(x=Age)) +
  geom_histogram(binwidth = 2, fill = "blue", col = "black")

# grid.arrange(maleH, femaleH, ncol = 2,
#              top = textGrob("Histograms",gp=gpar(fontsize=20,font=3)))

#qqplot
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

qqp <- titanic %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) +
  geom_abline() + 
  ggtitle("QQ Plot")

# qqp

#barplots
# titanic %>% ggplot(aes(x=Survived, group=Sex, fill=Sex)) +
# geom_bar()
# titanic %>% ggplot(aes(x=Survived)) +
# geom_bar()

# titanic %>% ggplot(aes(x=Age, fill=Survived)) + 
#   geom_density(alpha=0.2, stat="count")

#boxplots
# titanic %>% filter(Fare>0) %>% ggplot(aes(Fare, Survived, fill=Survived)) +
# scale_x_continuous(trans="log2") +
# geom_boxplot() +
# geom_jitter(width = 0.1, alpha = 0.2) 

#More barplots
# titanic %>% ggplot(aes(x=Pclass, fill=Survived)) +
# geom_bar()
# 
# titanic %>% ggplot(aes(x=Pclass, fill=Survived)) +
#   geom_bar(position = position_fill())
# 
# titanic %>% ggplot(aes(x=Survived, fill=Pclass)) +
#   geom_bar(position = position_fill())

#grid density
titanic %>% ggplot(aes(Age, fill=Survived)) +
  geom_density(alpha=0.4, stat = "count") +
  facet_grid(Sex~Pclass)


