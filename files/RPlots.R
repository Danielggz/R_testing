#libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
#load data
data(murders)

#add rate to murders
murders %>% mutate(rate = total/population*100000)

#create the rate for abline
r <- murders %>%
    summarize(rate = sum(total) / sum(population) * 10^6) %>%
    pull(rate)
#inicialite the plot p
p <- murders %>% 
    ggplot(aes(population/10^6, total, label = abb)) + 
#add abline before point
    geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
    geom_point(aes(col = region), size = 3) +
    geom_text_repel() + 
    scale_x_log10() + 
    scale_y_log10() + 
#labels and title
    xlab("Population in millions (log scale)") +
    ylab("Total number of murders (log scale)") +
    ggtitle("US Gun Murders in 2010") + 
#Change name of region leyend
    scale_color_discrete(name = "Region") +
#add theme
    theme_economist()

#print plot
p
#<---------------------------------------------------------->

#OTHER PLOTS

#load data
data(heights)

#convert to centimeters (as it should be)
heights <- heights %>% mutate(height = height*2.54)

#create plot
basep <- heights  %>% filter(sex=="Male") %>% ggplot(aes(x=height))
    
#histogram plot
hp <- basep +
  geom_histogram(binwidth = 2, fill = "blue", col = "black") + 
  xlab("Male heights in centimeters") + 
  ggtitle("Histogram")

#smooth density plot
dp <- heights %>% ggplot(aes(x=height, fill=sex)) + 
  geom_density(alpha=0.4) + 
  ggtitle("Smooth density")

#qq plot
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))

qqp <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))

qqp <- qqp + geom_qq(dparams = params) +
  geom_abline() + 
  ggtitle("QQ Plot")

#print qqplot
qqp
#<------------------PRINT GROUP PLOTS------------------------------>
library(gridExtra)
library(grid)
grid.arrange(hp, dp, ncol = 2,
             top = textGrob("Other plots",gp=gpar(fontsize=20,font=3)))
