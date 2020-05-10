library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder2 <- gapminder %>% 
  filter(year %in% c("1970","2010") & continent == "Africa" 
         & !is.na(gdp) & !is.na(infant_mortality) & !is.na(country)) %>%
  mutate(dollars_per_day = gdp/population/365)
  

gapminder2 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) + 
  geom_text(nudge_x = -0.1) + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(.~year)