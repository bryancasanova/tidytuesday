library(tidyverse)

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv') %>% 
  as_tibble()


##### Birth register by sex #####
babynames %>% 
  group_by(year, sex) %>% 
  summarize(borns = sum(n)) %>% 
  ggplot(aes(x = year, y = borns, colour = sex)) +
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = seq(1880, 2020, 20)) + 
    theme_bw() +
    theme(legend.position = "bottom")
  #Comment: There are some interesting slopes with an increasing birth certificates aroind 1920 and 1060


##### The difference in these registers were always the same? let's find out ####
babynames %>% 
  group_by(year, sex) %>% 
  summarize(borns = sum(n)) %>% 
  spread(sex, borns) %>% 
  mutate(dif = F-M) %>% 
  ggplot(aes(x = year, y = dif)) +
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = seq(1880, 2020, 20)) + 
    theme_bw() +
    labs(y = "Difference between female and males birth registers") + 
    theme(legend.position = "bottom")
 #Comment: Apparently until 1940 there were more female birth registers, since point they have more male registers

#### Let's see the evolution of the 3 most common females names from 1880 to 1920 ####

common_names <- babynames %>% 
  filter(year <= 1900,
         sex == "F") %>% 
  group_by(name) %>% 
  summarize(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  pull(name) %>% 
  head(5)

babynames %>% 
  filter(sex == "F",
         name %in% common_names) %>% 
  ggplot(aes(x = year, y = n, colour = name)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) + 
  theme_bw() +
  theme(legend.position = "bottom")

 #Comment: The amount of Mary's born between 1910 to 1970 is impressive,
# On the other names, this represents that names become popular for a while, then it decreases, but in a few years, they became popular again
