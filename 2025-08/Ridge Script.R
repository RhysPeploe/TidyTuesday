library(tidyverse)
library(ggridges)
library(ggplot2)

article_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/article_dat.csv')

ggplot(article_dat, aes(x = year, y = jabbrv, fill = jabbrv)) +
  geom_boxplot() +
  theme_ridges()

ggplot(article_dat, aes(x = year, y = jabbrv, fill = jabbrv)) +
  geom_violin() +
  theme_ridges()

ggplot(article_dat, aes(x = year, y = jabbrv, fill = jabbrv)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges()+ 
  theme(legend.position = "none")+
  labs(title = "Number of Reproductive Medicine Articles by Time, Split by Journal")+
  facet_wrap(~access_to_care)

ggplot(article_dat, aes(x = year, y = jabbrv, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  theme_ridges()+ 
  theme(legend.position = "none") +
  scale_fill_viridis() +
  labs(title = "Number of Reproductive Medicine Articles by Time, Split by Journal")+
  facet_wrap(~access_to_care)

table(article_dat$access_to_care, article_dat$jabbrv, useNA = "ifany")

ggplot(article_dat %>% 
         filter(jabbrv != "BJOG",
                jabbrv != "Hum Reprod",
                !is.na(access_to_care)), aes(x = year, y = jabbrv, fill = jabbrv)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges()+ 
  theme(legend.position = "none")+
  labs(title = "Number of Reproductive Medicine Articles by Time, Split by Journal")+
  facet_wrap(~access_to_care)

ggplot(article_dat %>% 
         filter(jabbrv != "BJOG",
                jabbrv != "Hum Reprod",
                !is.na(access_to_care)) %>% 
         mutate(access_to_care = case_when(
           access_to_care == 0 ~ "Not Addressed",
           access_to_care == 1 ~ "Addressed",
         )), aes(x = year, y = jabbrv, fill = jabbrv)) +
  geom_density_ridges(alpha = 0.5, scale = 1.2) +
  theme_ridges()+
  theme(legend.position = "none")+
  labs(title = "Addressing Access to Care in Reproductive Medicine Articles",
       subtitle = "Density of articles over time, by journal. Journals will 5 or less data points omitted.",
       y = "Journal",
       x = "Year")+
  facet_wrap(~access_to_care, ncol = 1)+
  scale_fill_manual(values = c("#071013", "#23B5D3", "#75ABBC", "#A2AEBB"))
