library(tidyverse)
library(janitor)

tuesdata <- tidytuesdayR::tt_load(2026, week = 15)

beaufort_scale <- tuesdata$beaufort_scale
birds <- tuesdata$birds
sea_states <- tuesdata$sea_states
ships <- tuesdata$ships


bird_ship = birds %>% 
  left_join(ships, by = join_by(record_id)) 

names(bird_ship)
bird_ship %>% tabyl(species_common_name)
bird_ship %>% tabyl(species_abbreviation)
bird_ship %>% tabyl(age)
bird_ship %>% tabyl(wan_plumage_phase)
bird_ship %>% tabyl(plumage_phase)
bird_ship %>% tabyl(accompanying)
bird_ship %>% tabyl(hemisphere)
bird_ship %>% tabyl(activity)
bird_ship %>% tabyl(speed)
bird_ship %>% tabyl(direction) # ----
bird_ship %>% tabyl(cloud_cover)
bird_ship %>% tabyl(precipitation)
bird_ship %>% tabyl(wind_direction)


ggplot(bird_ship %>% 
         tabyl(direction), aes(x = direction))+
  geom_histogram(binwidth = 20, bins = 18, boundary = 0,
                 colour = "black", fill = "#92140C") +
  coord_polar(theta = "x")+
  theme(plot.background = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue", colour = "lightblue"),
        axis.minor.ticks.x.bottom = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.title = element_blank())+
  scale_x_continuous(breaks = seq(0, 360, 20)) +
  labs(title = "Ship Direction at Bird Sightings in Te Papa Tongarewa")+
  stat_bin(binwidth = 20, geom = "text", colour = "white", mapping = aes(label =..count..),
           boundary = 0, position = position_stack(vjust = 0.8))

ggsave("TidyTuesday/2026-15/ship_direction_polar.tif", width = 12.8, height = 8.16)

ggplot(bird_ship %>% 
         tabyl(wind_direction), aes(x = wind_direction))+
  geom_histogram(binwidth = 20, bins = 18, boundary = 0,
                 colour = "black", fill = "#FF7F11") +
  coord_polar(theta = "x")+
  theme(plot.background = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue"),
        axis.minor.ticks.x.bottom = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.title = element_blank())+
  scale_x_continuous(breaks = seq(0, 360, 20)) +
  labs(title = "Wind Direction at Bird Sightings in Te Papa Tongarewa")+
  stat_bin(binwidth = 20, geom = "text", colour = "white", mapping = aes(label =..count..),
           boundary = 0, position = position_stack(vjust = 0.8))

ggsave("TidyTuesday/2026-15/wind_direction_polar.tif", width = 12.8, height = 8.16)
