library(tidyverse)
library(scales)

apod = tidytuesdayR::tt_load(2026, 3)$apod

(apod$media_type %>% table())/nrow(apod)

(apod %>% 
  filter(media_type == "image") %>% 
  nrow())/nrow(apod)

df = tibble(
  x = 1,
  y = c((apod %>% 
         filter(media_type == "image") %>% 
         nrow())/nrow(apod),
        1 - (apod %>% 
               filter(media_type == "image") %>% 
               nrow())/nrow(apod)),
  group = c("highlight", "background")
)
df

ggplot(df, aes(x = x, y = y, fill = group)) +
  geom_col(show.legend = FALSE) +
  coord_polar(theta = "y", direction = -1) +
  xlim(c(-2,2)) +
  scale_fill_manual(values = c("grey90","red")) +
  theme_void() +
  annotate("text", 
           label = str_c(round(df[which(df$group == "highlight"), "y"], 2)*100, "%"),
           # family = "Inter",
           fontface = "bold",
           colour = "red", size = 12, 
           x = -2, y = 0)
  
