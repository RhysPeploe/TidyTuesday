library(tidyverse)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon_df <- tuesdata$pokemon_df

pokemon_attack_plot = function(pokemon_type, col_higlight = "red"){
  type_df = pokemon_df %>% 
    filter(type_1 == pokemon_type)
  
  max_pokemon = type_df[which.max(type_df$attack), "pokemon"]
  
  ggplot(pokemon_df, aes(x = attack, y = 1)) +
    geom_point(pch = 124, size = 5, col = "#1c2c5e")+
    geom_point(data = type_df,
               pch = 124, size = 10, color = col_higlight) +
    labs(y = "",
         x = "") +
    theme(
      axis.text = element_blank(),
      plot.background = element_rect(fill = "#95c1fe"),
      panel.background = element_rect(fill = "#95c1fe"),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      title = element_text(colour = col_higlight)) +
    ylim(c(0.998, 1.002))+
    annotate("text", x = max(type_df$attack) - 18,
             y = 1.0012, label = max_pokemon, col = col_higlight,
             fontface = "bold", size = 8)  +
    geom_image(
      data = slice_head(pokemon_df, n = 1),
      aes(
        x = 25,
        y = 1.0015,
        image = "logo.png"
      ),
      size = 0.4
    ) +
    annotate(
      "curve", x = max(type_df$attack) - 15, xend = max(type_df$attack), 
      y = 1.0011, yend = 1.0002,
      colour = "#ffca08",
      arrow = arrow(
        length = unit(2.5, "mm"), type = "closed"
      ),
      curvature = -0.5
    )+
    annotate("text", x = 60,
             y = 1.0012, label = pokemon_type, col = col_higlight,
             fontface = "bold", size = 8)  
}
pokemon_attack_plot("fire", "#ffca08")

ggpubr::ggarrange(
  pokemon_attack_plot("grass", "darkgreen"),
  pokemon_attack_plot("fire", "red"),
  pokemon_attack_plot("dark", "purple"),
  ncol = 1
)

