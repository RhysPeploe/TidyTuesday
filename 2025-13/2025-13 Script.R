library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon_df <- tuesdata$pokemon_df

pokemon_attack_plot = function(pokemon_type, col_higlight = "red"){
  type_df = pokemon_df %>% 
    filter(type_1 == pokemon_type)
  
  max_pokemon = type_df[which.max(type_df$attack), "pokemon"]
  
  ggplot(pokemon_df, aes(x = attack, y = 1)) +
    geom_point(pch = 124, size = 5, col = "gray")+
    geom_point(data = type_df,
               pch = 124, size = 10, color = col_higlight) +
    labs(y = "",
         x = "Attack",
         title = pokemon_type) +
    theme(
      axis.text.y = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          title = element_text(colour = col_higlight)) +
    ylim(c(0.998, 1.002))+
    annotate("text", x = max(type_df$attack) - 5,
             y = 1.002, label = max_pokemon, col = col_higlight)
}
pokemon_attack_plot("grass")

ggpubr::ggarrange(
  pokemon_attack_plot("grass", "darkgreen"),
  pokemon_attack_plot("fire", "red"),
  pokemon_attack_plot("dark", "purple"),
  ncol = 1
)

