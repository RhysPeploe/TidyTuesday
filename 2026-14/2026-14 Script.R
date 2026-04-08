tuesdata <- tidytuesdayR::tt_load(2026, week = 14)

repairs <- tuesdata$repairs
repairs_text <- tuesdata$repairs_text

library(worlddatr)
library(tidyverse)
library(janitor)
library(cowplot)

head(repairs)

repairs %>% 
  left_join(world_income %>% 
              select(alpha_3_code, alpha_2_code, country) %>% 
              rename("country_name" = "country"), 
            by = c("country" = "alpha_2_code")) %>% 
  create_map(country_col = "alpha_3_code",
             log_scale = T)

ggsave("TidyTuesday/2026-14/repaired_globe.tif")

repairs %>% 
  mutate(year_repair = year(repair_date)) %>% 
  tabyl(year_repair)

repairs %>% 
  tabyl(kind_of_product)

repairs %>% 
  tabyl(category)

repairs %>% 
  tabyl(brand)

summary(repairs$estimated_year_of_production)

repairs %>% 
  tabyl(repaired, country)


data = repairs %>% 
  mutate(repaired = if_else(repaired == "ja", "yes", repaired)) %>% 
  group_by(country, repaired) %>% 
  summarise(n = n()) %>% 
  pivot_wider(id_cols = country,
              names_from = repaired,
              values_from = n) %>% 
  ungroup() %>% 
  mutate(n = rowSums(.[,-1], na.rm = TRUE),
         prop_yes = yes/n,
         prop_half = half/n,
         prop_yes_neg = 1 - prop_yes,
         prop_half_neg = 1 - prop_yes - prop_half) %>% 
  select(country, prop_yes, prop_yes_neg) %>% 
  pivot_longer(cols = c(prop_yes, prop_yes_neg))


make_donut = function(data, colour, x = 1){
  ggplot(data, aes(x = x, y = value, fill = name)) +
    geom_col(show.legend = FALSE)+
    coord_polar(theta = "y", direction = -1) +
    xlim(c(-2,4)) +
    theme_void() +
    annotate("text", x = -2, y = 0, label = str_c(round(data[1, "value"],2)*100, "%"),
             size = 5, fontface = "bold", colour = colour)+
    geom_text(mapping = aes(x = 3, y = 0, label = country),
             size = 6, fontface = "bold", colour = colour)+
    scale_fill_manual(values = c(colour, "grey90"))
}

make_donut(data[which(data$country == "AT"),], "red")  
make_donut(data[which(data$country == "FI"),], "blue")

plot_grid(
  make_donut(data[which(data$country == "AT"),], "red"),
  make_donut(data[which(data$country == "FI"),], "blue")
)


# plot_grid(
#   rep(NULL, 3),
#   make_donut(data[which(data$country == "AT"),], "red"),
#   NULL,
#   make_donut(data[which(data$country == "AT"),], "orange"),
#   NULL,
#   make_donut(data[which(data$country == "AT"),], "blue"),
#   make_donut(data[which(data$country == "AT"),], "green"),
#   nrow = 3
# )

plot_title = ggdraw()+
  draw_label(
    "Percentage of Repaired Items of Repair Cafes by Country",
    x = 0,
    hjust = 0,
    fontface = "bold",
    size = 16
  ) 

plot_plots = plot_grid(
  ncol = 20, 
  make_donut(data[which(data$country == "CA"),], "#6CD4FF"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "SE"),], "#863B90"),
  NULL,
  make_donut(data[which(data$country == "FI"),], "#863B90"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  #
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "IE"),], "#863B90"),
  make_donut(data[which(data$country == "GB"),], "#863B90"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  #
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "NL"),], "#863B90"),
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  #
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "BE"),], "#863B90"),
  NULL, 
  make_donut(data[which(data$country == "DE"),], "#863B90"),
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  #
  make_donut(data[which(data$country == "US"),], "#6CD4FF"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "FR"),], "#863B90"),
  NULL,
  make_donut(data[which(data$country == "LU"),], "#863B90"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "KR"),], "#FE938C"),
  NULL,
  #
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "CH"),], "#863B90"),
  make_donut(data[which(data$country == "LI"),], "#863B90"),
  make_donut(data[which(data$country == "AT"),], "#863B90"),
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  #
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "ES"),], "#863B90"),
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "IT"),], "#863B90"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "HK"),], "#FE938C"),
  make_donut(data[which(data$country == "TW"),], "#FE938C"),
  NULL,
  #
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "PT"),], "#863B90"),
  NULL,
  NULL,  
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "IL"),], "#863B90"),
  NULL,NULL,NULL,NULL,NULL,NULL,
  #
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  #
  NULL,
  make_donut(data[which(data$country == "GF"),], "#6CD4FF"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "ZA"),], "#4A7C59"),
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  make_donut(data[which(data$country == "AU"),], "#FE938C"),
  make_donut(data[which(data$country == "NZ"),], "#FE938C"),
  #
  nrow = 10
)


plot_grid(
  plot_title,
  plot_plots,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

ggsave("TidyTuesday/2026-14/repaired_by_country.jpeg", width = 28, height = 14)



