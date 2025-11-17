library(tidyverse)
library(worlddatr)
library(cowplot)
library(scales)

tuesdata <- tidytuesdayR::tt_load(2025, week = 45)
who_tb_data <- tuesdata$who_tb_data %>% 
  select(iso3, year, g_whoregion, c_cdr, cfr,
         e_mort_100k, e_inc_100k)

tb_map = function(yearr, metric, max_scale,
                  colour_high = "#14B1E7", colour_low = "#CCECF9"){
  map_data = world_map %>% 
    left_join(who_tb_data %>% 
                filter(year == yearr), by = c("alpha_3_code" = "iso3")) %>% 
    filter(alpha_3_code != "ATA")
  
  colour_text = "#435C6D"
  
  colnames(map_data)[which(colnames(map_data)==metric)] = "metric_col"
  
  ggplot(map_data, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "#FDF3F4", colour = "black", linewidth = 0.01) +
    geom_polygon(data = map_data %>% 
                   filter(!is.na(metric_col),
                          metric_col != 0), mapping = aes(fill = metric_col),
                 colour = "black", alpha = 0.85, linewidth = 0.01)+
    geom_polygon(data = map_data %>% 
                   filter(alpha_3_code == "LSO"), mapping = aes(fill = metric_col),
                 colour = "black", alpha = 0.85, linewidth = 0.01) +
    scale_fill_gradient(high =  colour_high,
                        low = colour_low, 
                        breaks = pretty_breaks(),
                        limits = c(0, max_scale)) +
    labs(fill = metric) +
    theme(panel.background = element_rect(fill = "#FFFFFF"), 
          plot.background = element_rect(fill = "#FFFFFF"), 
          panel.grid = element_blank(),
          plot.title = element_text(size = 12), 
          plot.subtitle = element_text(size = 10), 
          axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          legend.key.height = unit(1, "cm"), 
          legend.background = element_rect(fill = "#FFFFFF"), 
          legend.text = element_text(colour = colour_text), 
          legend.title = element_text(colour = colour_text)) 
}

tb_map(2000, "c_cdr", max_scale = 240)
tb_map(2000, "cfr", max_scale = 1)
tb_map(2012, "cfr", max_scale = 1)

  
plots = plot_grid(
  tb_map(2018, "c_cdr", 200, colour_high = "#129FCE", colour_low = "#CCECF9") +
    theme(legend.position = "none") +
    labs(title = 2018),
  tb_map(2018, "cfr", 1, colour_high = "#5C3348", colour_low = "#E9D8E1")+
    theme(legend.position = "none") +
    labs(title = 2018),
  tb_map(2023,"c_cdr", 200, colour_high = "#129FCE", colour_low = "#CCECF9")+
    theme(legend.position = "none")+
    labs(title = 2023),
  tb_map(2023,"cfr", 1, colour_high = "#5C3348", colour_low = "#E9D8E1")+
    theme(legend.position = "none") +
    labs(title = 2023),
  nrow = 2
)

legend_ccdr = get_legend(tb_map(2023, "c_cdr", 200))
legend_cfr = get_legend(tb_map(2023, "cfr", 1, colour_high = "#5C3348", colour_low = "#E9D8E1"))

plot_grid(legend_ccdr,
          plots,
          legend_cfr, 
          ncol = 3, rel_widths = c(0.1,0.6,0.1))

ggsav"