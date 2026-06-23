devtools::install_github("hrbrmstr/streamgraph")

library(tidyverse)
library(htmlwidgets)
library(streamgraph)

tuesdata <- tidytuesdayR::tt_load(2026, week = 25)

# encyclicals <- tuesdata$encyclicals
papal_encyclicals <- tuesdata$papal_encyclicals
# scripture_references <- tuesdata$scripture_references

plot = papal_encyclicals %>% 
  mutate(pope = factor(pope, levels = c(
    "Leo XIII", "Pius X", "Benedict XV", "Pius XI", "Pius XII", "John XXIII",
    "Paul VI", "John Paul II", "Benedict XVI", "Francis", "Leo XIV"
  ))) %>% 
  count(pope, pontificate_year) %>% 
  streamgraph(key = "pope",
              value = "n",
              date = "pontificate_year") %>% 
  sg_fill_manual(c("gray30", # Benedict XV
                   "#191814", # Benedict XVI
                   "#BDBEC0",  # Francis I
                   "#FF4E0E", # John Paul II
                   "#F79400", # John XXIII
                   "#BC306A", # Leo XIII
                   "#B83EA7", # Leo XIV
                   "#C9252C", # Paul VI
                   "#FFCC20", # Pius X
                   "#DCB14F", # Pius XI
                   "#FDEDBD"  # Pius XII
                   )) 


plot

saveWidget(plot, file=paste0(getwd(), "/TidyTuesday/2026-25/streamgraph.html"))
