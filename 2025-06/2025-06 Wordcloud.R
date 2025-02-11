setwd("~/R/TidyTuesday/2025-06")

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 6)

cdc_datasets <- tuesdata$cdc_datasets

tags = cdc_datasets %>% 
  select(tags) %>% 
  filter(tags != "This dataset does not have any tags")
  
str_split(tags$tags, pattern = ",") %>% 
  unlist() %>% 
  trimws() %>% 
  as.data.frame() %>% 
  rename("word" = ".") %>% 
  group_by(word) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  wordcloud2::wordcloud2(
    shape = "star",
    color = rep_len(c(
      "#f0e2c3", 
      "#c9b51a",
      "#6d4e4e", 
      "#838284",
      "#0e1634",
      "#373c46",
      "#3c9234",
      "#444c64",
      "#266b36",
      "#3e3c1c"),
      nrow(tags)),
    size = 1.2
  )


