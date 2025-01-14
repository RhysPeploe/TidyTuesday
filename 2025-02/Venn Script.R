# Libraries & Data
library(tidyverse)
library(stopwords)
library(admiral)
library(VennDiagram)
library(RColorBrewer)

tuesdata <- tidytuesdayR::tt_load('2025-01-14')
conf2023 <- tuesdata$conf2023
conf2024 <- tuesdata$conf2024

#-------------------------------------------------------------------------------
# Extract and clean unique words from the session track
track = unique(conf2024$track) %>% 
  str_replace_all("[^[:alnum:]]", " ") %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.data.frame() %>% 
  convert_blanks_to_na() %>% 
  rename("track_words" = ".") %>% #remove s
  filter(!is.na(track_words)) %>% 
  mutate(track_words = str_to_lower(track_words)) %>% 
  unique()

# Load and clean common stopwords 
stop = stopwords(source = "stopwords-iso") %>%   #stopwords(source = "smart")
  as.data.frame() %>% 
  rename("stopwords" = ".") %>% 
  mutate(stopwords = str_to_lower(stopwords))

# Remove stopwords from unique track words
anti_join(track, stop, by = c("track_words" = "stopwords"))

#-------------------------------------------------------------------------------
# Extract and clean unique words from the talk title
title = unique(conf2024$talk_title) %>% 
  str_replace_all("[^[:alnum:]]", " ") %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.data.frame() %>% 
  convert_blanks_to_na() %>% 
  rename("title_words" = ".") %>% #remove s
  filter(!is.na(title_words)) %>% 
  mutate(title_words = str_to_lower(title_words)) %>% 
  unique()

# Remove stopwords from unique abstract words
anti_join(title, stop, by = c("title_words" = "stopwords"))

#-------------------------------------------------------------------------------
# Extract and clean unique words from the talk abstract
abstract = unique(conf2024$description) %>% 
  str_replace_all("[^[:alnum:]]", " ") %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.data.frame() %>% 
  convert_blanks_to_na() %>% 
  rename("abstract_words" = ".") %>% #remove s
  filter(!is.na(abstract_words)) %>% 
  mutate(abstract_words = str_to_lower(abstract_words))%>% 
  unique()

# Remove stopwords from unique abstract words
anti_join(abstract, stop, by = c("abstract_words" = "stopwords"))

#-------------------------------------------------------------------------------
# Create Venn Diagram
venn.diagram(
  x = list(track$track_words, title$title_words, abstract$abstract_words),
  category.names = c("Track", "Title", "Abstract"),
  filename = "R/TidyTuesday/2025-02/venn.png",
  
  lwd = 2,
  lty = 1,
  col=c("#BA274A", "#FFE74C", "#4D6CFA"),
  fill = c(alpha("#BA274A",0.3), alpha("#FFE74C",0.3), alpha("#4D6CFA",0.5)),
  
  cex = 1.4, cat.cex = 1.2,
  fontface = "bold", cat.fontface = "bold",
  
  main = "Number of Unique Words in the Talk Title, Abstract and Track \n of Posit2024 Presentations, with Stopwords Removed",
  main.cex = 1.1, main.fontface = "bold", height = 3320, width = 3420
  )
