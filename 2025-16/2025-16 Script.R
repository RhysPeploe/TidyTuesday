library(tidyverse)
library(ggwaffle)
library(waffle)
library(extrafont)
library(fontawesome)
library(ggpubr)

tuesdata <- tidytuesdayR::tt_load(2025, week = 16)
daily_accidents <- tuesdata$daily_accidents

daily_accidents = daily_accidents %>% 
  mutate(e420 = if_else((month(date) == 4 & day(date) == 20), TRUE, FALSE),
         e710 = if_else((month(date) == 7 & day(date) == 10), TRUE, FALSE),
         day = yday(date),
         year_group = if_else(year(date) >= 1992 & year(date) < 1998, "1992-97",
                              if_else(year(date) >= 1998 & year(date) < 2004, "1998-03",
                              if_else(year(date) >= 2004 & year(date) < 2010, "2004-10",
                              if_else(year(date) >= 2010 & year(date) < 2017, "2010-16", NA))))) %>% 
  relocate(e420, .after = fatalities_count) 

# waffle(((daily_accidents %>% 
#            filter(e420 == TRUE)) %>% 
#           group_by(year_group) %>% 
#           summarise(total_fatalities = sum(fatalities_count)) %>% 
#           ungroup())$total_fatalities/10,use_glyph = "car") # 
# waffle(((daily_accidents %>% 
#            filter(e710 == TRUE)) %>% 
#           group_by(year_group) %>% 
#           summarise(total_fatalities = sum(fatalities_count)) %>% 
#           ungroup())$total_fatalities/10)

iron(
  waffle(((daily_accidents %>% 
             filter(e420 == TRUE)) %>%
            group_by(year_group) %>% 
            summarise(total_fatalities = sum(fatalities_count)) %>% 
            ungroup())$total_fatalities/10,
         pad = 3, size = 2, legend_pos = "none",glyph_size = 2, 
         title = " Fatalities from car crashes in US on 4/20 & 7/10 \n Split into year groups 1992-97, 98-03, 04-10 & 10-16"),
  waffle(((daily_accidents %>% 
             filter(e710 == TRUE)) %>% 
            group_by(year_group) %>% 
            summarise(total_fatalities = sum(fatalities_count)) %>% 
            ungroup())$total_fatalities/10, size =2,glyph_size = 2, legend_pos = "none")
)



  annotate_figure(top = text_grob("Fatalities on 4/20 & 7/10"))





# 
# waffle_iron(daily_accidents %>% filter(e420 == TRUE), aes_d(group = year_group), rows = 20) %>% mutate(day = "420")
# waffle_iron(daily_accidents %>% filter(e710 == TRUE), aes_d(group = year_group), rows = 20) %>% mutate(day = "710")
# 
# bind_rows(waffle_iron(daily_accidents %>% filter(e420 == TRUE), aes_d(group = year_group), rows = 20) %>% mutate(day = "420"),
#           waffle_iron(daily_accidents %>% filter(e710 == TRUE), aes_d(group = year_group), rows = 20) %>% mutate(day = "710"))
# 
# ggplot(bind_rows(waffle_iron(daily_accidents %>% filter(e420 == TRUE) %>% mutate(fatalities_count/10), aes_d(group = year_group), rows = 20) %>% mutate(day = "420"),
#                  waffle_iron(daily_accidents %>% filter(e710 == TRUE) %>% mutate(fatalities_count/10), aes_d(group = year_group), rows = 20) %>% mutate(day = "710")),
#        aes(x = x, y = y, fill = group))+
#   geom_waffle(color = "white") +
#   facet_wrap(~day, ncol=1) +
#   theme_void()
